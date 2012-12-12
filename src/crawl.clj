(ns crawl
  (:use [lamina.core :exclude (close)])
  (:require [aleph.http.websocket :as ws]
            [cheshire.core :as json]
            [clojure.string :as s]
            [crawl.compression :as compression]
            [crawl.util :as util]
            crawl.lobby))

(defn- wrap-decompression
  [ch]
  (let [inflater (compression/make-inflater)]
    (splice
     (map* (partial compression/inflate-to-string inflater) ch)
     ch)))

(defn- wrap-json
  [ch]
  (let [ch* (channel)
        keyword-munger (fn [k] (keyword (s/replace k "_" "-")))]
    (join (map* json/generate-string ch*) ch)
    (splice (map* #(json/parse-string % keyword-munger) ch) ch*)))

(defn- respond-to-pings
  [ch]
  (util/handle-messages ch (util/match-msg #"ping")
                        (sink (fn [ping]
                                (enqueue ch {:msg "pong"})))))

(defn- print-msgs
  [name ch]
  (let [ch* (channel)]
    (join ch (split (sink (partial println name)) ch*))
    (splice ch* ch)))

(defn- wrap-connection
  [ch]
  {:channel ch})

(defn- separate-login-messages
  [conn]
  (let [login-ch (channel)]
    (assoc conn
      :channel (util/handle-messages (:channel conn) (util/match-msg #"login_.*")
                                     login-ch)
      ::logins login-ch)))

(defn connect
  [domain port]
  (let [client (ws/websocket-client {:scheme "ws" :server-name domain
                                     :server-port port
                                     :uri "/socket"})]
    (run-pipeline client
                  wrap-decompression
                  wrap-json
                  respond-to-pings
                  wrap-connection
                  crawl.lobby/handle-lobby-info
                  separate-login-messages)))

(defn login
  [connection credentials]
  (let [{username :username password :password} credentials]
    (enqueue (:channel connection) {:msg "login"
                                    :username username
                                    :password password})
    (run-pipeline (read-channel (::logins connection))
                  (fn [msg]
                    (case (:msg msg)
                      "login_fail" nil
                      "login_success" (:username msg))))))

(defn close
  [connection]
  (lamina.core/close (:channel connection)))
