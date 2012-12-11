(ns crawl
  (:use lamina.core)
  (:require [aleph.http.websocket :as ws]
            [crawl.compression :as compression]
            [cheshire.core :as json]))

(defn- wrap-decompression
  [ch]
  (let [inflater (compression/make-inflater)]
    (splice
     (map* (partial compression/inflate-to-string inflater) ch)
     ch)))

(defn- wrap-json
  [ch]
  (let [ch* (channel)]
    (join (map* json/generate-string ch*) ch)
    (splice (map* #(json/parse-string % true) ch) ch*)))

(defn- respond-to-pings
  [ch]
  (let [is-ping #(= "ping" (:msg %))
        ch* (channel)]
    (join ch (split (sink->> (filter* is-ping)
                             (fn [ping]
                               (println "pong" ping)
                               (enqueue ch {:msg "pong"})))
                    ch*))
    (splice (remove* is-ping ch*) ch)))

(defn- print-msgs
  [name ch]
  (let [ch* (channel)]
    (join ch (split (sink (partial println name)) ch*))
    (splice ch* ch)))

(defn connect
  [domain port]
  (let [client (ws/websocket-client {:scheme "ws" :server-name domain
                                     :server-port port
                                     :uri "/socket"})]
    (run-pipeline client
                  wrap-decompression
                  wrap-json
                  (partial print-msgs "post")
                  respond-to-pings)))
