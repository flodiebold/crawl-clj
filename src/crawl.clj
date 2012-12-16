(ns crawl
  (:use [lamina.core :exclude (close)])
  (:require [aleph.http.websocket :as ws]
            [cheshire.core :as json]
            [clojure.string :as s]
            [crawl.compression :as compression]
            [crawl.util :as util]
            crawl.lobby
            crawl.game))

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
  [conn]
  (receive-all (filter* (util/match-msg #"ping") (tap (:incoming conn)))
               (fn [ping]
                 (enqueue (:outgoing conn) {:msg "pong"})))
  conn)

(defn- wrap-connection
  [ch]
  (let [incoming-ch (channel* :grounded? true :permanent? true)]
    (siphon ch incoming-ch)
    {:incoming incoming-ch :outgoing ch}))

(defn connect
  "Connects to a DCSS server at the given domain and port.
Returns a map representing the connection."
  [domain port]
  (let [client (ws/websocket-client {:scheme "ws" :server-name domain
                                     :server-port port
                                     :uri "/socket"})]
    (run-pipeline client
                  wrap-decompression
                  wrap-json
                  wrap-connection
                  respond-to-pings
                  crawl.lobby/handle-lobby-info)))

(defn login
  "Logs in at the connection with the credentials (:username and :password).
Returns a result channel containing either the logged-in username, or nil 
in case of failure."
  [connection credentials]
  (let [{username :username password :password} credentials]
    (enqueue (:outgoing connection) {:msg "login"
                                     :username username
                                     :password password})
    (run-pipeline (read-channel (take* 1 (util/get-msgs connection #"login_.*")))
                  (fn [msg]
                    (case (:msg msg)
                      "login_fail" nil
                      "login_success" (:username msg))))))

(defn close
  "Closes the DCSS connection."
  [connection]
  (lamina.core/close (:outgoing connection)))

(defn start-game
  "Starts the game with the given id."
  [connection game-id]
  (enqueue (:outgoing connection) {:msg "play"
                                   :game_id game-id})
  (crawl.game/game-states connection))

(defn watch-player
  [connection username]
  "Watches the player with the given username."
  (enqueue (:outgoing connection) {:msg "watch"
                                   :username username})
  (crawl.game/game-states connection))

(defn- connect-and
  [domain port credentials then & args]
  (run-pipeline
   (connect domain port)
   (fn [conn]
     (run-pipeline
      (if credentials
        (login conn credentials)
        (success-result true))
      (fn [result]
        (when result
          (let [ch (apply then conn args)
                ch* (splice ch (:outgoing conn))]
            (on-closed ch* (fn [] (close conn)))
            ch*)))))))

(defn connect-and-start
  "Connects, logs in and starts a game, returning a channel of game states."
  [domain port credentials game-id]
  (connect-and domain port credentials start-game game-id))

(defn connect-and-watch
  [domain port username]
  (connect-and domain port nil watch-player username))
