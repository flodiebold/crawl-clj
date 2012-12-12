(ns crawl.lobby
  (:use lamina.core)
  (:require [crawl.util :as util]))

(defn- handle-lobby-msg
  [lobby-info msg]
  (case (:msg msg)
    "lobby_clear" {:unfinished true}
    "lobby_complete" (dissoc lobby-info :unfinished)
    "lobby_remove" (dissoc lobby-info (:id msg))
    "lobby_entry"
    (let [id (:id msg)
          data (dissoc msg :msg)
          old-data (get lobby-info id {})]
      (assoc lobby-info id (merge old-data data)))))

(defn handle-lobby-info
  [conn]
  (let [ch (:channel conn)
        lobby-ch (channel)]
    {:channel (util/handle-messages ch (util/match-msg #"lobby_.*") lobby-ch)
     :lobby (->> lobby-ch
                 (reductions* handle-lobby-msg {})
                 (filter* (complement :unfinished))
                 (atom-sink {}))}))
