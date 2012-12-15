(ns crawl.game
  (:use lamina.core)
  (:require [clojure.string :as s]
            [crawl.enums :as e]
            [crawl.util :as u]))

(def blank-game-state
  {:player {:name ""
            :god "" :piety-rank 0 :penance false
            :hp 0 :hp-max 0 :real-hp-max 0
            :mp 0 :mp-max 0
            :ac 0 :ev 0 :sh 0
            :xl 0 :xl-progress 0
            :time 0 :gold 0
            :place "" :depth 0
            :str 0 :int 0 :dex 0
            :str-max 0 :int-max 0 :dex-max 0
            :status []
            :inv (vec (repeat 52 nil)) :equip {}
            :pos {:x 0 :y 0}}
   :input-mode :normal
   :messages []})

(defmulti transform-game (fn [game-state msg] (-> (:msg msg)
                                                 (s/replace "_" "-")
                                                 keyword)))

(defmethod transform-game :version
  [game-state msg]
  (assoc game-state :version (:text msg)))

(defmethod transform-game :input-mode
  [game-state msg]
  (assoc game-state :input-mode (get e/input-modes (:mode msg))))

(defmethod transform-game :ui-state
  [game-state msg]
  ; ignore for now
  game-state)

(defn- update-equip
  [eq data]
  (reduce (fn [eq [k v]]
            (assoc eq (e/equip (u/parse-int (name k))) v))
          eq data))

(defn- update-item
  [item data]
  (u/flexible-merge
   {:tiles nil}
   item data))

(defn- update-inv
  [inv data]
  (reduce (fn [inv [k v]]
            (update-in inv [(u/parse-int (name k))]
                       update-item v))
          inv data))

(defmethod transform-game :player
  [game-state msg]
  (update-in game-state [:player]
             #(u/flexible-merge
               {:msg nil
                :equip update-equip
                :inv update-inv}
               % msg)))

(defmethod transform-game :msgs
  [game-state msg]
  (update-in
   game-state [:messages]
   (fn [msgs]
     (let [rollback (min (+ (get msg :rollback 0)
                            (get msg :old-msgs 0))
                         (count msgs))]
       (-> msgs
           (subvec 0 (- (count msgs) rollback))
           (into (:messages msg)))))))

(defmethod transform-game :default
  [game-state msg]
  (println "WARNING: Unhandled message " (:msg msg))
  game-state)

(def discard-messages
  #"(?x) ping|game_client|lobby_.*|
         html|set_game_links")

(defn- handle-msg
  [game-state msg]
  (when msg
    (if (re-matches discard-messages (:msg msg))
      game-state
      (transform-game game-state msg))))

(defn game-states
  [conn]
  (take-while* (complement nil?) (reductions* handle-msg blank-game-state
                                              (tap (:incoming conn)))))

