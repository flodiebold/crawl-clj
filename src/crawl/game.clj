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
   :map {}
   :monsters {}
   :input-mode :normal
   :messages []})

(def blank-monster
  {:name ""
   :type 0
   :att :hostile
   :btype 0
   :threat 0})

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
            (assoc eq (e/equip (u/parse-int (name k)))
                   (if (neg? v) nil v)))
          eq data))

(defn- update-item
  [item data]
  (u/flexible-merge
   {:tiles nil}
   item data))

(defn- update-inv
  [inv data]
  (reduce (fn [inv [k v]]
            (let [slot (u/parse-int (name k))]
              (update-in inv [slot]
                         update-item (assoc v :slot slot))))
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

(defn- update-monster
  [mon data monsters]
  (let [base-mon (if (contains? data :id)
                   (get monsters (:id data))
                   (or mon blank-monster))
        keymap {:id :id
                :name :name
                :type :type
                :att :attitude
                :btype :base-type
                :threat :threat}
        handlers {:attitude (fn [_ a] (e/attitude a))
                  nil nil}]
    (u/flexible-merge handlers base-mon
                      (u/map-keys keymap data))))

(defn- extract-tile-flags
  [t]
  (when (contains? t :bg)
    (let [bg (:bg t)]
      (into {} (for [[k v] e/tile-flags]
                 [k (not= 0 (bit-and bg v))])))))

(defn- update-map-cell
  [cell data monsters]
  (let [keymap {:f :feat
                :g :glyph
                :col :colour
                :mon :monster}
        handlers {:feat (fn [_ f] (e/dungeon-features f))
                  :monster (fn [old new]
                             (update-monster old new monsters))
                  nil nil}
        flags (extract-tile-flags (:t data))
        cell2 (u/flexible-merge
               handlers cell
               (merge (u/map-keys keymap data) flags))]
    [cell2
     (if (get-in cell2 [:monster :id])
       (let [m (:monster cell2)]
         (assoc monsters (:id m) m))
       monsters)]))

(defn- update-map
  [[m [last-x last-y] monsters] data]
  (let [x (or (:x data) (inc last-x))
        y (or (:y data) last-y)
        [new-cell new-monsters] (update-map-cell (get-in m [x y])
                                                 data monsters)]
    [(assoc-in m [x y] new-cell) [x y] new-monsters]))

(defmethod transform-game :map
  [game-state msg]
  (let [m (if (:clear msg)
            {}
            (:map game-state))
        mo (:monsters game-state)
        [m2 _ mo2] (reduce update-map [m nil mo] (:cells msg))]
    (assoc game-state :map m2 :monsters mo2)))

(defmethod transform-game :default
  [game-state msg]
  (println "WARNING: Unhandled message " (:msg msg))
  game-state)

(def discard-messages
  #"(?x) ping|game_client|lobby_.*|
         html|set_game_links|game_started|watching_started|flash|
         cursor|txt|
         init_menus|menu|close_menu|
         update_spectators|chat")

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

