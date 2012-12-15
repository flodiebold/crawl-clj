(ns crawl.util
  (:use lamina.core))

(defn match-msg
  "Returns a predicate that matches messages whose type matches re."
  [re]
  (fn [msg] (and msg (re-matches re (:msg msg)))))

(defn get-msgs
  "Returns a channel with all messages from the connection whose type matches re."
  [connection re]
  (let [ch (tap (:incoming connection))]
    (filter* (match-msg re) ch)))
