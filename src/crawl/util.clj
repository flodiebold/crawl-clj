(ns crawl.util
  (:use lamina.core))

(defn handle-messages
  "Removes messages satisfying pred from ch and joins them into msg-ch."
  [ch pred msg-ch]
  (let [ch* (channel)]
    (join ch (split (join->> (filter* pred) msg-ch)
                    ch*))
    (splice (remove* pred ch*) ch)))

(defn match-msg
  "Returns a predicate that matches messages whose type matches re."
  [re]
  (fn [msg] (re-matches re (:msg msg))))
