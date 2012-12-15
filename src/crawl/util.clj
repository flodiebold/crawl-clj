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

(defn flexible-merge
  [handler-map m1 m2]
  (letfn [(default-handler [_ v] v)
          (merge-entry [m e]
            (let [k (key e) v (val e)
                  handler (get handler-map k default-handler)]
              (if handler
                (assoc m k (handler (get m k) v))
                m)))]
    (reduce merge-entry m1 m2)))

(defn parse-int
  [s]
  (Long/parseLong s))
