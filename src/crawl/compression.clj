(ns crawl.compression
  (:import java.util.zip.Inflater
           java.io.ByteArrayOutputStream
           org.jboss.netty.buffer.HeapChannelBuffer))

(defn make-inflater
  []
  (Inflater. true))

(let [dbuf-size 1024
      dbuf (byte-array dbuf-size)]
  (defn inflate-to-string
    [^Inflater inflater ^HeapChannelBuffer buf]
    (let [msg-len (.readableBytes buf)
          msg-bytes (byte-array (+ msg-len 4))
          os (ByteArrayOutputStream.)]
      (.readBytes buf msg-bytes 0 msg-len)
;      (println (into [] msg-bytes))
      (doseq [i (range 4)]
        (aset msg-bytes (+ i msg-len) (if (== 0 (quot i 2)) (byte 0) (byte -1))))
      (.setInput inflater msg-bytes)
      (while (not (.needsInput inflater))
        (let [decomp-count (.inflate inflater dbuf 0 dbuf-size)]
          (.write os dbuf 0 decomp-count)))
      (.toString os "UTF-8"))))
