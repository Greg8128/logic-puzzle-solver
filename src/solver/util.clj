(ns solver.util)

(defn if-filled*
  [f]
  (try (f) (catch Exception e true )))

(defmacro if-filled
  "handle NullPointerExceptions by returning True"
  [& forms]
  `(if-filled* #(~@forms)))

;; credit: Stuart Sierra https://stackoverflow.com/questions/3407876
(defn unchunk
  "prevent clojure from chunking a lazy sequence"
  [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (unchunk (next s))))))
