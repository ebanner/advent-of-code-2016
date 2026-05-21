(require '[clojure.string :as str])


(defmacro reduce* [[acc init x coll] & body]
  `(reduce (fn [~acc ~x] ~@body) ~init ~coll))


(defn get-messages [input?]
  (->>
   (str/split-lines (or input? (slurp "input")))
   (mapv vec)))


(defn max-char [counts]
  (->
   (apply min-key val counts)
   (key)))


(defn get-counts [chars]
  (reduce* [counts {}
            c chars]
    (update counts c (fnil inc 0))))


(defn -main [& [input?]]
  (let [messages (get-messages input?)
        N (count messages)
        M (count (first messages))
        column (fn [j]
                 (for [i (range N)] (get-in messages [i j])))]

    (->>
     (range M)
     (map column)
     (map get-counts)
     (map max-char)
     (apply str))))
