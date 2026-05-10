(require '[clojure.string :as str])


(defmacro reduce* [[acc init x coll] & body]
  `(reduce (fn [~acc ~x] ~@body) ~init ~coll))


(defn parse [line]
  (let [[head tail] (str/split line #"\[")
        toks (str/split head #"-")
        [[encrypted-name sector-id] checksum]
        [[(butlast toks) (last toks)] (subs tail 0 (dec (count tail)))]]

    [(str/join encrypted-name) (Integer/parseInt sector-id) checksum]))


(defn room-descriptions [input]
  (->
   (or input (slurp "input"))
   (str/split-lines)
   (->> (map parse))))


(defn get-counts [s]
  (reduce* [counts {}
            c s]
    (update counts c (fnil inc 0))))


(defn checks? [sorted-counts checksum]
  (every? identity
    (for [[[c _] d]
          (map vector sorted-counts checksum)]
      (= c d))))


(defn real? [[encrypted-name _ checksum]]
  (let [counts (get-counts encrypted-name)
        sorted-counts
        (sort-by (juxt (comp - val) key) counts)]

    (checks? sorted-counts checksum)))


(def get-sector-id second)

(defn -main [& [args]]
  (->>
   (room-descriptions args)
   (filter real?)
   (map get-sector-id)
   (reduce +)))
