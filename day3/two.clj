(defn parse [line]
  (->
   line
   (clojure.string/split #"\s+")
   (->> (mapv read-string))))


(defn get-triangles [[input] & _]
  (let [grid (->>
              (or input (slurp "input"))
              (clojure.string/split-lines)
              (map clojure.string/trim)
              (mapv parse))
        [N M] [(count grid) (count (first grid))]]

    (for [j (range M) i (range 0 N 3)]
      [(get-in grid [i j])
       (get-in grid [(inc i) j])
       (get-in grid [(inc (inc i)) j])])))


(defn valid? [[a b c]]
  (and
   (> (+ a b) c)
   (> (+ a c) b)
   (> (+ b c) a)))


(defn -main [& args]
  (->>
   (get-triangles args)
   (filter valid?)
   (count)))


(println (-main))
