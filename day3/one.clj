(defn parse [line]
  (->
   line
   (clojure.string/split #"\s+")
   (->> (map read-string))))


(defn get-triangles [[input] & _]
  (->>
   (or input (slurp "input"))
   (clojure.string/split-lines)
   (map clojure.string/trim)
   (map parse)))


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

