(require '[clojure.string :as str])


(defn ip-addresses [input?]
  (->
   (or input? (slurp "input"))
   (str/split-lines)))


(defmacro reduce* [init [x coll] & body]
  (let [acc-names (map name (keys init))
        acc-syms  (map symbol acc-names)]
    `(reduce (fn [{:keys [~@acc-syms]} ~x]
               ~@body)
             ~init
             ~coll)))


(defn get-chunks [ip-address]
  (->
   (reduce* {:current-chunk [] :chunks []}
            [c (str ip-address \!)]

     (cond (= c \[)   {:current-chunk [c]                    :chunks (conj chunks (apply str current-chunk))}
           (= c \])   {:current-chunk []                     :chunks (conj chunks (apply str (conj current-chunk c)))}
           (= c \!)   {:current-chunk []                     :chunks (conj chunks (apply str current-chunk))}
           :else      {:current-chunk (conj current-chunk c) :chunks chunks}))

   :chunks))


(defn abba? [chunk]
  (letfn
      [(abba-seq? [[a b c d]]
         (and
          (= a d)
          (= b c)
          (not= a b)))]

      (some abba-seq?
            (map vector chunk (rest chunk) (drop 2 chunk) (drop 3 chunk)))))


(defn hypernet? [chunk]
  (and
   (= (first chunk) \[)
   (= (last chunk) \])))


(defn → [f g]
  (some-fn (complement f) g))


(defn tls? [ip-address]
  (let [chunks (get-chunks ip-address)]

    (and
     (some abba? chunks)
     (every? (→ hypernet? (complement abba?)) chunks))))


(defn -main [& [input?]]
  (->>
   (ip-addresses input?)
   (filter tls?)
   (count)))
