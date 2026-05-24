(require '[clojure.string :as str]
         '[clojure.set :as set])


(defmacro reduce* [init [x coll] & body]
  (let [acc-names (map name (keys init))
        acc-syms  (map symbol acc-names)]
    `(reduce (fn [{:keys [~@acc-syms]} ~x]
               ~@body)
             ~init
             ~coll)))


(defn ip-addresses [input?]
  (->
   (or input? (slurp "input"))
   (str/split-lines)))


(defn get-chunks [ip-address]
  (->
   (reduce* {:current-chunk [] :chunks []}
            [c (str ip-address \!)]

     (cond (= c \[)   {:current-chunk [c]                    :chunks (conj chunks (apply str current-chunk))}
           (= c \])   {:current-chunk []                     :chunks (conj chunks (apply str (conj current-chunk c)))}
           (= c \!)   {:current-chunk []                     :chunks (conj chunks (apply str current-chunk))}
           :else      {:current-chunk (conj current-chunk c) :chunks chunks}))

   :chunks))


(defn get-aba [chunk]
  (letfn
      [(aba? [[a b c]]
         (and
          (= a c)
          (not= a b)))]

      (filter aba?
              (partition 3 1 chunk))))


(def get-bab get-aba)


(defn hypernet? [chunk]
  (and
   (= (first chunk) \[)
   (= (last chunk) \])))


(def supernet? (complement hypernet?))


(defn invert [[a b a]]
  (list b a b))


(defn has? [aba bab]
  (->
   (set/intersection
    (-> (map invert aba) set)
    bab)

   (seq)))


(defn ssl? [ip-address]
  (let [chunks (get-chunks ip-address)

        aba
        (->
         (apply set/union
           (keep #(when (supernet? %) (get-aba %))
                 chunks))
         set)

        bab
        (->
         (apply set/union
           (keep #(when (hypernet? %) (get-bab %))
                 chunks))
         set)]

    (has? aba bab)))


(defn -main [& [input?]]
  (->>
   (ip-addresses input?)
   (filter ssl?)
   (count)))
