(require '[clojure.string :as str])


(defmacro reduce* [first-arg & rest-args]
  (if (map? first-arg)
    (let [[binding & body] rest-args
          [x coll] binding
          acc-syms (mapv (comp symbol name) (keys first-arg))]

      `(reduce (fn [{:keys [~@acc-syms]} ~x]
                 ~@body)
               ~first-arg
               ~coll))

    (let [[acc init x coll] first-arg
          body              rest-args]
      `(reduce (fn [~acc ~x] ~@body) ~init ~coll))))


(defn get-instruction-lines [input & _]
  (->
   (or input (slurp "input"))
   (str/split-lines)))


(def PAD [[1 2 3]
          [4 5 6]
          [7 8 9]])


(defn clamp [n]
  (->
   n
   (max 0)
   (min 2)))


(defn nudge [[i j] direction]
  (->>
   (case direction
     \U [(dec i) j]
     \L [i (dec j)] \R [i (inc j)]
     \D [(inc i) j])

   (map clamp)))


(defn move [instructions position]
  (reduce* [position position
            instruction instructions]

    (nudge position instruction)))


(defn -main [& args]
  (def instruction-lines (get-instruction-lines args))

  (->>
   (reduce* {:bathroom-code 0 :position [1 1]}
     [instructions instruction-lines]

     (let [position' (move instructions position)
           digit (get-in PAD position')]

       {:bathroom-code (+ (<< bathroom-code) digit)
        :position position'}))

   (:bathroom-code)))

(println (-main))
