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


(defn get-instruction-lines [[input] & _]
  (->
   (or input (slurp "input"))
   (str/split-lines)))


(def PAD
  [[nil nil  1   nil nil]
   [nil 2    3   4   nil]
   [5   6    7   8   9]
   [nil \A  \B  \C   nil]
   [nil nil \D  nil nil]])


(defn valid? [position]
  (->
   (get-in PAD position)
   (some?)))


(defn nudge [[i j] direction]
  (let [position
        (case direction
          \U [(dec i) j]
          \L [i (dec j)] \R [i (inc j)]
          \D [(inc i) j])]

    (if (valid? position)
      position
      [i j])))


(defn move [instructions position]
  (reduce* [position position
            instruction instructions]

    (nudge position instruction)))


(defn -main [& args]
  (def instruction-lines (get-instruction-lines args))

  (->>
   (reduce* {:bathroom-code [] :position [2 0]}
     [instructions instruction-lines]

     (let [position' (move instructions position)
           digit (get-in PAD position')]

       {:bathroom-code (conj bathroom-code digit)
        :position position'}))

   (:bathroom-code)
   (apply str)))


(println (-main))
