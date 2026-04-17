(require '[clojure.string :as str])


(defmacro reduce* [[acc init x coll] & body]
  `(reduce (fn [~acc ~x] ~@body) ~init ~coll))


(defn get-instructions [args]
  (->
   (or (first args) (slurp "input"))
   (str/split #",")
   (->> (map str/trim))
   (->> (map parse))))


(defn parse [instruction-str]
  (let [[turn distance-str]
        [(first instruction-str) (subs instruction-str 1)]]

    (list turn
          (parse-long distance-str))))


(defn march [[x y] distance direction]
  {:position (case direction
               :N [x (+ y distance)]
               :E [(+ x distance) y]
               :S [x (- y distance)]
               :W [(- x distance) y])
   :direction direction})


(defn rotate [direction turn]
  (case turn
    \R (case direction
         :N :E
         :E :S
         :S :W
         :W :N)
    \L (case direction
         :N :W
         :W :S
         :S :E
         :E :N)))


(defn move [[turn distance] {:keys [position direction]}]
  (let [direction' (rotate direction turn)]

    (march position distance direction')))


(defn distance [{[x y] :position}]
  (+ (Math/abs x) (Math/abs y)))


(defn -main [& args]
  (->
   (reduce* [state {:position [0 0] :direction :N}
             instruction (get-instructions args)]

     (move instruction state))

   (distance)))


(println (-main))
