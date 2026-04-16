(require '[clojure.string :as str])


(defn parse [instruction-str]
  (let [[turn distance-str]
        [(first instruction-str) (subs instruction-str 1)]]

    (list turn
          (parse-long distance-str))))


(defn get-instructions [args]
  (->
   (or (first args) (slurp "input"))
   (str/split #",")
   (->> (map str/trim))
   (->> (map parse))
   (->> (apply concat))))


(defn march [{[x y] :position direction :direction} distance]
  {:position (case direction
               :N [x (+ y distance)]
               :E [(+ x distance) y]
               :S [x (- y distance)]
               :W [(- x distance) y])
   :direction direction})


(defn rotate [{:keys [position direction]} turn]
  (let [direction'
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
               :E :N))]

    {:position position :direction direction'}))


(defn get-distance [{[x y] :position}]
  (+ (Math/abs x) (Math/abs y)))


(defn visited? [state visited]
  (and (some #{(:position state)} visited)
       (not= (peek visited) (:position state))))


(defn -main [& args]
  (def instructions (get-instructions args))

  (->

   (loop [state {:position [0 0] :direction :N}
          instructions instructions
          path []]

     (let [path' (conj path (:position state))]

       (cond
         (visited? state path) state
         (char? (first instructions)) (recur (rotate state (first instructions)) (rest instructions) path')
         (zero? (first instructions)) (recur state (rest instructions) path')
         :else (recur (march state 1) (cons (dec (first instructions)) (rest instructions)) path'))))

   (get-distance)))


(println (-main))
