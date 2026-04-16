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


(defn travel [state [instruction & rest] path]
  (let [path' (conj path (:position state))]

    (cond
      (visited? state path) state
      (char? instruction) (travel (rotate state instruction) rest path')
      (zero? instruction) (travel state rest path')
      :else (travel (march state 1) (cons (dec instruction) rest) path'))))


(defn -main [& args]
  (let [instructions (get-instructions args)
        state (travel {:position [0 0] :direction :N}
                      instructions
                      [])]

    (get-distance state)))


(println (-main))
