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
   (->> (map parse))))


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
  (let [instructions (get-instructions args)
        state (atom {:position [0 0] :direction :N})]

    (doseq [instruction instructions]
      (swap! state #(move instruction %)))

    (distance @state)))


(println (-main))
