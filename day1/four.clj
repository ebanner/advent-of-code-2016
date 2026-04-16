(require '[clojure.string :as str])


(defmacro defclause [name args pred & body]
  (let [clauses-sym (symbol (str name "-clauses"))
        clause-key  (list 'quote pred)]
    `(do
       (defonce ~clauses-sym (atom {}))
       (def ~name
         (fn [& args#]
           (loop [[[_k# c#] & rest#] (seq @~clauses-sym)]
             (when-not c#
               (throw (ex-info "No matching clause" {:args args#})))
             (if (apply (:pred c#) args#)
               (apply (:body c#) args#)
               (recur rest#)))))
       (swap! ~clauses-sym assoc
              ~clause-key
              {:pred (fn ~(vec args) ~pred)
               :body (fn ~(vec args) ~@body)}))))


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


(defclause travel [state _ path] (visited? state path)
  state)

(defclause travel [state [instruction & rest] path] true
  (let [path' (conj path (:position state))]

    (cond
      (char? instruction) (travel (rotate state instruction) rest path')
      (zero? instruction) (travel state rest path')
      :else (travel (march state 1) (cons (dec instruction) rest) path'))))


(defn -main [& args]
  (def instructions (get-instructions args))

  (def state (travel {:position [0 0] :direction :N}
                     instructions
                     []))

  (get-distance state))


;; (println (-main))
