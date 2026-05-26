(require '[clojure.string :as str])


(defmacro reduce* [[acc init x coll] & body]
  `(reduce (fn [~acc ~x] ~@body) ~init ~coll))

(defmacro map* [[f & args] coll]
  `(map (partial ~f ~@args) ~coll))

(defmacro filter* [[f & args] coll]
  `(filter (partial ~f ~@args) ~coll))


(defn parse-rect [[_ dim-str]]
  (->>
   (str/split dim-str #"x")
   (map Integer/parseInt)
   (cons :rect)
   (vec)))


(defn get-x [x-str]
  (let [[_ x] (str/split x-str #"=")]
    (Integer/parseInt x)))


(defn parse-column [toks]
  (let [col (get-x (nth toks 2))
        height (-> (last toks) Integer/parseInt)]

    [:rotate-column col height]))


(defn get-y [y-str]
  (let [[_ y] (str/split y-str #"=")]
    (Integer/parseInt y)))


(defn parse-row [toks]
  (let [row (get-y (nth toks 2))
        width (-> (last toks) Integer/parseInt)]

    [:rotate-row row width]))


(defn parse-rotate [toks]
  (case (second toks)
    "column" (parse-column toks)
    "row" (parse-row toks)))


(defn parse [line]
  (let [toks (str/split line #" ")]

    (case (first toks)
      "rect" (parse-rect toks)
      "rotate" (parse-rotate toks))))


(defn get-instructions [input?]
  (->>
   (or input? (slurp "input"))
   (str/split-lines)
   (map parse)))


(defn draw-rect [[width height] grid]
  (let [idxs
        (for [i (range height) j (range width)] [i j])]

    (reduce* [grid grid
              [i j] idxs]

      (assoc-in grid [i j] \#))))


(defn rotate-row [[i dist] grid]
  (let [M (-> (first grid) count)
        row (nth grid i)
        rotate (fn [j] (-> (+ j dist) (mod M)))

        idxs (for [[i x] (map-indexed vector row)
                   :when (= x \#)]
               i)

        rotated-idxs (map rotate idxs)]

    (reduce* [grid grid
              j (range M)]

      (assoc-in grid [i j]
                (if (some #{j} rotated-idxs)
                  \#
                  \.)))))


(defn get-col [grid j]
  (let [N (count grid)]

    (->>
     (for [i (range N)] [i j])
     (map* (get-in grid)))))


(defn rotate-col [[j dist] grid]
  (let [N (count grid)
        col (get-col grid j)
        rotate (fn [i] (-> (+ i dist) (mod N)))

        idxs (for [[j p] (map-indexed vector col)
                   :when (= p \#)]
               j)

        rotated-idxs (map rotate idxs)]

    (reduce* [grid grid
              i (range N)]

      (assoc-in grid [i j]
                (if (some #{i} rotated-idxs)
                  \#
                  \.)))))


(defn execute [[name & args] grid]
  (case name
    :rect (draw-rect args grid)
    :rotate-column (rotate-col args grid)
    :rotate-row (rotate-row args grid)))


(defn get-grid []
  (->>
   (for [_ (range 6)] (repeat 50 \.))
   (mapv vec)))


(defn count-pixels [grid]
  (let [N (count grid)
        M (-> (first grid) count)]

    (->>
     (for [i (range N) j (range M)] [i j])
     (map* (get-in grid))
     (filter* (contains? #{\#}))
     (count))))


(defn -main [& [input?]]
  (->
   (reduce* [grid (get-grid)
             instruction (get-instructions input?)]

     (execute instruction grid))

   (count-pixels)))
