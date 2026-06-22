(require '[clojure.string :as str])

(defmacro reduce* [coll init f]
  `(reduce ~f ~init ~coll))

(def clauses [])
(defmacro defclause [name args pred & body]
  `(do
     (defn ~name [& args#]
       (loop [[clause# & rest#] clauses]
         (if (apply (:pred clause#) args#)
           (apply (:body clause#) args#)
           (recur rest#))))

     (let [clause# {:pred (fn ~args ~pred)
                    :body (fn ~args ~@body)}]

       (def clauses (conj clauses clause#)))))


(defn parse-gets [line]
  (let [[value value* goes to bot bot*] (str/split line #" ")]

    (into
     [:gets]
     (map Integer/parseInt [bot* value*]))))


(defn parse-gives [line]
  (let [[bot bot* gives low to dest1 low* and high to dest2 high*]
        (str/split line #" ")]

    [:gives
     (Integer/parseInt bot*)
     (if (= dest1 "bot") :bot :output)
     (Integer/parseInt low*)
     (if (= dest2 "bot") :bot :output)
     (Integer/parseInt high*)]))


(defn parse [line]
  (letfn [(gets? [line]
            (= (first (str/split line #" "))
               "value"))

          (gives? [line]
            (= (first (str/split line #" "))
               "bot"))]

    (cond (gives? line) (parse-gives line)
          (gets? line) (parse-gets line))))


(defn get-instructions [input?]
  (->>
   (or input? (slurp "input"))
   (str/split-lines)
   (mapv parse)))


(defn get-bots [instructions]
  (reduce* instructions
    {}
    (fn [bots instruction]
      (case (first instruction)
        :gets (let [[gets bot value] instruction]
                (->
                 bots
                 (update bot (fnil conj []) value)
                 (update bot sort)))
        :gives bots))))


(defclause execute [[instr & _] bots bins] (= instr :gets)
  {:bots bots :bins bins})


(defclause execute [instruction bots bins] :else
  (let [[gives source bot dest1 bot' dest2] instruction
        [low high] (bots source)]

    (merge
     bots
     {source {}
      dest1 (-> bots (update dest1 (fnil conj []) low) (update dest1 sort))
      dest2 (-> bots (update dest2 (fnil conj []) low) (update dest2 sort))})))


(defn -main [& input?]
  (def instructions (get-instructions input?))

  (def bots (get-bots instructions))

  (reduce* instructions
    {:bots bots :bins {}}
    (fn [{:keys [bots bins]} instruction]
      (execute instruction bots bins))))
