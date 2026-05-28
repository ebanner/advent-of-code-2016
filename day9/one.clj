(require '[clojure.string :as str])


(def clauses [])
(defmacro defclause [name args pred body]
  `(do
     (defn ~name ~args
       (loop [[clause# & rest#] clauses]
         (if (apply (:pred clause#) ~args)
           (apply (:body clause#) ~args)
           (recur rest#))))

     (let [clause# {:pred (fn ~args ~pred)
                    :body (fn ~args ~body)}]

       (def clauses (conj clauses clause#)))))


(defmacro reduce* [init [x coll] & body]
  (let [acc-names (map name (keys init))
        acc-syms  (map symbol acc-names)]
    `(reduce (fn [{:keys [~@acc-syms]} ~x]
               ~@body)
             ~init
             ~coll)))


(defn get-file [input?]
  (->
   (or input? (slurp "input"))
   (str/trimr)))


(defn parse-marker [marker]
  [1 1])


(defn parse [file]
  (reduce* {:state :ELSE :chunks [] :chunk [] :ticks -1}
      [c file]

      (letfn
          [(handle-else []
             (if (= c \()
               {:state :IN-MARKER :chunks (conj chunks chunk) :chunk [] :ticks ticks}
               {:state state :chunks chunks :chunk (conj chunk c) :ticks ticks}))

           (handle-in-marker []
             (if (= c \))
               (let [[ticks' _] (parse-marker chunk)]
                 {:state :IN-CHARS :chunks chunks :chunk (conj chunk c) :ticks ticks'})

               {:state state :chunks chunks :chunk (conj chunk c) :ticks (dec ticks)}))

           (handle-in-chars []
             (if (zero? ticks)
               {:state :ELSE :chunks (conj chunks chunk) :chunk [] :ticks -1}
               {:state state :chunks chunks :chunk (conj chunk c) :ticks ticks}))]

        (case state
          :ELSE (handle-else)
          :IN-MARKER (handle-in-marker)
          :IN-CHARS (handle-in-chars)))))


(defclause expand [chunk] (string? chunk)
  chunk)


(defclause expand [[_ rep] s] :else
 (->>
  s
  (repeat rep)
  (apply str)))


(defn decompress [file]
  (->>
   (parse file)
   (map expand)
   (apply str)))


(defn -main [& [input?]]
  (->
   (get-file input?)
   (decompress)
   (count)))
