(require '[clojure.string :as str])


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


(defmacro reduce* [init [x coll] & body]
  (let [acc-names (map name (keys init))
        acc-syms  (map symbol acc-names)
        has-result? (= (first body) :result)
        result-expr (when has-result? (second body))
        body-forms  (if has-result? (drop 2 body) body)]
    (if has-result?
      `(let [{:keys [~@acc-syms]}
             (reduce (fn [{:keys [~@acc-syms]} ~x]
                       ~@body-forms)
                     ~init
                     ~coll)]
         ~result-expr)
      `(reduce (fn [{:keys [~@acc-syms]} ~x]
                 ~@body-forms)
               ~init
               ~coll))))


(defn get-file [input?]
  (->
   (or input? (slurp "input"))
   (str/trimr)))


(defn parse-marker [marker]
  (let [toks (str/split marker #"x")]
    (map Integer/parseInt toks)))


(defn parse-chunk [[first & rest]]
  (if (number? first)
    [first (apply str rest)]
    (apply str first rest)))


(defn parse [file]
  (reduce* {:state :ELSE :chunks [] :chunk [] :ticks -1}
           [c file]
           :result (if chunk (conj chunks (parse-chunk chunk)) chunks)

    (letfn
        [(handle-else []
           (if (= c \()
             {:state :IN-MARKER :chunks (conj chunks (apply str chunk)) :chunk [] :ticks ticks}
             {:state state :chunks chunks :chunk (conj chunk c) :ticks ticks}))

         (handle-in-marker []
           (if (= c \))
             (let [[ticks' rep] (parse-marker (apply str chunk))]
               {:state :IN-CHARS :chunks chunks :chunk [rep] :ticks ticks'})
             {:state state :chunks chunks :chunk (conj chunk c) :ticks (dec ticks)}))

         (handle-in-chars []
           (if (zero? ticks)
             {:state :ELSE :chunks (conj chunks (parse-chunk chunk)) :chunk [c] :ticks -1}
             {:state state :chunks chunks :chunk (conj chunk c) :ticks (dec ticks)}))]

      (case state
        :ELSE (handle-else)
        :IN-MARKER (handle-in-marker)
        :IN-CHARS (handle-in-chars)))))


(defclause expand [chunk] (string? chunk)
  chunk)


(defclause expand [[rep s]] :else
  (->>
   s
   (repeat rep)
   (apply str)))


(defn decompress [file]
  (let [parsed (parse file)
        expanded (map expand parsed)]

    (apply str expanded)))


(defn -main [& [input?]]
  (let [file (get-file input?)
        decompressed (decompress file)]

    (count decompressed)))
