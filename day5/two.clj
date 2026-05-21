(defmacro reduce* [[acc init x coll] & body]
  `(reduce (fn [~acc ~x] ~@body) ~init ~coll))


(defn md5 [s]
  (let [digest (java.security.MessageDigest/getInstance "MD5")
        bytes  (.digest digest (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))


(defn get-door-id [input?]
  input?)


(defn valid? [hash]
  (= (subs hash 0 5) "00000"))


(defn init []
  (vec (repeat 8 nil)))


(defn get-hash [key]
  (md5 key))


(defn -main [& [input?]]
  (let [door-id (get-door-id input?)
        get-key (fn [n] (str door-id (str n)))]

    (->>
     (reduce* [password (init)
               n (range)]

       (if (every? some? password)
         (reduced password)

         (let [key (get-key n)
               hash (get-hash key)]

           (if (valid? hash)
             (update-password password hash)
             password))))

     (apply str))))
