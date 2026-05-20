(defn md5 [s]
  (let [digest (java.security.MessageDigest/getInstance "MD5")
        bytes  (.digest digest (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))


(defn get-door-id [input?]
  input?)


(defn -main [& [input?]]
  (let [door-id (get-door-id input?)]

    (->>
     (take 8
       (for [n (range)
             :let [hash (md5 (str door-id (str n)))]
             :when (= (subs hash 0 5) "00000")]

         (nth hash 5)))

     (apply str))))
