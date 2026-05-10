(require '[clojure.string :as str])


(defn parse [line]
  (let [[head tail] (str/split line #"\[")
        toks (str/split head #"-")
        [[encrypted-name sector-id] checksum]
        [[(butlast toks) (last toks)] (subs tail 0 (dec (count tail)))]]

    [(str/join "-" encrypted-name) (Integer/parseInt sector-id) checksum]))


(defn get-room-descriptions [input?]
  (->
   (or input? (slurp "input"))
   (str/split-lines)
   (->> (map parse))))


(def OFFSET (int \a))

(defn decrypt-name [[encrypted-name sector-id _]]
  (letfn [(rotate [c]
            (->
             (int c)
             (- OFFSET)
             (+ sector-id)
             (mod 26)
             (+ OFFSET)
             (char)))

          (rotate* [c]
            (if (= c \-) \space (rotate c)))]

    (->>
     encrypted-name
     (map rotate*)
     (apply str))))


(defn -main [& [input?]]
  (let [room-descriptions (get-room-descriptions input?)
        decrypted-names (map decrypt-name room-descriptions)]

    (map vector room-descriptions decrypted-names)))
