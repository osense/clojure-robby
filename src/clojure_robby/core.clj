(ns clojure-robby.core
  (:gen-class))

(defn ** [x n]
  "Raises x to the power of n."
  (reduce * (repeat n x)))


(def map-size 20)
(def tiles [\w \p \g])
(def actions [\u \d \l \r \p])

; DNA functios
(defn rand-dna []
  "Initializes a vector of random DNA."
  (defn rand-gene []
    (nth actions (rand-int (count actions))))
  (vec (repeatedly (** (count tiles) (count actions)) rand-gene)))

(defn cross [a b]
  "Crosses two vectors of DNA."
  (defn choose-rand [a b]
    (if (< (rand-int 100) 50) a b))
  (map choose-rand a b))

; Fitness evaluation functions


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
