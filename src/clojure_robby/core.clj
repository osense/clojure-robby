(ns clojure-robby.core
  (:gen-class))

(defn ** [x n]
  "Raises x to the power of n."
  (reduce * (repeat n x)))


(def map-size 20) ; Size of the map used for robot simulation.
(def gold-prob 10) ; Probability in 100 that a tile contains gold.
(def simul-steps 100) ; Steps to allow the robot to make in a simulation.
(def tiles [\w \p \g]) ; Tiles that populate the map.
(def actions [\u \d \l \r \p]) ; Actions the robot can take.


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
(defn rand-map []
  "Initializes a map with walls at the boundaries and randomly positioned gold."
  (def insides (- map-size 2))
  (def wall-row (vec (repeat map-size \w)))
  (defn rand-tile []
    (if (< (rand-int 100) gold-prob) \g \p))
  (defn rand-row []
    (vec (concat [\w] (repeatedly insides rand-tile) [\w])))
  (vec (concat [wall-row] (repeatedly insides rand-row) [wall-row])))

(defn on-index [themap x y]
  (nth (nth themap y) x))

;(defn simulate [dna onmap]
 ; "Simulates the DNA on a map. Evaluates to the score the robot achieved in simul-steps."


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
