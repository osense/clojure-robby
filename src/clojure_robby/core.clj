(ns clojure-robby.core
  (:gen-class))

(defn ** [x n]
  "Raises x to the power of n."
  (reduce * (repeat n x)))


(def map-size 20) ; Size of the map used for robot simulation.
(def gold-prob 10) ; Probability in 100 that a tile contains gold.
(def simul-steps 100) ; Steps to allow the robot to make in a simulation.
(def tiles [\w \p \g]) ; Tiles that populate the map: wall, path, and gold.
(def actions [\u \d \l \r \p]) ; Actions the robot can take: up, down, left, right, pick up.


; DNA functions
(defn rand-dna []
  "Initializes a vector of random DNA."
  (defn rand-gene []
    (nth actions (rand-int (count actions))))
  (vec (repeatedly (** (count tiles) (count actions)) rand-gene)))

(defn cross [a b]
  "Crosses two vectors of DNA."
  (defn choose-rand [x y]
    (if (< (rand-int 100) 50) x y))
  (map choose-rand a b))


; Fitness evaluation functions
(defn rand-map []
  "Initializes a map with walls at the boundaries and randomly positioned gold."
  (defn gen-tile [x y]
    (def last-coord(- map-size 1))
    (if (or (= x 0) (= y 0) (= x last-coord) (= y last-coord))
      \w
      (if (< (rand-int 100) gold-prob) \g \p)))
  (map (fn [y] (map (fn [x] (gen-tile x y)) (range map-size))) (range map-size)))

(defn on-index [themap x y]
  "Given a map and coordinates, evaluates to what is on the map at the position."
  (nth (nth themap y) x))

;(defn simulate [dna onmap]
 ; "Simulates the DNA on a map. Evaluates to the score the robot achieved in simul-steps."


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
