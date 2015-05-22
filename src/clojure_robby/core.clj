(ns clojure-robby.core
  (:gen-class))

(defn ** [x n]
  "Raises x to the power of n."
  (reduce * (repeat n x)))


(def map-size 20) ; Size of the map used for robot simulation.
(def gold-prob 20) ; Probability in 100 that a tile contains gold.
(def simul-steps 100) ; Steps to allow the robot to make in a simulation.
(def tiles [\w \p \g]) ; Tiles that populate the map: wall, path, and gold.
(def actions [\u \d \l \r \x \p]) ; Actions the robot can take: up, down, left, right, pick up.
(def dir-vects ['(0 -1) '(0 1) '(-1 0) '(1 0) '(0 0)]) ; Direction vectors, corresponding to the actions.

(def mutation-prob 10) ; Probability in 100 that a gene will mutate.
(def action-penalty 5) ; How many points robot loses when hitting the wall or picking up when it shouldn't.
(def gold-value 10) ; How many points picking up gold is worth.


; DNA functions
(defn rand-dna []
  "Initializes a vector of random DNA."
  (defn rand-gene []
    (rand-nth actions))
  (vec (repeatedly (** (count tiles) 5) rand-gene)))

(defn cross [a b]
  "Crosses two vectors of DNA."
  (defn choose-rand [x y]
    (if (< (rand-int 100) 50) x y))
  (mapv choose-rand a b))

(defn mutate [dna]
  "Randomly replace some genes in the DNA."
  (defn mutate-gene [gene]
    (if (< (rand-int 100) mutation-prob) (rand-gene) gene))
  (mapv mutate-gene dna))


; Fitness evaluation functions
(defn rand-map []
  "Initializes a map with walls at the boundaries and randomly positioned gold."
  (defn gen-tile [x y]
    (def last-coord(- map-size 1))
    (if (or (= x 0) (= y 0) (= x last-coord) (= y last-coord))
      \w
      (if (< (rand-int 100) gold-prob) \g \p)))
  (def map-coords (vec (range map-size)))
  (mapv (fn [y] (mapv (fn [x] (gen-tile x y)) map-coords)) map-coords))

(defn get-tile [the-map pos]
  "Given a map and coordinates, evaluates to what is on the map at the position."
  (let [[x y] (vec pos)]
    (nth (nth the-map y) x)))

(defn set-tile [the-map pos tile]
  "Returns a copy of the map where the tile on pos has been replaced."
  (let [[x y] (vec pos)]
    (def new-row (assoc (nth the-map y) x tile))
    (assoc the-map y new-row)))

(defn get-tile-index [t]
  "Returns the numeric index of a tile."
  (.indexOf tiles t))

(defn get-action-index [a]
  "Returns the numeric index of a tile."
  (.indexOf actions a))

(defn add-vec [v1 v2]
  "Adds 2 vectors"
  (map + v1 v2))


(defn simulate [dna the-map]
  "Simulates the DNA on a map. Evaluates to the score the robot achieved in simul-steps."
  (defn make-step [[current-map pos score]]
    "Makes a single step on the map, according to the DNA sequence."
    ;(print pos)
    (defn get-situation []
      "Returns the tile situation at the current pos."
      (def nearby-tiles (map (fn[v] (get-tile the-map (add-vec pos v))) dir-vects))
      (map get-tile-index nearby-tiles))
    (defn get-action [tile-situation]
      "Returns the index of action to take when faced with a given tile situation."
      (def multipliers (map (fn [n] (** (count tiles) n)) (range (count actions))))
      (reduce + (map * tile-situation multipliers)))
    (def action (nth dna (get-action (get-situation))))
    (case action
      (\l \r \u \d)
      (do
        (def new-pos (add-vec pos (nth dir-vects (get-action-index action))))
        (if (not= \w (get-tile current-map new-pos))
          [current-map new-pos score]
          [current-map pos (- score action-penalty)]))
      \x
      (do
        (def new-pos (add-vec pos (rand-nth (take 4 dir-vects))))
        (if (not= \w (get-tile current-map new-pos))
          [current-map new-pos score]
          [current-map pos (- score action-penalty)]))
      \p
      (do
        (if (= \g (get-tile current-map pos))
          [(set-tile current-map pos \p) pos (+ score gold-value)]
          [current-map pos (- score action-penalty)]))))
  (def final-state (nth (iterate make-step [the-map '(1 1) 0]) simul-steps))
  (nth final-state 2))


; Evolution functions
(defn evolve [individuals]
  "Takes a bunch of individuals (DNAs) and cross-breeds by fitness."
  (def the-map (rand-map))
  (def ordered (sort-by (fn[dna] (simulate dna the-map)) individuals))
  (println (simulate (last ordered) the-map))
  (def crossed 
    (map (fn [pair] (let [[a b] (vec pair)] (cross a b)))
         (partition 2 ordered)))
  (concat (mutate crossed) ()))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
