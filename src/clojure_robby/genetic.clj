(ns clojure-robby.genetic)

(use 'clojure-robby.util)


(def map-count 20) ; The amount of maps to evaluate each individual on.
(def individual-count 30); The amount of individuals to operate on.

(def map-size 20) ; Size of the map used for robot simulation.
(def gold-prob 50) ; Probability in 100 that a tile contains gold.
(def simul-steps 100) ; Steps to allow the robot to make in a simulation.
(def tiles [\w \p \g]) ; Tiles that populate the map: wall, path, and gold.
(def actions [\u \d \l \r \p \x]) ; Actions the robot can take: up, down, left, right, pick up, move random.

(def multipliers (map (fn [n] (** (count tiles) n)) (range 5))) ; Used to calculate genes to use.
(def dir-vects ['(0 -1) '(0 1) '(-1 0) '(1 0) '(0 0)]) ; Direction vectors, corresponding to the 5 actions.

(def mutation-prob 1) ; Probability in 100 that a gene will mutate.
(def wall-penalty 5) ; How many points robot loses when hitting the wall
(def pick-up-penalty 1) ; How many points the robot loses when pickin up on an empty spot.
(def gold-value 10) ; How many points picking up gold is worth.

(def configs ['map-count, 'individual-count, 'map-size, 'gold-prob, 'simul-steps,
              'mutation-prob, 'wall-penalty, 'pick-up-penalty, 'gold-value]) ; List of configuration variables.


; DNA functions
(defn rand-gene []
  "Returns a random gene."
  (rand-nth actions))

(defn rand-dna []
  "Initializes a vector of random DNA."
  (vec (repeatedly (** (count tiles) 5) rand-gene)))

(defn cross [a b]
  "Crosses two vectors of DNA."
  (let [choose-rand (fn [x y] (if (< (rand-int 100) 50) x y))]
    (mapv choose-rand a b)))

(defn mutate [dna]
  "Randomly replace some genes in the DNA."
  (let [mutate-gene (fn [gene] (if (< (rand-int 100) mutation-prob) (rand-gene) gene))]
    (mapv mutate-gene dna)))


; Fitness evaluation functions
(defn rand-map []
  "Initializes a map with walls at the boundaries and randomly positioned gold."
  (let [last-coord (- map-size 1)
        map-coords (vec (range map-size))
        gen-tile (fn[x y]
                   (if (or (= x 0) (= y 0) (= x last-coord) (= y last-coord))
                     \w
                     (if (< (rand-int 100) gold-prob) \g \p)))]
    (mapv (fn [y] (mapv (fn [x] (gen-tile x y)) map-coords)) map-coords)))

(defn get-tile [the-map pos]
  "Given a map and coordinates, evaluates to what is on the map at the position."
  (let [[x y] pos]
    (nth (nth the-map y) x)))

(defn set-tile [the-map pos tile]
  "Returns a copy of the map where the tile on pos has been replaced."
  (let [[x y] pos
        new-row (assoc (nth the-map y) x tile)]
    (assoc the-map y new-row)))

(defn get-tile-index [t]
  "Returns the numeric index of a tile."
  (.indexOf tiles t))

(defn get-action-index [a]
  "Returns the numeric index of a tile."
  (.indexOf actions a))

(defn add-vec [v1 v2]
  "Adds 2 vectors"
  (mapv + v1 v2))

(defn simulate [dna the-map]
  "Simulates the DNA on a map. Evaluates to the score the robot achieved in simul-steps."
  (let [make-step
        (fn [[current-map pos score]]
          "Makes a single step on the map, according to the DNA sequence."
          (let [situation (mapv (fn[v] (get-tile-index (get-tile current-map (add-vec pos v)))) dir-vects)
                action-idx (reduce + (mapv * situation multipliers))
                action (nth dna action-idx)
                step-to 
                (fn [the-pos]
                  (if (not= \w (get-tile current-map the-pos))
                    [current-map the-pos score]
                    [current-map pos (- score wall-penalty)]))]
            ;(print pos)
            (case action
              (\l \r \u \d)
              (let [new-pos (add-vec pos (nth dir-vects (get-action-index action)))]
                (step-to new-pos))
              \x
              (let [new-pos (add-vec pos (rand-nth (take 4 dir-vects)))]
                (step-to new-pos))
              \p
              (if (= \g (get-tile current-map pos))
                [(set-tile current-map pos \p) pos (+ score gold-value)]
                [current-map pos (- score pick-up-penalty)]))))
        iteration (iterate make-step [the-map [1 1] 0])
        [_ final-pos final-score] (nth iteration simul-steps)]
    final-score))

