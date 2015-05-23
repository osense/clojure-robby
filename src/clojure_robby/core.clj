(ns clojure-robby.core
  (:gen-class)
  (:use clojure-robby.genetic))


; Evolution functions
(defn select [individuals]
  "Takes a bunch of individuals (DNAs) and returns the better half of the population, sorted from worst."
  (let [the-maps (repeatedly map-count rand-map)
        eval-map (fn [a-map]
                      (mapv (fn [a-dna] (simulate a-dna a-map)) individuals))
        fitnesses (reduce (partial mapv +) (pmap eval-map the-maps))
        sorted (sort-by #(nth fitnesses (.indexOf individuals %)) individuals)]
    (println (float (/ (reduce + fitnesses) (* individual-count map-count))))
    (drop (/ (count individuals) 2) sorted)))

(defn breed [individuals]
  "Takes a bunch of individuals (DNAs) and returns twice as many after breeding."
  (let [breed-one (fn [dna]
                   (cross dna (rand-nth individuals)))
        breed-all (fn []
                    (mapv breed-one individuals))]
    (mapv mutate (concat (breed-all) (breed-all)))))

(defn evolve-from [population n]
  "Returns the population after n generations, starting with a given population."
  (let [iteration (iterate #(breed (select %)) population)]
    (nth iteration n)))

(defn evolve [n]
  "Returns the population after n generations, starting with a random population."
  (evolve-from (repeatedly individual-count rand-dna) n))

(defn best [population]
  "Returns the best individual in a generation."
  (if (> (count population) 1)
    (best (select population))
    (first population)))


; Entry point, starts a REPL
(defn -main [& args]
  (let [repl
        (fn []
          (print "=> ")
          (flush)
          (println (eval (read-string (read-line)))))]
    (eval (use 'clojure-robby.core 'clojure-robby.genetic))
    (println "Started REPL. Type (help) for help and ^C to exit.")
    (while true (try (repl) (catch Exception e (println (.getMessage e)))))
    (shutdown-agents)))

(defn help []
  (doall (map println ["Clojure-robby, a genetic algorithm that evolves a simple robot.",
                       "Main funcions:",
                       "   help           - Prints this help.",
                       "   config         - Prints current configuration."
                       "   evolve n       - Returns a random population, after evolving for n generations.",
                       "   evolve-from p n- Returns population p, after evolving it for n generations.",
                       "   best p         - Returns the DNA of the best individual in a population.",
                       "",
                       "Example usage:",
                       "(def champ (nth-best 50))"]))
  'ok)

(defn config []
  (doall (map (fn [key] (println key "=" (eval key))) configs))
  (println "Redefine any of these by calling (def name value); e.g. (def gold-prob 20)")
  'ok)


