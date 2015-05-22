(ns clojure-robby.util)

(defn ** [x n]
  "Raises x to the power of n."
  (reduce * (repeat n x)))

