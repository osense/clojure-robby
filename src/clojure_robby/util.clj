(ns clojure-robby.util)

(defn ** [x n]
  "Raises x to the power of n."
  (reduce * (repeat n x)))

(defn write-lines [file lines]
  (with-open [wtr (clojure.java.io/writer file)]
    (doseq [line lines] (.write wtr line))))
