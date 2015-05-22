(ns clojure-robby.util)

(defn ** [x n]
  "Raises x to the power of n."
  (reduce * (repeat n x)))

(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

