(ns erdos.metric.fn.vector
  (:require [erdos.metric.fn.core :as c]))

(defmacro ^:private square [a]
  `(let [a# ~a] (* a# a#)))

(defn l1 [a b]
  (loop [x 0,
         [a & as :as aa] (seq a),
         [b & bs :as bb] (seq b)]
    (if (or aa bb)
      (recur (+ x (Math/abs (- (or a 0) (or b 0))))
             as bs)
      x)))

(defn l2 [a b]
  (loop [x 0,
         [a & as :as aa] (seq a),
         [b & bs :as bb] (seq b)]
    (if (or aa bb)
      (recur (+ x (square (- (or a 0) (or b 0))))
             as bs)
      x)))

(defn lp [p, a b]
  (loop [x 0,
         [a & as :as aa] (seq a),
         [b & bs :as bb] (seq b)]
    (if (or aa bb)
      (recur (+ x (Math/pow (Math/abs (- (or a 0) (or b 0))) p))
             as bs)
      x)))

:ok
