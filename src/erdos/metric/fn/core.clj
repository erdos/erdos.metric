(ns erdos.metric.fn.core
  (:require [clojure.set :as set]))

(defn intersection-count
  "number of items found in both collections."
  [xs ys]
  (if (or (empty? xs) (empty? ys))
    0
    (let [xs (set xs)]
      (loop [i 0, ys (seq (set ys))]
        (if ys
          (if (xs (first ys))
            (recur (inc i) (next ys))
            (recur i (next ys)))
          i)))))

;; (intersection-count "abcde" "abggg") => 2
;; (intersection-count "aaaa" "aab") => 1


(defn discrete
  "Discrete metric"
  [a b]
  (if (= a b) 0.0 1.0))

(defmacro reduce-two [result a b])


;(defmacro defmetric [sym m body])

(comment

  ;; IDEAS: protocols for various properties.

  (defprotocol HasSupremum
    (supremum [_]))

  (defn bound? [x] (satisfies? BoundMetric x))

  (defprotocol MetricAxioms
    (separation? [_])
    (indiscernibles? [_])
    (symmetry? [_])
    (subadditivity? [_]))

  (defprotocol FiniteDomain
    (enum-domain [_]))
  (defprotocol FiniteRange
    (enum-range [_]))

  (defprotocol )

  (defn add [m1 m2]
    (metric {:bound (if (and (bound? m1) (bound? m2))
                      (+ (bound m1) (bound m2)))
             :posdef (and (:posdef m1) (:posdef m2))
             :symmetriy (and (:symmetry m1) (:symmetry m2))
             :triangle (and (triangle? m1) (triangle? m2))
             :ultrametric false
             :intrinsic false
             :translation-invariant true
             :range (if (and (:range m1) (:range m2))
                      (for [x (:range m1) y (:range m2)]
                        (+ m1 m2)))
             :domain (if (and (:domain m1)
                              (:domain m2))
                       (set/intersection (set (:domain m1))
                                         (set (:domain m2))))

             }
            (+ (m1 %1 %2) (m2 %1 %2))

            )
    )

  )
