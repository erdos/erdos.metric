(ns erdos.metric.fn.set
  (:require [erdos.metric.fn.core :as c]
            [clojure.set :as set]))

(defn steinhaus [pt m]
  (assert (ifn? m))
  (fn [a b] (/ (* 2.0 (m a b)) (+ (m a pt) (m b pt) (m a b)))))

(defn schoenberg [lambda d]
  ;; schoenberg transform metric
  (assert (and (float? lambda) (pos? lambda)))
  (assert (ifn? d))
  (fn [a b] (- 1.0 (Math/exp (* -1.0 lambda (d a b))))))


                                        ; METRICS

(defn jaccard
  "Jaccard metric"
  [a b]
  (assert (set? a)) (assert (set? b))
  (if (= a b)
    0.0
    (let [|ab| (c/intersection-count a b)]
      (- 1.0 (/ |ab| (+ (.count a) (.count b) (- |ab|)))))))

(defn dice
  "Sorensen-Dice distance"
  [xs ys]
  (assert (set? xs)) (assert (set? ys))
  (if (= xs ys) 0.0
      (- 1.0 (/ (* 2.0 (c/intersection-count xs ys))
                (+ (.count xs) (.count ys))))))

(defn ochiai
  "Ovhiai metric."
  [xs ys]
  (assert (set? xs)) (assert (set? ys))
  (if (or (empty? xs) (empty? ys))
    nil ;; TODO: return big val (??)
    (/ (double (count (set/intersection xs ys)))
       (Math/sqrt (* (.count xs) (.count ys))))))



                                        ; SIMILARITIES

(defn sokal-sneath-2-sim [x y]
  (assert (set? x)) (assert (set? y))
  (let [xub (set/union x y) xmb (set/intersection x y)
        symdiff (set/difference xub xmb)]
    (/ (count xmb)
       (+ (count xub) (count symdiff)))))

(defn simpson-sim [x y]
  (assert (set? x)) (assert (set? y))
  (if (or (empty? x) (empty? y)) 0.0
      (/ (count (set/intersection x y))
         (min (count x) (count y)))))

(defn braun-blanquet-sim [x y]
  (assert (set? x)) (assert (set? y))
  (/ (count (set/intersection x y))
     (max (count x) (count y))))

(defn mountford-sim [x y]
  (assert (set? x)) (assert (set? y))
  (/ (count (set/intersection x y))
     (+ (* (count x) (count (set/difference y x)))
        (* (count y) (count (set/difference x y))))))
