(ns erdos.metric.fn.core)

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


;; Metrics Transformations

(defn add
  "Addition of two metrics"
  [m1 m2]
  (assert (ifn? m1))
  (assert (ifn? m2))
  (fn [a b] (+ (m1 a b) (+ m2 a b))))


(defn sum
  "Summation of finite number of metrics"
  [& ms]
  (assert (every? ifn? ms))
  (fn [a b] (reduce + (map #(% a b) ms))))


(defn mult
  "Multiplication of two metrics"
  [m1 m2]
  (assert (ifn? m1))
  (assert (ifn? m2))
  (fn [a b] (* (m1 a b) (m2 a b))))


(defn prod
  "Product of finite number of metrics"
  [& ms]
  (assert (every? ifn? ms))
  (fn [a b] (reduce * (map #(% a b) ms))))


(defn dilate
  "Dilatation of a metric"
  [c m]
  (assert (number? c))
  (assert (ifn? m))
  (fn [a b] (* c (m a b))))


(defn truncate [c m]
  (assert (number? c))
  (assert (ifn?))
  (fn [a b] (min c (m a b))))


(defn uniformly [x m]
  (assert (number? x))
  (assert (ifn? m))
  (fn [a b] (max x (m a b))))


(defn translate [c m]
  (assert (number? c) "c is not a number!")
  (assert (>= c 0) "c is negative!")
  (assert (ifn? m) "m is not a metric!")
  (fn [a b] (if (= a b) 0.0 (+ c (m a b)))))
