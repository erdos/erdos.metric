(ns erdos.metric.fn.transform
  (:require [erdos.metric.fn.core :as c]))

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
