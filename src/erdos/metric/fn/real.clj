(ns erdos.metric.fn.real
  "Metric for real numbers")

(defn diff
  "Also known as: Absolute difference, Euclidean difference, L1, etc."
  ([a b] (Math/abs (- a b))))

(defn square [a b]
  (* (- a b) (- a b)))

(defn log [a b]
  (assert (pos? a))
  (assert (pos? b))
  (Math/abs (Math/log (/ (double a) (double b)))))

(defn rail
  "British Rail metric"
  [a b]
  (+ (Math/abs a) (Math/abs b)))
