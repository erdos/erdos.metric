(ns erdos.metric.fn.seq
  "Distances of sequences.")

(defn lcp-length
  "Length of longest common prefix."
  [xx yy]
  (loop [xx (seq xx) yy (seq yy) i 0]
    (if (and xx yy (= (first xx) (first yy)))
      (recur (next xx) (next yy) (inc i)) i)))

(defn hamming
  "Hamming distance: number of distinct values on same position."
  [xs ys]
  (assert (sequential? xs))
  (assert (sequential? ys))
  (count (filter false? (map = xs ys))))

(defn baire
  "Baire metric"
  [xs ys]
  (assert-seqable xs) (assert-seqable ys)
  (/ 1.0 (+ 1.0 (|lcp| xs ys))))

(defn cantor
  "Cantor metric"
  ([c xs ys]
   (assert (and (float? c) (< 0 c 1)) "Invalid constant parameter")
   (assert-seqable xs)
   (assert-seqable ys)
   (if (= xs ys) 0.0
       (Math/pow c (+ 1.0 (|lcp| xs ys)))))
  ([xs ys] (cantor 0.5 xs ys)))
