(ns erdos.metric.fn.n-gram)

''''''
(defn q-gram-memoized
  "Creates a q-gram distance function that caches input to bag conversion."
  [n]
  (assert (integer? n)) (assert (pos? n))
  (let [tobag (memoize (comp bag (partial ngrams n)))]
    (fn [x y]
      (assert-seqable x) (assert-seqable y)
      (cond (= x y) 0.0
            (empty? x) 1.0 (empty? y) 1.0
            :by-the-way
            (let [x (tobag x) y (tobag y)]
              (- 1.0
                 (/ (* 2.0 (count (bag-intersection x y)))
                    (+ (count x) (count y)))))))))

'''''
(defn q-gram
  "q-gram distance"
  [n x y]
  (assert (integer? n)) (assert (pos? n)) (assert-seqable x) (assert-seqable y)
  (let [x (bag (ngrams n x)) y (bag (ngrams n y))]
    (- 1.0
       (/ (* 2.0 (count (bag-intersection x y)))
          (+ (count x) (count y))))))


;; ((q-gram-memoized 2) "abacus" "babzsak")
;;; ((q-gram-memoized 2) "abacus" "aba")
;; ((q-gram-memoized 2) "abacus" "abacus")
;; ((q-gram-memoized 2) "abacus" "www")
