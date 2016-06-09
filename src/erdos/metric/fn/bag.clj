(ns erdos.metric.fn.bag
  "Metrics between bag (multiset) objects."
  (:require [clojure.set :as set]))

;; BAG is an immutable multiset implementation.

(deftype ^:once Bag [^clojure.lang.IPersistentMap m, ^int s]
  clojure.lang.IPersistentSet
  (get [this x]
    (if-let [e (find m x)] (key e)))
  (contains [this x] (contains? m x))
  (disjoin [this x]
    (if-let [[[_ c] v] (find m x)]
      (if (= 1 c)
        (Bag. (dissoc m x) (dec s))
        (Bag. (assoc m x (dec c)) (dec s)))
      this))
  clojure.lang.Seqable
  (seq [this] (mapcat (fn [[x n]] (repeat n x)) m))
  clojure.lang.Counted
  (count [this] s)
  ;;clojure.lang.IMeta
  Object
  (equals [this x] (if (instance? Bag x) (.equals m (.m x)) false))
  (hashCode [this] (hash-combine (hash m) Bag))
  clojure.lang.IFn
  (invoke [this k] (if (find m k) k))
  (invoke [this k def] (if (find m k) k def)))

(def empty-bag (Bag. {} 0))

(defn seq->bag [x]
  (Bag. (frequencies x) (count x)))

(defn bag [x]
  (cond (instance? Bag x) x
        (sequential? x) (seq->bag (seq x))
        (set? x)   (Bag. (zipmap (seq x) (repeat 1)) (count x))
        (map? x)   (Bag. x (reduce + (vals x)))
        (nil? x)  empty-bag
        :else (-> (format "Can not make bag from: %s" (type x))
                  (IllegalArgumentException.)
                  (throw))))

(defn bag? [x] (instance? Bag x))

(defn intersection
  ([a] a)
  ([a b & xs] (reduce intersection (intersection a b) xs))
  ([a b]
   (assert (bag? a) (str "expected bag, got: " (type a)))
   (assert (bag? b) (str "expected bag, got: " (type b)))
   (let [m2 (.m b)
         f (fn [m [k v]] (if-let [c (m2 k)] (assoc m k (min c v)) m))]
     (bag (reduce f {} (.m a))))))

(defn union
  ([a] a)
  ([a b & xs] (reduce union (union a b) xs))
  ([a b]
   (assert (bag? a)) (assert (bag? b))
   (let [m2 (.m b)
         f (fn [m [k v]] (if-let [c (m2 k)] (assoc m k (max c v)) m))]
     (bag (reduce f {} (.m a))))))

(defn subtract
  ([a & ds]
   (-> (fn [a x]
         (reduce (fn [a [k v]]
                   (if-let [c (get a k)]
                     (if (<= c v)
                       (dissoc a k)
                       (assoc a k (- c v)))
                     a)) a (seq (.m x))))
       (reduce (.m (bag a)) (map bag ds))
       (bag))))


(defn bag-distance
  "Bag distance: max {|a-b|, |b-a|}"
  [xs ys]
  (assert (bag? xs)) (assert (bag? ys))
  (max (count (subtract xs ys))
       (count (subtract ys xs))))


'END
