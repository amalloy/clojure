;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Author: Tassilo Horn

(ns clojure.test-clojure.reducers
  (:require [clojure.core.reducers :as r])
  (:use clojure.test))

(defmacro defequivtest
  ;; f is the core fn, r is the reducers equivalent, rt is the reducible ->
  ;; coll transformer
  [name [f r rt] fns]
  `(deftest ~name
     (let [c# (range -100 1000)]
       (doseq [fn# ~fns]
         (is (= (~f fn# c#)
                (~rt (~r fn# c#))))))))

(defequivtest test-map
  [map r/map #(into [] %)]
  [inc dec #(Math/sqrt (Math/abs %))])

(defequivtest test-mapcat
  [mapcat r/mapcat #(into [] %)]
  [(fn [x] [x])
   (fn [x] [x (inc x)])
   (fn [x] [x (inc x) x])])

(defequivtest test-reduce
  [reduce r/reduce identity]
  [+' *'])

(defequivtest test-filter
  [filter r/filter #(into [] %)]
  [even? odd? #(< 200 %) identity])

(defequivtest test-drop-while
  [drop-while r/drop-while #(into [] %)]
  [neg? pos? #(< % 200) #(> % 200) #{-100}])

(deftest test-iterate
  (is (= [100000]
         (->> (r/iterate inc 0)
              (r/drop 1e5)
              (r/take 1)
              (into [])))))

(deftest test-range
  (is (= (take 10000 (range))
         (->> (r/range)
              (r/take 10000)
              (into []))))
  (is (not (counted? (r/range))))
  (doseq [argvec [[7000]
                  [0 5736]
                  [10000 21 -2]
                  [0 3710 2/3]
                  [1 -8642 -2]]]
    (let [seq-version (apply range argvec)
          reduce-version (apply r/range argvec)
          reduced-vector (into [] reduce-version)]
      (is (counted? reduce-version))
      (is (= seq-version reduced-vector))
      (is (= (count reduce-version)
             (count reduced-vector)))
      (let [seq-sum (reduce + seq-version)
            folded-sum (r/fold + reduce-version)
            vector-fold-sum (r/fold + reduced-vector)]
        (is (= seq-sum folded-sum vector-fold-sum))))))

(deftest test-sorted-maps
  (let [m (into (sorted-map)
                '{1 a, 2 b, 3 c, 4 d})]
    (is (= "1a2b3c4d" (reduce-kv str "" m))
        "Sorted maps should reduce-kv in sorted order")
    (is (= 1 (reduce-kv (fn [acc k v]
                          (reduced (+ acc k)))
                        0 m))
        "Sorted maps should stop reduction when asked")))
