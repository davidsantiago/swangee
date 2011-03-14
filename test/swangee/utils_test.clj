(ns swangee.utils-test
  (:use clojure.test
        swangee.utils))

(deftest test-as-coll
  (is (= nil     (as-coll nil)))
  (is (= #{}     (as-coll #{})))
  (is (= []      (as-coll [])))
  (is (= '()     (as-coll '())))
  (is (= [\a]    (as-coll \a)))
  (is (= [1]     (as-coll 1)))
  (is (= [:a]    (as-coll :a)))
  (is (= ["a"]   (as-coll "a")))
  (is (= #{1}    (as-coll #{1})))
  (is (= [1]     (as-coll [1])))
  (is (= [1 [1]] (as-coll [1 [1]]))))