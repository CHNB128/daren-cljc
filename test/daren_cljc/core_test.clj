(ns daren-cljc.core-test
  (:require [clojure.test :refer :all]
            [daren-cljc.core :refer :all]))

(deftest base-test
  (testing "infer-map"
    (let [x 1 y 2 z 3]
      (is (= {:x 1 :y 2 :z 3} (infer-map x y z))))))
