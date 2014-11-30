(ns clusterization.core-test
  (:require [clojure.test :refer :all]
            [clusterization.core :refer :all])
  (import java.lang.Math))

(deftest euclid-distance-test
  (are [p1 p2] (= p1 p2)              
	   1.0 (euclid_distance [2] [1])
       2.0 (euclid_distance [4] [2])
	  (Math/sqrt 8) (euclid_distance [1 2] [3 4])))

(deftest hamming-distance-test
  (are [p1 p2] (= p1 p2)
	   3 (hamming_distance [1 2 3] [4 5 6])    
       2 (hamming_distance [5 3] [2 7])      
       1 (hamming_distance [6.0] [-6.0])
       0 (hamming_distance [-1] [-1])))

(deftest get-point-potential-test
  (let [p [2 4]
        points [[2 4] [1 2] [3 3]]
        alpha 1.5]
    (is (= (+ 1 (Math/exp (- (* alpha (Math/sqrt 5)))) (Math/exp (- (* alpha (Math/sqrt 2)))))
         (first (calculate_point_potential p points euclid_distance alpha))))))