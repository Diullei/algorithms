(ns ninety-nine-clojure-problems.problem-1
  (:use clojure.test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    (*) Find the last element of a list.
;
;    (Note that the Lisp transcription of this problem is incorrect.)
;
;    Example in Haskell:
;
;    Prelude> myLast [1,2,3,4]
;    4
;    Prelude> myLast ['x','y','z']
;    'z'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SOLUTION HERE

(defn my-last [xs]
  (if (= (count xs) 1)
         (first xs)
         (my-last (rest xs))))

;; TEST CASE

(deftest Problem01
  (is (= 4 (my-last '(1 2 3 4)))))
