(ns cljsicp.one
  (:require [defun :refer [defun fun letfun]]))

(defn sqrt
  [n]
  (let [improve-guess (fn [guess]
                        (/ (+ guess (/ n guess)) 2))
        good-enough? (fn [guess]
                      (< (Math/abs (- (* guess guess) n)) 0.00001))
        sqrt-iter (fn sqrt-iter [i]
                    (if (good-enough? i)
                      i
                      (sqrt-iter (improve-guess i))))]
    (sqrt-iter 1.0)))


(defun
  fibo
  ([1] 1)
  ([i] (recur i 1 0))
  ([0 a b] a)
  ([i a b] (recur (- i 1) (+' a b) a)))

(defun
  fibo-list
  ([i] (recur i 1 0 []))
  ([0 a b res] res)
  ([i a b res] (recur (- i 1) (+' a b) a (conj res a))))

