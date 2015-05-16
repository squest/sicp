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

(def coins [1 2 5 10 20 50 100 200])

(def
  sum-coins
  (memoize
    (fun ([amount] (sum-coins amount 7))
         ([0 _] 1)
         ([amount :guard #(neg? %) _] 0)
         ([_ 0] 1)
         ([amount coin]
           (let [coin-value (nth coins coin)]
             (reduce + (map #(sum-coins (- amount (* coin-value %))
                                        (- coin 1))
                            (range (inc (quot amount coin-value))))))))))
(defun
  sum-coin
  ([amount] (sum-coin amount 7))
  ([0 _] 1)
  ([amount :guard #(neg? %) _] 0)
  ([_ 0] 1)
  ([amount coin]
    (let [coin-value (nth coins coin)]
      (reduce + (map #(sum-coin (- amount (* coin-value %))
                                (- coin 1))
                     (range (inc (quot amount coin-value))))))))

(defn sum-coins-1
  ([amount] (sum-coins-1 amount 7))
  ([amount coin]
   (cond
     (== 0 amount) 1
     (< amount 0) 0
     (== 0 coin) 1
     :else (let [cvalue (nth coins coin)
                 lim (+ 1 (quot amount cvalue))]
             (loop [i (int 0) res (int 0)]
               (if (> i lim)
                 res
                 (recur (+ 1 i)
                        (+ res (sum-coins-1 (- amount (* cvalue i))
                                            (- coin 1))))))))))


