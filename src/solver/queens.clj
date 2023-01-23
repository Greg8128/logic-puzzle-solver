(ns solver.queens
  (:require [solver.solver :as solver])
  (:use solver.solver))

;; An approach for solving the n-queens puzzle
;; It is taken for granted that each queen will be on a different row
;; The solver finds the column for each row

(defn msq
  "Generarte the initial move set for n queens"
  [nqueens] (into {} (for [r (range nqueens)] (vector r (into [] (range nqueens))))))

(defn
  checkerq
  "Check the given move"
  [state k v]
  (and
   (->> state
        (map #(apply + %))
        (filter #(= (+ k v) %))
        empty?)
   (->> state
        (map #(apply - %))
        (filter #(= (- k v) %))
        empty?)
   (->> state
        (map second)
        (filter #(= v %))
        empty?)))

(defn sol
  "Solve the n queens puzzle. An initial configuration 'initp' may be provided."
  ([n] (sol n (sorted-map)))
  ([n initp] (solver/solve (msq n) (into (sorted-map) initp) checkerq)))
