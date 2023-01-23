(ns solver.solver
  (:require [solver.util])
  (:use solver.util))

(defn refine-moves "return narrowed move set containing only legal moves" [moveset state checker]
  (into {} (map (fn [[pos mvs]] [pos (into [] (filter #(checker state pos %1) mvs))]) moveset)))

(defn solve_
  [moveset state checker]
  (if (= 0 (count moveset))
    (list [state "found solution"])
    (let [refined-moves (refine-moves moveset state checker)
          [k vs] (apply min-key count refined-moves)
          msmk (dissoc moveset k)
          out (map #(conj (solveall_ msmk (assoc state k %) checker) [state [k vs %]]) (unchunk vs))
          ans (first (filter (comp not nil? second) out))]
      ans)))

(defn filter-moves
  [ms st]  (into {} (filter #((comp not contains?) st (first %)) ms)))

(defn solve
  "Solve a given problem.
   Moveset: collection positions -> potential (non-nil) values
   State: collection positions -> current value (possibly nil)
   Checker: State -> position -> value -> bool"
  [moveset state checker]
  (solveall_
   (filter-moves moveset state)
   state checker))

;; (def solve (comp first solveall))

;; (def skiptosol (comp second last first))
