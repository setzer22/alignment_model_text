(ns edu.upc.modelvsdocument.lap 
  (:gen-class)
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter])
  (:require [clojure.spec :as spec] 
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all])
  (:import (edu.upc.modelvsdocument Matriu LAPSolver)))

(spec-fn make-matrix ::t/cost-matrix)
(defn make-matrix [matrix] 
  (let [m (into-array (map float-array matrix))]
    (Matriu. m)))

(spec-fn solve-lap ::t/cost-matrix)
(defn solve-lap [matrix] 
  "Solves the LAP that's encoded by matrix (maximizing). The matrix must be square."
  {:pre [(= (count matrix) (count (nth matrix 1)))]}
  (defonce lap-solver (LAPSolver. (count matrix)))
  (let [matrix  (transform [ALL ALL] - matrix)]
    (.setSize lap-solver (count matrix))
    (.resol lap-solver (make-matrix matrix))
    (vec (.getAssignacio lap-solver))))
