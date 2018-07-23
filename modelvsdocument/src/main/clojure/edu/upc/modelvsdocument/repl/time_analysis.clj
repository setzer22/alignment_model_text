(ns edu.upc.modelvsdocument.repl.time-analysis
  (:require [incanter.core :refer :all]
            [incanter.charts :refer :all]
            [incanter.stats :refer :all]))

(def sizes [9 6 12 7 4 10 9 5 4 10 12 11 4 4 8 5 8 26 19 6 6 4 11 6 18 5 9 14 4 5 4 5 5 5 6 8])
(def exec-times [224 78 293 203 42 178 125 56 43 200 204 234 45 37 98 77 95 5965 1485 99 70 55 191 104 21436 75 131 587 55 108 64 50 78 75 85 131])
(def their-times [8 3 35 13 1 27 6 1 1 4524 11 13 2 1 4 2 5 10202 66068 5 13 2 20 4 408188 3 22 43759 1 7 4 2 3 3 4 35])

(view (doto (scatter-plot sizes exec-times :x-label "Number of tasks" :y-label "Execution time (ms)")
        (add-points sizes their-times)))
