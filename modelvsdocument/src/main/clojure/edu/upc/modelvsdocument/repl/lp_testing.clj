(ns edu.upc.modelvsdocument.repl.lp-testing
  (:require [loco.core :refer :all]
            [loco.constraints :refer :all]))

(solution
  [($in :t1 0 1000)
   ($in :t2 0 1000)
   ($in :t3 0 1000)
   ($in :t4 0 1000)
   ($in :t5 0 1000)
   ($in :t6 0 1000)
   ($>= :t1 15)
   ($>= ($+ :t1 :t2) 35)
   ($>= ($+ :t2 :t3) 65)
   ($>= ($+ :t3 :t2) 80)
   ($>= ($+ :t4 :t5) 40)
   ($>= ($+ :t5 :t6) 25)]
  :minimize ($+ :t1 :t2 :t3 :t4 :t5 :t6))
