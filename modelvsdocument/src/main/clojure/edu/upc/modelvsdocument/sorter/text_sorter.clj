(ns edu.upc.modelvsdocument.sorter.text-sorter
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.schemas :as t :refer :all])
  (:use [com.rpl.specter] 
        [edu.upc.modelvsdocument.utils])
  (:gen-class))

(spec-fn build-order-matrix ::t/text -> ::t/order-matrix)
(defn build-order-matrix [text]
  (let [alignables (a/get-all-alignables text)]
    (reduce
     (fn [m [s s']]
       (let [[id id'] [(Integer/parseInt (a/id s))
                       (Integer/parseInt (a/id s'))]]
         (cond (neg? (* id id')) (assoc-in m [s s'] :||)
               (< id id')        (assoc-in m [s s'] :->)
               (> id id')        (assoc-in m [s s'] :<-)
               :else             (assoc-in m [s s'] :!=))))
     {}
     (for [s alignables, s' alignables] [s, s']))))

