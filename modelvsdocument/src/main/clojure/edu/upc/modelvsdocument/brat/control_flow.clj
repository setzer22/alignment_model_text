(ns edu.upc.modelvsdocument.brat.control-flow
  (:require [clj-pipeline.core :as pipeline]
            [loco.core :refer :all]
            [loco.constraints :refer :all]
            [clojure.math.combinatorics :as comb]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.brat.utils :refer :all]
            [edu.upc.modelvsdocument.brat.specs :as brt]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn infer-relations
  "Given an initial list of *certain* order relations, returns a fuzzy
   ordering with all possible interpretations."
  [ids, base-relations, end-events]
  (let [int->rel {0 :??, 1 :!=, 2 :||, 3 :->, 4 :<-}
        rel {:?? 0, :!= 1, :|| 2, :-> 3, :<- 4}
        vars (for [t1 ids, t2 ids
                   :when (not= t1 t2)]
               [:relation t1 t2])
        problem
        (concat

         ;; Variable Definitions
         (for [var vars]
           ($in var (vals rel)))

         ;; Constraint 1: Symmetry
         (for [[v t1 t2] vars, reltype [:-> :<- :|| :!=]
               :let [r (rel reltype)
                     inv-r (rel ({:-> :<-, :<- :->, :|| :||, :!= :!=} reltype))]]
           ($if ($= [v t1 t2] r)
                ($= [v t2 t1] inv-r)))

         ;; Constraint 2: Transitivity
         (for [[v t1 t2] vars, [v t2' t3] vars, reltype [:-> :<- :|| :!=]
               :when (and (= t2 t2') (not= t1 t3))
               :let [r (rel reltype)]]
           ($if ($and ($= [v t1 t2] r)
                      ($= [v t2 t3] r))
                ($= [v t1 t3] r)))

         ;; Constraint 3: Sequentiality
         ;; There is the assumption that there is an implicit ->
         ;; relation if two consecutive tasks have no explicit
         ;; relation between them.
         ;; NOTE: Since the symmetric property is not enforced in
         ;; the initial relations, we must check both ways.
         (for [[t1 t2] (partition 2 1 ids)
               :when (and (not (get-in base-relations [t1 t2]))
                          (not (get-in base-relations [t2 t1]))
                          (not (end-events t1)))]
           ($= [:relation t1 t2] (rel :->)))

         ;; Constraint 4: Base relations, explicit in the annotations
         (for [t1 ids, t2 ids
               :let [r (get-in base-relations [t1 t2])]
               :when r]
           ($= [:relation t1 t2] (rel r)))

         [])
        sols (solution problem :minimize (apply $+ vars))]
    ;; NOTE: The amount of possible interpretations for a large enough text
    ;;       with many free order relations is potentially huge. A text with
    ;;       |A| activities and 4 relations has at most 4^|A| possible
    ;;       interpretations. That number is obviously unfeasible but for now
    ;;       we'll just assume in practice is going to be much lower.
    (if (seq sols)
      (->> sols
           (transform [ALL MAP-VALS] int->rel)
           (transform [ALL MAP-KEYS] #(into [] (rest %)))
           (transform [ALL] nest))
      ::contradictory-control-flow)))

#_(infer-relations ["Check" "Retrieve" "Specify" "PlaceOrder" "PlaceEmail"]
                   {"Check" {"Specify" :->
                             "Retrieve" :->}
                    "Specify" {"Retrieve" :!=}})

(pipeline/defpipe-step compute-flows [relation-graph actions]
  (let+ [action-ids (map :id (sort-by :position actions))
         end-events (set (map :id (filter :end? actions)))
         initial-facts :dbg (-> (for [a action-ids]
                                  (mapcat
                                   (fn [brat-r, rel]
                                     (map #(-> [[%1 %2] %3])
                                          (repeat a)
                                          (adj-with-type brat-r relation-graph a)
                                          (repeat rel)))
                                   ["DirectlyFollows" "LateFollows" "IsExclusive" "IsParallel"]
                                   [      :->             :->          :!=           :||     ]))
                                (concat-seq)
                                (nest))]
    {:flows (infer-relations action-ids initial-facts end-events)}))
