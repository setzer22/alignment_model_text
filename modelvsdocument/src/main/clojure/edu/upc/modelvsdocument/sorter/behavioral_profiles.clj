(ns edu.upc.modelvsdocument.sorter.behavioral-profiles
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.bpmn :as bpmn :refer :all]
            [com.rpl.specter :refer :all]
            [clojure.set :as set])
  (:import [org.jbpt.pm ProcessModel Activity Gateway AndGateway XorGateway]
           [org.jbpt.pm.bpmn Task]
           [org.jbpt.bp BehaviouralProfile RelSet RelSetType]
           [org.jbpt.bp.construct BPCreatorUnfolding]
           [org.jbpt.petri PetriNet NetSystem]
           [org.jbpt.pm.structure ProcessModel2NetSystem]))

(def reltype->ordering
  {RelSetType/Order          :->
   RelSetType/ReverseOrder   :<-
   RelSetType/Exclusive      :!=
   RelSetType/Interleaving   :||})

(spec-fn compute-process-order-relation ::t/bpmn ::t/bpmn-process)

(defn mk-bp-creator-unfolding
  "Constructor function made so we can create multiple copies of this singleton class.
   If we don't use this, the code is not thread safe."
  []
  (let [constructor (.getDeclaredConstructor BPCreatorUnfolding (make-array Class 0))
        __ (.setAccessible constructor true)]
    (.newInstance constructor (make-array Object 0))))

(defn compute-process-order-relation [model process & {:keys [remove-back-edges?]}]
  (let [{:keys [jbpt-process id->jbpt-elements id->jbpt-tasks]} (process->jbpt-process model process :remove-back-edges? remove-back-edges?)
        net (ProcessModel2NetSystem/transform jbpt-process)
        bp-creator-unfolding (mk-bp-creator-unfolding)
        bp (.deriveRelationSet bp-creator-unfolding net)
        flow-elems (sort-by #(.getName %) (vals id->jbpt-elements))
        n (count flow-elems)]
    (into {} (for [t1 flow-elems]
                [(.getName t1)
                 (into {} (for [t2 flow-elems, :let [rel (.getRelationForEntities bp t1 t2)]]
                            [(.getName t2) (reltype->ordering rel)]))]))))

(comment
  (def model edu.upc.modelvsdocument.core/model-struct)
  (def processes (select [:processes ALL] model))
  (def process (second processes))

  (let [id->name (into {} (map (fn [id] [id (.getName (.getFlowElement (:model model) id))])
                              (elements-of-process process)))]
    (clojure.pprint/pprint id->name))

  (print *e)
  (def process-order (compute-process-order-relation model process))
  (every? not-nil? (for [t (bpmn/all-element-ids model)])
    (process-order t))

  (process-order "EndEvent_1fx9yp3"))

;((process-order "Task_0rpvccw") "Task_0sl26uo")

