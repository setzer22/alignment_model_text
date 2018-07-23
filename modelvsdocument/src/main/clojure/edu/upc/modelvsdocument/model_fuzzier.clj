(ns edu.upc.modelvsdocument.model-fuzzier
  (:require
   [com.rpl.specter :as specter]
   [random-seed.core :refer :all]
   [edu.upc.modelvsdocument.utils :refer :all]
   [edu.upc.modelvsdocument.alignable :as al]
   [edu.upc.modelvsdocument.bpmn :as bpmn])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

(defn random-filter
  "Returns coll after randomly keeping elements with probability prob"
  [coll keep-prob]
  (filter (fn [x] (< (rand) keep-prob)) coll))

(defn add-feature-noise
  "Randomly discards features for those tasks that are adjacent/incident to a gateway.
   A gateway is selected with probability p-neighbor and its features are discarded with
   probability p-list"
  [model features p-neighbor p-list]
  (let [gateway? (partial bpmn/gateway? model)
        gateways (filter gateway? (bpmn/all-flow-elements model))
        gateway-neighborhoods (map (fn [gateway]
                                     (concat (map #(.getSourceRef %) (.getIncomingFlows gateway))
                                             (map #(.getTargetRef %) (.getOutgoingFlows gateway))))
                                   gateways)
        neighbors-to-noise (flatten (map #(random-filter % p-neighbor) gateway-neighborhoods))
        alignables-to-noise (distinct (map #(bpmn/get-element % model) neighbors-to-noise))]
    (reduce
     (fn [features g]
       (let [f (get features g)]
         (assoc features g (into [] (random-filter f p-list)))))
     features
     alignables-to-noise)))

(comment
  BEGIN

  (def --model (bpmn/construct-model "/home/josep/ModelsBpmn/Zoo.bpmn"))

  (def --dummy-features (mapify (map vector (bpmn/all-elements --model) (repeat [:a :b :c :d]))))

  (specter/transform [specter/ALL specter/FIRST] #(.getName %)
                     (add-feature-noise --model --dummy-features 1/3 0.5 1231983))

  END)
