(ns edu.upc.modelvsdocument.script.json-bpmn-parser
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [clojure.set :as set]
            [clojure.spec :as spec]
            [clojure.java.io :as io]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clojure.inspector :as inspector]
            [clojure.data.json :as json]
            [clojure.data.xml :as xml]
            [clojure.walk :as walk]))

(def json-type->bpmn-type
  {"EndCancelEvent" "endEvent"
   "IntermediateErrorEvent" "intermediateCatchEvent"
   "MessageFlow" "messageFlow"
   "Lane" "lane"
   "BPMNDiagram" "UNUSED"
   "InclusiveGateway" "inclusiveGateway"
   "StartMessageEvent" "startEvent"
   "SequenceFlow" "sequenceFlow"
   "EventbasedGateway" "eventBasedGateway"
   "ParallelGateway" "parallelGateway"
   "IntermediateEvent" "intermediateCatchEvent"
   "EndMessageEvent" "endEvent"
   "IntermediateTimerEvent" "intermediateCatchEvent"
   "Exclusive_Databased_Gateway" "exclusiveGateway"
   "IntermediateMessageEventThrowing" "intermediateThrowEvent"
   "StartErrorEvent" "startEvent"
   "EndNoneEvent" "endEvent"
   "EndSignalEvent" "endEvent"
   "IntermediateMessageEventCatching" "intermediateCatchEvent"
   "StartTimerEvent" "startEvent"
   "Pool" "pool"
   "StartNoneEvent" "startEvent"
   "CollapsedSubprocess" "UNUSED"
   "Subprocess" "UNUSED"
   "EndErrorEvent" "endEvent"
   "Task" "task"})

(spec/def ::stencil (spec/keys :req-un [:unq/id]))
(spec/def ::child-shapes (spec/* ::node))
(spec/def ::properties map?)
(spec/def ::node (spec/keys :req [::stencil ::child-shapes ::properties]))

(defn node? [maybe-node] (spec/valid? ::node maybe-node))

(defn json-tree-seq [node]
  (tree-seq #(-> % :childShapes empty? not) :childShapes node))

(defn build-outgoing-graph [main-node]
  (let [nodes (json-tree-seq main-node)
        initial-graph {}]
    (reduce 
      (fn [graph, node]
        (-> graph 
            (assoc (:resourceId node) (map :resourceId (:outgoing node))))) 
      initial-graph
      nodes)))

(defn make-id-generator [] 
  (let [counter-table (atom {})] 
    (fn [t] 
      (let [current-count (get @counter-table t 0)]
        (swap! counter-table assoc t (inc current-count))
        (str t "_" current-count)))))

(defn gen-id! [node id-generator]
  (id-generator (-> node :stencil :id)))


(defn make-id-mapping [json-model] 
  (let [id-generator (make-id-generator)] 
    (as-> json-model $$
      (json-tree-seq $$)
      (map (fn [node] [(:resourceId node) (-> node :stencil :id)]) $$)
      (map (fn [[id t]] [id (id-generator t)]) $$)
      (into {} $$))))

(defn dissoc-except [map & ks]
  (let [all-keys (set (keys map))
        other-keys (set/difference all-keys (set ks))] 
    (apply dissoc map other-keys))); TODO: To utils

(defn mk-flowElement [{:keys [resourceId stencil, properties]} id-mapping] 
  (xml/element (str "bpmn:" (json-type->bpmn-type (:id stencil))) (assoc (dissoc-except properties :name) 
                                                                         :id (id-mapping resourceId)) ""))

(defn mk-lane [node id-mapping]
  (xml/element "bpmn:lane" {:id (id-mapping (:resourceId node)) :name (-> node :properties :name)} 
               (for [child (:childShapes node)
                     :let [id (id-mapping (:resourceId child))]]
                 (xml/element "bpmn:flowNodeRef" {} id))))

(defn mk-pool [node id-mapping]
  (assert (= "Pool" (-> node :stencil :id)))
  (xml/element "bpmn:process" {:id (id-mapping (:resourceId node))
                               :isExecutable "false"}
               (xml/element "bpmn:laneSet"
                            {}
                            (let [lanes (filter #(= (-> % :stencil :id) "Lane") (:childShapes node))]
                              (for [lane lanes] 
                                (mk-lane lane id-mapping))))))


(comment 
  "Test code:"
  (def models 
    (let [jsons-path "/home/josep/Repositories/inconsistenciesmodeltext/input/models/123/BPMN/"
          json-files (filter #(= "json" (extension %)) 
                             (file-seq (io/file jsons-path)))]
      (for [f json-files]
        (-> f slurp json/read-str walk/keywordize-keys))))

  (inspector/inspect-tree 
    (-> (io/file "/home/josep/Repositories/inconsistenciesmodeltext/input/models/123/BPMN/BicycleManufacturing/BicycleManufacturing_rev1.json")
        slurp
        json/read-str
        walk/keywordize-keys))

  (let [model (first models)
        id-mapping (make-id-mapping model)
        lane (-> model :childShapes first :childShapes first)
        pool (-> model :childShapes first)
        task (first (select (walker #(= "Task" (-> % :stencil :id))) model))]
    (xml/emit-str (mk-pool pool id-mapping)))

  (build-outgoing-graph (first models)))



