(ns edu.upc.modelvsdocument.json-bpmn-parser
  (:import [org.activiti.bpmn BpmnAutoLayout]
           [org.activiti.bpmn.converter BpmnXMLConverter])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [clojure.set :as set]
            [clojure.spec :as spec]
            [clojure.java.io :as io]
            [clojure.spec.test :as stest]
            [clojure.inspector :as inspector :refer [inspect]]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.script.graph-reduction :as graph :refer [incoming outgoing]]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.textserver :as textserver]
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
   "CollapsedSubprocess" "task"
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

(defn build-flow-graph [main-node]
  (let [nodes (json-tree-seq main-node)
        initial-graph {}]
    (reduce 
      (fn [graph, node]
        (-> graph 
            (assoc (:resourceId node) (set (map :resourceId (:outgoing node)))))) 
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

(defn mk-flowElement [{:keys [resourceId stencil, properties]} incoming-ids outgoing-ids id-mapping] 
  (apply xml/element (str "bpmn:" (json-type->bpmn-type (:id stencil))) 
               (assoc (dissoc-except properties :name) 
                      :id (id-mapping resourceId)) 
               (concat
                 (map #(xml/element "bpmn:incoming" {} (id-mapping %)) incoming-ids)
                 (map #(xml/element "bpmn:outgoing" {} (id-mapping %)) outgoing-ids))))

(defn mk-lane [node id-mapping]
  (xml/element "bpmn:lane" {:id (id-mapping (:resourceId node)) :name (-> node :properties :name)} 
               (for [child (:childShapes node)
                     :let [id (id-mapping (:resourceId child))]]
                 (xml/element "bpmn:flowNodeRef" {} id))))

(defn lane? [node]
  (= (-> node :stencil :id) "Lane"))

(defn not-lane? [node]
  (not (lane? node)))

(defn pool? [node]
  (= (-> node :stencil :id) "Pool"))

(defn sequence-flow? [node]
  (= (-> node :stencil :id) "SequenceFlow"))

(defn message-flow? [node]
  (= (-> node :stencil :id) "MessageFlow"))

(defn extract-edges [edge-nodes flow-graph]
  (into {}  
        (for [node edge-nodes
              :let [id (:resourceId node)
                    in (incoming flow-graph id)
                    out (outgoing flow-graph id)
                    __ (assert (and (= 1 (count in)) (= 1 (count out))))]]
          [id [(first in) (first out)]])))

(defn mk-sequenceFlow [id-mapping id source-ref target-ref]
  (xml/element "bpmn:sequenceFlow" {:id (id-mapping id)
                                    :sourceRef (id-mapping source-ref) 
                                    :targetRef (id-mapping target-ref)}))

(defn mk-pool [node id-mapping flow-graph sequence-flow-edges]
  (assert (= "Pool" (-> node :stencil :id)))
  (let [lanes (filter lane? (:childShapes node))] 
    (apply xml/element "bpmn:process" {:id (id-mapping (:resourceId node))
                                       :isExecutable "false"}
           ; Lane Set
           (apply xml/element "bpmn:laneSet"
                  {}
                  (for [lane lanes] (mk-lane lane id-mapping)))
           ; SequenceFlows
           (for [[id [source-ref target-ref]] sequence-flow-edges]
             (mk-sequenceFlow id-mapping id source-ref target-ref))
           ; Tasks
           (for [flow-element (mapcat :childShapes lanes)
                 :let [incoming-ids (incoming flow-graph (:resourceId flow-element))
                       outgoing-ids (outgoing flow-graph (:resourceId flow-element))]]
             (mk-flowElement flow-element incoming-ids outgoing-ids id-mapping)))))

(defn mk-collaboration [pools message-flows messages id-mapping]
  ;NOTE: I assumed there should be only one Collaboration node, hence the hardcoded ID
  (let [id-generator (make-id-generator)] 
    (apply xml/element "bpmn:collaboration" {:id "Collaboration_1"}
           (concat 
             (for [pool pools
                   :let [participant-id (id-generator "Participant")
                         target-id (id-mapping (:resourceId pool))
                         pool-name (-> pool :properties :name)]]
               (xml/element "bpmn:participant" {:id participant-id, :name pool-name, :processRef target-id}))
             (for [message-flow message-flows
                   :let [id (id-mapping (:resourceId message-flow))
                         [source-ref target-ref] (map id-mapping (messages (:resourceId message-flow)))]]
               (xml/element "bpmn:messageFlow" {:id id :sourceRef source-ref :targetRef target-ref}))))))

(defn mk-model [model]
  (assert (= "BPMNDiagram" (-> model :stencil :id)))
  (let [id-mapping (make-id-mapping model)
        pools (filter pool? (:childShapes model))
        pool-ids (map #(id-mapping (:resourceId %)) pools)
        sequence-flows (filter sequence-flow? (:childShapes model))
        message-flows (filter message-flow? (:childShapes model))

        raw-flow-graph (build-flow-graph model)
        message-flow-id? (set (map :resourceId message-flows)) 
        sequence-flow-id? (set (map :resourceId sequence-flows)) 
        task-graph (graph/filter-graph raw-flow-graph message-flow-id?)
        __ (def test-task-graph (transform (walker string?) id-mapping (graph/reduce-graph task-graph sequence-flow-id?))) ;TODO
        message-flow-edges (extract-edges message-flows raw-flow-graph)
        sequence-flow-edges (extract-edges sequence-flows raw-flow-graph)] 
    (apply xml/element "bpmn:definitions" 
           {:xmlns:bpmn "http://www.omg.org/spec/BPMN/20100524/MODEL"
            :xmlns:bpmndi "htpp://www.omg.org/spec/BPMN/20100524/DI"
            :xmlns:di "http://www.omg.org/spec/DD/20100524/DI"
            :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
            :xmlns:dc "http://www.omg.org/spec/DD/20100524/DC"
            :id "Definitions_1"
            :targetNamespace "http://bpmn.io/schema/bpmn"} 
           (mk-collaboration pools message-flows message-flow-edges id-mapping)
           (map #(mk-pool % id-mapping task-graph sequence-flow-edges) pools))))

(comment 
  "Convert the files in batch"
  (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"
        benchmark-files (file-seq (io/file benchmark-path))
        json-files (filter #(= "json" (extension %)) benchmark-files)]
    (pmap 
      (fn [json-file]
        (let [bpmn-xml (json->xml json-file)]
              (spit (str benchmark-path (strip-extension json-file) ".bpmn") bpmn-xml)))
      json-files)))


(comment 
  "Test code:"

  (defn read-model-from-string [s]
    (spit "/var/tmp/model.bpmn" s)
    (bpmn/read-model "/var/tmp/model.bpmn"))

  (defn json->xml [path]
    (-> path io/file slurp json/read-str walk/keywordize-keys mk-model xml/indent-str))


  (defn map-subset? [m1 m2]
    (and (every?  (set  (keys m1))  (keys m2))  ;; subset on keys
         (every? #(= (m1 %) (m2 %))  (keys m2))) )

  (let [jsons-path "/home/josep/Repositories/inconsistenciesmodeltext/input/models/123/BPMN/"
        json-files (sort-by #(.getName %) (filter #(= "json" (extension %)) 
                           (file-seq (io/file jsons-path))))]
    (doseq [json json-files
            :let [model (-> json slurp json/read-str walk/keywordize-keys)]]
      (print (.getName json) " ... ")
      (try (do (spit "/home/josep/test-model.bpmn" (xml/indent-str (mk-model model)))
               (let [G1 test-task-graph
                     G2 (apply merge-with into (map :graph (:processes (bpmn/build-model (bpmn/read-model "/home/josep/test-model.bpmn")))))]
                 (if (map-subset? G1 G2) (println "[OK]") 
                   (do (println "[NO]")
                       G1
                       G2))))
           (catch Exception e (println "[[NO!]]")))))
  (inspector/inspect-tree (-> "/home/josep/Repositories/inconsistenciesmodeltext/input/models/123/BPMN/Model6-1/Model6-1_rev1.json"
                              io/file
                              slurp
                              json/read-str
                              walk/keywordize-keys))
  (spit "/home/josep/test-model.bpmn" (-> "/home/josep/Repositories/inconsistenciesmodeltext/input/models/123/BPMN/Model6-1/Model6-1_rev1.json"
      io/file 
      slurp
      json/read-str
      walk/keywordize-keys
      mk-model
      xml/indent-str))


  (map #(.getName %) (bpmn/all-tasks (bpmn/build-model (bpmn/read-model "/home/josep/test-model.bpmn"))))
  
  )



