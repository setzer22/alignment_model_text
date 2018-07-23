(ns edu.upc.modelvsdocument.bpmn-alignable
  (:require [edu.upc.modelvsdocument.alignable :as alignable]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.config :refer [config]]
            [edu.upc.modelvsdocument.bpmn-analysis :as analysis]
            [edu.upc.modelvsdocument.bpmn-analysis-old :as analysis-old]
            [edu.upc.modelvsdocument.config :as globals])
  (:import [org.activiti.bpmn.model BpmnModel FlowNode Task]))

(defrecord Bpmn [model processes message-flows]
  alignable/Context
  (get-element [this id] (.getFlowElement (:model this) id))
  (get-all-alignables [this]
    (filter (if (globals/config :enable-anchors)
              (partial bpmn/alignable? this)
              bpmn/task?)
     (bpmn/all-flow-elements this)))
  (get-output-alignables [this] (filter (partial bpmn/task-instance? this) (bpmn/all-flow-elements this))))

(extend-type FlowNode
  alignable/Alignable
  (label
    ([this model] (alignable/label this))
    ([this] (if (empty? (.getName this)) (str "Unnamed (" (.getId this) ")")
                (.getName this))))
  (id [this] (.getId this)))

;;TODO: @Refactor Rename flow-element? to flow-node? !
(defn flow-element? [x]
  (instance? FlowNode x))

(defn task? [x]
  (instance? Task x))

(defn analyze
  "Pre: model can be either a path to a BPMN file or an Activiti
   BpmnModel instance.
  Returns: A wrapped model enriched with text analysis information"
  ([model]
   (cond (string? model) (map->Bpmn (bpmn/build-model (bpmn/read-model model)))
         (instance? BpmnModel model) (map->Bpmn (bpmn/build-model model))))
  ([model text]
   (map->Bpmn (if (config :use-old-label-parser)
                (analysis-old/analyze-model-text (analyze model) text)
                (analysis/analyze-model-text (analyze model) text)))))

(defn get-debug-model
  ([model-name]
   (analyze (str (System/getProperty "user.home") "/ModelsBpmn/" model-name
                 (if (.endsWith ".bpmn" model-name) "" ".bpmn"))))
  ([model-name text]
   (analyze (str (System/getProperty "user.home") "/ModelsBpmn/" model-name
                 (if (.endsWith ".bpmn" model-name) "" ".bpmn"))
            text)))


