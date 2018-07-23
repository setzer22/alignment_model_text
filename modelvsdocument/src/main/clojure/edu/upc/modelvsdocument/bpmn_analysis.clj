(ns edu.upc.modelvsdocument.bpmn-analysis
  (:require [com.rpl.specter :refer :all]
            [clojure.pprint :refer :all]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [clojure.string :as string]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.utils :as utils :refer :all]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.nlp4bpm-commons.freeling-api :as fl] ;;TODO: If this is OK, why not remove "textserver" layer from this module?
            [edu.upc.modelvsdocument.bpmn :as bpmn]))


(spec-fn analyze-model-text ::t/bpmn string?)
(defn analyze-model-text [model, context-text]
  (let [;; TASK PARSING
        task-ids (into [] (bpmn/all-task-ids model))
        task-labels (mapv #(bpmn/get-label model %) task-ids)
        nil-label-indices (indices-such-that nil? task-labels)
        task-ids (remove-indices task-ids nil-label-indices)
        task-labels (remove-indices task-labels nil-label-indices)

        parsed-task-labels (fl/analyze-labels-cached :labels task-labels :label-type "task")
        analyzed-task-labels (mapify (zip task-ids parsed-task-labels))

        start-id (inc (count analyzed-task-labels))

        ;; GENERIC LABEL PARSING
        ;; TODO: Add custom parsers for labels, gateways, events...
        other-ids    (remove (set task-ids) (bpmn/all-element-ids model))
        other-labels (map #(bpmn/get-label model %) other-ids)
        other-ids    (into [] (concat other-ids (bpmn/all-pool-ids model) (bpmn/all-lane-ids model) (bpmn/all-sequenceflow-ids model)))
        other-labels (into [] (concat other-labels (bpmn/all-pool-labels model) (bpmn/all-lane-labels model) (bpmn/all-sequenceflow-labels model)))
        nil-label-indices (indices-such-that nil? other-labels)
        other-ids (remove-indices other-ids nil-label-indices)
        other-labels (remove-indices other-labels nil-label-indices)

        parsed-other-labels (fl/analyze-labels-cached :labels other-labels
                                               :label-type "other" :start-id start-id)
        analyzed-other-labels (mapify (zip other-ids parsed-other-labels))]
    (assoc model :analyzed-labels (merge analyzed-task-labels analyzed-other-labels))))

#_(edu.upc.modelvsdocument.config/with-config {:enable-cache false}
    (analyze-model-text (bpmn/construct-model "/home/josep/ModelsBpmn/Zoo.bpmn") nil))

