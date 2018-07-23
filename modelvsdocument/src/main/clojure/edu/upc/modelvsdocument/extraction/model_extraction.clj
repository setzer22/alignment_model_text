(ns edu.upc.modelvsdocument.extraction.model-extraction
  (:gen-class)
  (:require [clojure.spec :as spec]
            [clojure.string :as string]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.extraction.macros :refer [model-extractor]]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.sorter.model-sorter :as modelsorter]
            [clj-http.client :as client]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.tf-idf :as idf]
            [clojure.data.json :as json]
            [clj-xpath.core :as xpath]
            [edu.upc.modelvsdocument.wordnet :as wn]
            [edu.upc.modelvsdocument.alignable :as a])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.extraction.feature]
        [edu.upc.modelvsdocument.bpmn]
        [edu.upc.modelvsdocument.extraction.common]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.config])
  (:import [org.activiti.bpmn.model Task Gateway Event ExclusiveGateway ParallelGateway]))

(comment
  LABEL PARSER INTEGRATION TEST


  ;;TODO:
  ;; [X] Fix computation of TF-IDF table
  ;; [X] Change way in which sentence analysis is accessed:
  ;;      - [X] Instead of  (get-in [:analyzed-labels id :sentences] ...), now we have
  ;;          always a single sentence.
  ;;      - [X] Instead of accessing the predicates, now there is the field
  ;;          :label-analysis inside each analyzed label
  ;; [ ] The field name :label-analysis is not really good. Change it to :label-predicates
  ;; [X] Test everything works

  (def --model (edu.upc.modelvsdocument.bpmn-alignable/analyze
                "/home/josep/ModelsBpmn/Model1-2.bpmn"
                ""))

  ("Task_0ssja9k" "Task_0j4npun" "Task_1qrcrh7" "Task_0w3czw9" "Task_17x20z3" "Task_0420rk1" "Task_02qz4bj" "Participant_1cb1ihb" "IntermediateCatchEvent_1pyp6o5" "Lane_0nocd2c" "Task_0oz8hjr" "Participant_1fg9rvj" "Task_1gst9f9" "ExclusiveGateway_0ajb73w" "Task_0qrzi4y" "Task_0oih0sg" "Task_0wrm9ya" "Participant_1r8k6hn" "Task_14zetua" "Task_106edn4" "IntermediateCatchEvent_1dgnju9" "Lane_0a6rbai" "Task_064spq4" "Lane_0zpnyy9") 

  (:analyzed-labels --model)

  (def --tf-idf-table (idf/tf-idf-table (select [:analyzed-labels ALL LAST] --model)))

  (extract-features --model)


  END)


(def extractor-functions
  "The list of extractor functions. Gets automatically updated by the task-extractor macro"
  (atom {}))

(model-extractor extract-forms [Task, Event]
  "Docstring"
  [task-id, model, tf-idf-table]
  (let [sentence (get-in model [:analyzed-labels task-id])]
    (let [forms (select [TOKENS :lemma] sentence)]
      (map #(has-form [%] (idf/get-tf-idf tf-idf-table % sentence)) forms))))

;TODO: When a synset has two parent synsets the multiplier should be the same
;      for both parents. This is not correctly handled as of right now. The case
;      is so uncommon that it doesn't matter much.
(model-extractor extract-synonyms-and-hypernyms [Task, Event]
  [task-id, model, tf-idf-table]
  (let [sentence (get-in model [:analyzed-labels task-id])]
    (let [tokens             (select [TOKENS] sentence)
          synsets-and-weights (map
                               (fn [token]
                                 [(senses-of-token token) (idf/get-tf-idf tf-idf-table token sentence)])
                               tokens)
          synsets-and-weights (as-> synsets-and-weights $$
                                (mapcat #(map vector (first %) (repeat (second %))) $$))]
      (make-synset-and-hypernym-features synsets-and-weights))))

(model-extractor extract-xor [ExclusiveGateway] [_, _, _] [(has-discourse-marker ["conditional"])])
(model-extractor extract-and [ParallelGateway] [_, _, _] [(has-discourse-marker ["parallel"])])

(model-extractor extract-lemmas [Task, Event, Gateway]
  [task-id, model, tf-idf-table]
  (let [sentence (get-in model [:analyzed-labels task-id])]
    (let [analyzed-text (select [TOKENS] sentence)]
      (map #(has-lemma [(:pos %) (:lemma %)] (idf/get-tf-idf tf-idf-table % sentence)) analyzed-text))))

(model-extractor extract-actions [Task, Event]
  [task-id, model, tf-idf-table]
  (let [sentence (get-in model [:analyzed-labels task-id])
        action-id-s (get-in sentence [:label_analysis :predicate :head])
        #_action-token #_(token-of-id sentence action-id)]
    (if (coll? action-id-s)
      ;; Double action case, multiple heads
      (let [action-tokens (remove nil? (map #(token-of-id sentence %) action-id-s))]
        (map
         (fn [action-token]
           (has-action [(:lemma action-token)]
                       (idf/get-tf-idf tf-idf-table action-token sentence)))
         action-tokens))
      ;; Normal case, single head
      (let [action-token (token-of-id sentence action-id-s)]
        (when (and action-id-s (non-stopword? action-token))
          [(has-action [(:lemma action-token)]
                       (idf/get-tf-idf tf-idf-table action-token sentence))])))))

(model-extractor extract-out-branches [Gateway]
  [id, model, tf-idf-table]
  (->> (a/get-element model id)
       .getOutgoingFlows
       (map #(.getId %))
       (map (fn [id] (get-in model [:analyzed-labels id])))
       (mapcat (fn [sentence]
                 (map (fn [token] (has-lemma [(:pos token) (:lemma token)] (idf/get-tf-idf tf-idf-table token sentence)))
                      (select [:tokens ALL non-stopword?] sentence))))))

(spec-fn extract-agents-lane-or-pool ::t/bpmn ::t/id -> ::t/feature-vector)
(defn- extract-agents-lane-or-pool [model lane-or-pool-id tf-idf-table]
  (let [sentence (get-in model [:analyzed-labels lane-or-pool-id])]
    (when sentence
      (let [;;TODO: XXX: Until we have a proper grammar for swimpools/lanes, we will
            ;; consider the head to be the last token as a heuristic
            head (last (select [TOKENS] sentence))
            tokens (select [TOKENS] sentence)

            ;;__ (def --sentence sentence)
            ;;__ (def --head head)
            ;;__ (def --tokens tokens)

            head-feature (agent-head [(:lemma head) (:pos head)]
                                     (idf/get-tf-idf tf-idf-table head sentence))
            token-features (map
                            (fn [token]
                              (in-agent [(:lemma token) (:pos token)]
                                        (idf/get-tf-idf tf-idf-table token sentence)))
                            tokens)]
        (concat [head-feature] token-features)))))

;; TODO: I'll leave this undone because I've yet to find a task text that contains an agent.
(spec-fn extract-agents-task ::t/bpmn ::t/id -> ::t/feature-vector)
(defn- extract-agents-task [model task-id]
  nil)

(model-extractor extract-agents [Task, Event]
  [task-id, model, tf-idf-table]
  (let [sentence (get-in model [:analyzed-labels task-id])]
    (let [task-sentence sentence
          lane-id        (let [t (bpmn/lane-of-task task-id model)] (when-not (nil? t) (.getId t)))
          pool-id        (let [p (bpmn/pool-of-task task-id model)] (when-not (nil? p) (.getId p)))
          actions        (distinct (select [:tokens ALL (is? :pos "verb")] task-sentence))]
      (concat
       ;; Agents in lane text
       (when-not (nil? pool-id)
         (extract-agents-lane-or-pool model pool-id tf-idf-table))
       ;; Agents in pool text
       (when-not (nil? lane-id)
         (extract-agents-lane-or-pool model lane-id tf-idf-table))
       ;; Agents in task text
       (extract-agents-task model task-id)))))

(model-extractor extract-patients [Task, Event]
  [task-id, model, tf-idf-table]
  (let [sentence (get-in model [:analyzed-labels task-id])]
    (concat
     ;; patient-head means "the head token of the agent is..."
     (map (fn head-feature-list [tk] (patient-head [(:lemma tk) (:pos tk)]
                                                   (idf/get-tf-idf tf-idf-table tk sentence)))
          (remove nil?
                  (filter non-stopword?
                          [(->> sentence :label_analysis :object :head (token-of-id sentence))])))
     ;; in-agent means "the agent contains token..."
     (map (fn token-feature-list [tk] (in-patient [(:lemma tk) (:pos tk)]
                                                  (idf/get-tf-idf tf-idf-table tk sentence)))
          (remove nil? (filter non-stopword?
                               (map #(token-of-id sentence %)
                                    (->> sentence :label_analysis :object :words))))))))

;; TODO:
;; - named-gateway? is now different.
;; - Maybe add also the branches, not just the gateway? [FUTURE]
;; - Change the way in which we retrieve the tokens.
(model-extractor extract-conditionals [Task, Event]
  [task-id, model,  _]
  (let [process        (process-of-task task-id model)
        named-gateway? (set (filter (:analyzed-labels model) (bpmn/all-gateway-ids model)))

        successors   (filter named-gateway? (remove nil? (get (process :graph) task-id)))
        predecessors (filter named-gateway? (remove nil? (get (reverse-graph (process :graph)) task-id)))

        successor-sentences   (map #(get-in model [:analyzed-labels %]) successors)
        predecessor-sentences (map #(get-in model [:analyzed-labels %]) predecessors)

        successor-tokens   (select [ALL TOKENS] successor-sentences)
        predecessor-tokens (select [ALL TOKENS] predecessor-sentences)]
    (concat
     (map (fn [t] (lemma-conditional-follow [(:lemma t) (:pos t)]))
          predecessor-tokens)
     (map (fn [t] (lemma-conditional-pred [(:lemma t) (:pos t)]))
          successor-tokens))))

(spec-fn extract-features ::t/bpmn ::t/analyzed-model -> ::t/feature-vector)
(defn extract-features [model]
  (let [tf-idf-table (idf/tf-idf-table (select [:analyzed-labels ALL LAST] model))
        alignables (a/get-all-alignables model)]
    (mapify
     (map
      (fn [alignable]
        (let [extractor-functions (mapcat second (filter (fn [[type fns]] (instance? type alignable)) @extractor-functions))]
          [alignable
           (into []
                 (distinct-features
                  (remove nil?
                          (flatten (map #(% (a/id alignable) model tf-idf-table)
                                        (vals extractor-functions))))))]))
      alignables))))
