(ns edu.upc.modelvsdocument.extraction.model-extraction-old
  (:gen-class)
  (:require [clojure.spec :as spec]
            [clojure.string :as string]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.extraction.macros :as macros :refer [model-extractor]]
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

(doseq [t [:task-id :pool-id :lane-id]]
  (intern *ns* (symbol (str "sentence-with-" (name t)))
          (fn sentence-with-id [model id]
            (select-one [ALL-SENTENCES #(= id (get % t))] model))))

;;(spec-fn extract-agents-token-list ::t/tokens -> ::t/feature-vector)
;;(defn extract-agents-token-list [tokens tf-idf-table]
;;(map #(in-agent [(:lemma %) (:pos %)]) tokens))

(spec-fn extract-agents-lane-or-pool ::t/bpmn ::t/id -> ::t/feature-vector)
(defn extract-agents-lane-or-pool [model lane-or-pool-id tf-idf-table]
  (when lane-or-pool-id
    (let [sentences (get-in model [:analyzed-labels lane-or-pool-id :sentences])
          heads-features (map
                          (fn [sentence]
                            (let [id (select-one [:dependencies FIRST :token] sentence)
                                  token (select-one [:tokens ALL #(= id (:id %))] sentence)]
                              (if (non-stopword? token)
                                (agent-head [(:lemma token) (:pos token)] (idf/get-tf-idf tf-idf-table token sentence))
                                [])))
                          sentences)
          token-features (mapcat
                          (fn [sentence]
                            (let [tokens (filter non-stopword? (select [TOKENS] sentence))]
                              (map #(in-agent [(:lemma %) (:pos %)]  (idf/get-tf-idf tf-idf-table % sentence)) tokens)))
                          sentences)]
      (concat heads-features token-features))))

(comment
  BEGIN

  (def --text (edu.upc.modelvsdocument.text/analyze :text (slurp "/home/josep/ModelsBpmn/Zoo.txt")
                                                    :lang "en"))

  (def --model (edu.upc.modelvsdocument.bpmn-alignable/analyze "/home/josep/ModelsBpmn/Zoo.bpmn"
                                                               (slurp "/home/josep/ModelsBpmn/Zoo.txt")))

  (def --sentence (first (get-in --model [:analyzed-labels "Participant_1cb1ihb" :sentences])))

  (def --tf-idf-table (idf/tf-idf-table (select [:analyzed-labels ALL LAST :sentences ALL] --model)))

  ("Task_0ssja9k"  "Task_0j4npun""Task_1qrcrh7" "Task_0w3czw9" "ParallelGateway_0g0ww2s" "Task_17x20z3" "Task_0420rk1" "Task_02qz4bj" "Participant_1cb1ihb" "ParallelGateway_0jj7fu7" "IntermediateCatchEvent_1pyp6o5" "ParallelGateway_1oafzb2" "Lane_0nocd2c" "Task_0oz8hjr" "ExclusiveGateway_06m442q" "Participant_1fg9rvj" "Task_1gst9f9" "ExclusiveGateway_0ajb73w" "Task_0qrzi4y" "ParallelGateway_0tw525m" "Task_0oih0sg" "Task_0wrm9ya" "Participant_1r8k6hn" "Task_14zetua" "Task_106edn4" "IntermediateCatchEvent_1dgnju9" "Lane_0a6rbai" "Task_064spq4" "Lane_0zpnyy9")

  (extract-patients  "Task_1qrcrh7" --model --tf-idf-table)

  (relevant-predicates-in (first (get-in --model [:analyzed-labels "Task_1qrcrh7" :sentences])))

  (heads-of-role-type
   (first (get-in --model [:analyzed-labels "Task_1qrcrh7" :sentences]))
   "A1")

  (tokens-of-role-type
   (first (get-in --model [:analyzed-labels "Task_1qrcrh7" :sentences]))
   "A1")

  END)

;; TODO: I'll leave this undone because I've yet to find a task text that contains an agent.
(spec-fn extract-agents-task ::t/bpmn ::t/id -> ::t/feature-vector)
(defn extract-agents-task [model task-id]
  nil)

;; =============== EXTRACTORS ===============

;; TODO: General Stuff:
;; - We should distinguish model and enriched model in the schemas.
;; - Some documentation is in order

(def extractor-functions
  "The list of extractor functions. Gets automatically updated by the task-extractor macro"
  (atom {}))


(model-extractor extract-forms [Task]
  "Docstring"
  [task-id, model, tf-idf-table]
  (let [sentences (get-in model [:analyzed-labels task-id :sentences])]
    (mapcat
     (fn [sentence]
       (let [forms (select [TOKENS :lemma] sentence)]
         (map #(has-form [%] (idf/get-tf-idf tf-idf-table % sentence)) forms)))
     sentences)))

;TODO: When a synset has two parent synsets the multiplier should be the same
;      for both parents. This is not correctly handled as of right now. The case
;      is so uncommon that it doesn't matter much.
(model-extractor extract-synonyms-and-hypernyms [Task]
  [task-id, model, tf-idf-table]
  (let [sentences (get-in model [:analyzed-labels task-id :sentences])]
    (mapcat
     (fn [sentence]
       (let [tokens             (select [TOKENS] sentence)
             synsets-and-weights (map
                                  (fn [token]
                                    [(senses-of-token token) (idf/get-tf-idf tf-idf-table token sentence)])
                                  tokens)
             synsets-and-weights (as-> synsets-and-weights $$
                                   (mapcat #(map vector (first %) (repeat (second %))) $$))]
         (make-synset-and-hypernym-features synsets-and-weights)))
     sentences)))

(model-extractor extract-xor [ExclusiveGateway] [_, _, _] [(has-discourse-marker ["conditional"])])
(model-extractor extract-and [ParallelGateway] [_, _, _] [(has-discourse-marker ["parallel"])])

(model-extractor extract-lemmas [Task]
                 [task-id, model, tf-idf-table]
                 (let [sentences (get-in model [:analyzed-labels task-id :sentences])]
                   (mapcat (fn [sentence]
                             (let [analyzed-text (select [TOKENS] sentence)]
                               (map #(has-lemma [(:pos %) (:lemma %)] (idf/get-tf-idf tf-idf-table % sentence)) analyzed-text)))
                           sentences)))

;; TODO: Use tf-idf
(model-extractor extract-actions [Task]
  [task-id, model, _]
  (let [sentences (get-in model [:analyzed-labels task-id :sentences])]
    (mapcat
     (fn [sentence]
       (let [analyzed-text (select [TOKENS #(= (:pos %) "verb")] sentence)]
         (map #(has-action [(:lemma %)]) analyzed-text)))
     sentences)))

(model-extractor extract-agents [Task]
  [task-id, model, tf-idf-table]
  (let [sentences (get-in model [:analyzed-labels task-id :sentences])]
    (mapcat
     (fn [sentence]
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
          (extract-agents-task model task-id))))
     sentences)))

(model-extractor extract-patients [Task]
  [task-id, model, tf-idf-table]
  (let [sentences (get-in model [:analyzed-labels task-id :sentences])]
    (mapcat
     (fn [sentence]
       (concat
        ;; patient-head means "the head token of the agent is..."
        (map (fn head-feature-list [tk] (patient-head [(:lemma tk) (:pos tk)]
                                                      (idf/get-tf-idf tf-idf-table tk sentence)))
             (heads-of-role-type sentence "A1"))
        ;; in-agent means "the agent contains token..."
        (map (fn token-feature-list [tk] (in-patient [(:lemma tk) (:pos tk)]
                                                     (idf/get-tf-idf tf-idf-table tk sentence)))
             (tokens-of-role-type sentence "A1"))))
     sentences)))

;; TODO:
;; - named-gateway? is now different.
;; - Maybe add also the branches, not just the gateway? [FUTURE]
;; - Change the way in which we retrieve the tokens.
(model-extractor extract-conditionals [Task]
  [task-id, model,  _]
  (let [process        (process-of-task task-id model)
        named-gateway? (set (filter (:analyzed-labels model) (bpmn/all-gateway-ids model)))

        successors   (filter named-gateway? (remove nil? (get (process :graph) task-id)))
        predecessors (filter named-gateway? (remove nil? (get (reverse-graph (process :graph)) task-id)))

        successor-sentences   (map #(get-in model [:analyzed-labels %]) successors)
        predecessor-sentences (map #(get-in model [:analyzed-labels %]) predecessors)

        successor-tokens   (select [ALL :sentences ALL TOKENS] successor-sentences)
        predecessor-tokens (select [ALL :sentences ALL TOKENS] predecessor-sentences)]
    (concat
     (map (fn [t] (lemma-conditional-follow [(:lemma t) (:pos t)]))
          predecessor-tokens)
     (map (fn [t] (lemma-conditional-pred [(:lemma t) (:pos t)]))
          successor-tokens))))

(spec-fn extract-features ::t/bpmn ::t/analyzed-model -> ::t/feature-vector)
(defn extract-features [model]
  (let [tf-idf-table (idf/tf-idf-table (select [:analyzed-labels ALL LAST :sentences ALL] model))
        alignables (a/get-all-alignables model)]
    (mapify
     (map
      (fn [alignable]
        (let [extractor-functions (mapcat second (filter (fn [[type fns]] (instance? type alignable)) @extractor-functions))]
          [alignable
           (into []
                 (distinct-features
                  (flatten (map #(% (a/id alignable) model tf-idf-table)
                                (vals extractor-functions)))))]))
      alignables))))

(comment
  "TEST"

  (map #(.getName %) (bpmn/all-lanes (bpmn/construct-model "/home/josep/ModelsBpmn/Dispatch-of-goods.bpmn")))

  (extract-features
   (edu.upc.modelvsdocument.bpmn-alignable/get-debug-model
    "Dispatch-of-goods"
    (slurp "/home/josep/ModelsBpmn/Dispatch-of-goods.txt")))

  (extract-agents
   "Task_3"
   --model
   (idf/tf-idf-table (select [:analyzed-labels ALL LAST :sentences ALL] --model)))

  (extract-conditionals
   "Task_3"
   --model
   (idf/tf-idf-table (select [:analyzed-labels ALL LAST :sentences ALL] --model)))

  (def --model (edu.upc.modelvsdocument.bpmn-analysis/analyze-model-text (bpmn/construct-model "/home/josep/ModelsBpmn/BicycleManufacturing.bpmn")
                                                                         (slurp "/home/josep/ModelsBpmn/BicycleManufacturing.txt")))

  (bpmn/all-task-ids --model)

  --model


  (let [model (bpmn/construct-model "/home/josep/ModelsBpmn/ClaimsCreation.bpmn")
        text  (slurp "/home/josep/ModelsBpmn/ClaimsCreation.txt")
        model (edu.upc.modelvsdocument.bpmn-analysis/analyze-model-text model text)]
    (= (extract-agents-lane model "Task_2" "Lane_0" [{:lemma "do"} {:lemma "go"}])
       (extract-agents-pool model "Task_2" "Lane_0" [{:lemma "do"} {:lemma "go"}])))

  )
