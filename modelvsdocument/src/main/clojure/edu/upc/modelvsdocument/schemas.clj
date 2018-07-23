(ns edu.upc.modelvsdocument.schemas
  (:use [edu.upc.modelvsdocument.utils])
  (:require [clojure.spec :as spec]))

(defn type-spec [t] #(= (type %) t))
(def file? (type-spec java.io.File))
(defn nat? [x] (and (int? x) (pos? x)))

(defmacro spec-fn [sym & preds]
  (assert (symbol? sym))
  (let [preds-spec (spec/cat :args (spec/* #(not= % '->)) :arrow (spec/? #(= % '->)) :ret (spec/? any?))
        {:keys [args ret] :as conformed} (spec/conform preds-spec preds)]
    (if (= conformed ::spec/invalid) 
      (throw (Exception. (str "spec-fn: Invalid syntax. " 
                              (spec/explain-str preds-spec preds)) )))
    `(spec/fdef ~sym :args (spec/cat ~@(mapcat vector 
                                               (map (fn [x] (keyword (str "arg-" x))) (range)) 
                                               (map (fn [arg] `(spec/spec ~arg)) args)))
                     :ret  ~(if ret ret `any?))))

; ===================
; FREELING STRUCTURES
; ===================

(spec/def ::id string?)
(spec/def ::ids (spec/* ::id))
(spec/def ::pos string?)
(spec/def ::wn string?)
(spec/def ::form string?)
(spec/def ::lemma string?)
(spec/def ::token (spec/keys :req-un [::pos ::id ::form] :opt-un [::wn ::lemma]))
(spec/def ::tokens (spec/* ::token))
(spec/def ::token-pair (spec/cat :first ::token :second ::token))

(spec/def ::role string?)
(spec/def ::head_token string?)
(spec/def ::argument (spec/keys :req-un [::role ::head_token]))
(spec/def ::arguments (spec/* ::argument))
(spec/def ::predicate (spec/keys :req-un [::id ::head_token] :opt-un [::arguments]))
(spec/def ::predicates (spec/* ::predicate))

(spec/def ::sentence (spec/keys :req-un [::tokens] :opt-un [::predicates]))
(spec/def ::sentences (spec/* ::sentence))
(spec/def ::paragraph (spec/keys :req-un [::sentences]))
(spec/def ::paragraphs (spec/* ::paragraph))

;(spec/def ::cputime string?)
;(spec/def ::wordcount string?)
(spec/def ::semantic_graph map?)

(spec/def ::text (spec/keys :req-un [::paragraphs ::semantic_graph]))
(spec/def ::analyzed-model (spec/keys :req-un [::paragraphs]))
(spec/def ::tokenized-text (spec/keys :req-un [::paragraphs]))

; ====================
; BPMN MODEL STRUCTURE
; ====================

; Id type defined only as an alias for string
(spec/def ::graph (spec/map-of any? set?))
(spec/def ::jbpt-process (type-spec org.jbpt.pm.ProcessModel))
(spec/def ::pool (spec/nilable (type-spec org.activiti.bpmn.model.Pool)))
(spec/def ::lane (type-spec org.activiti.bpmn.model.Lane))
(spec/def ::lanes (spec/* ::lane))
(spec/def ::task-to-lane (spec/nilable (spec/map-of string? string?)))
(spec/def ::process-id string?)
(spec/def ::start-event string?)
(spec/def ::end-events (spec/+ string?))
; TODO: Naming convention is a bit weird. Process is a Java object but processes an array of clojure objects...
(spec/def ::process (type-spec org.activiti.bpmn.model.Process))
(spec/def ::bpmn-process (spec/keys :req-un [::graph ::pool ::lanes ::task-to-lane ::process-id ::start-event ::end-events ::process]))
(spec/def ::processes (spec/* ::bpmn-process))
(spec/def ::model (type-spec org.activiti.bpmn.model.BpmnModel))
(spec/def ::message-flow (spec/spec (spec/cat :src ::id :dst ::id)))
(spec/def ::message-flows (spec/* ::message-flow))
(spec/def ::open-close-gateways (spec/* (spec/spec (spec/cat ::id (spec/? ::id)))))
(spec/def ::bpmn (spec/keys :req-un [::model ::processes ::message-flows ::open-close-gateways]))


(spec/def ::activiti-model (type-spec org.activiti.bpmn.model.BpmnModel))
(spec/def ::activiti-process (type-spec org.activiti.bpmn.model.Process))
(spec/def ::activiti-task (type-spec org.activiti.bpmn.model.Task))
(spec/def ::activiti-pool (type-spec org.activiti.bpmn.model.Pool))
(spec/def ::activiti-lane (type-spec org.activiti.bpmn.model.Lane))

; ==============
;  ORDER MATRIX
; ==============

; Message is message-flow spec

(defn enum [& values] (fn [x] (some #(= x %) values)))
(defn is-vector-and? [pred] (spec/and vector? pred))
(spec/def ::order-relation (enum :-> :<- :|| :!=))
(spec/def ::order-matrix-row (is-vector-and? (spec/+ ::order-relation)))
(spec/def ::order-matrix-column ::order-matrix-row)
(spec/def ::order-matrix (is-vector-and? (spec/+ ::order-matrix-row)))

(spec/def ::cost-matrix-row (is-vector-and? (spec/+ number?)))
(spec/def ::cost-matrix (is-vector-and? (spec/+ ::cost-matrix-row)))
(spec/def ::cost-matrix-int-row (is-vector-and? (spec/+ int?)))
(spec/def ::cost-matrix-int (is-vector-and? (spec/+ ::cost-matrix-int-row)))

(spec/def ::ftype keyword?)
(spec/def ::feature (spec/keys :req-un [::ftype :unq/argument :unq/weight]))
(spec/def ::feature-vector (spec/* ::feature))

(spec/def ::wordnet-id (spec/and string? #(re-matches #"\d\d\d\d\d\d\d\d-[a-z]" %)))

;========
; TF-IDF
;========
(spec/def ::tfs (spec/map-of string? number?))
(spec/def ::idfs (spec/map-of string? number?))
(spec/def ::tf-idf-table (spec/keys :req [::tfs ::idfs]))
