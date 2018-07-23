(ns edu.upc.modelvsdocument.bpmn
  (:gen-class)
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.utils])
  (:require [edu.upc.modelvsdocument.alignable :as alignable]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.config :refer [config]]
            [edu.upc.modelvsdocument.back-edges :as back-edges]
            [clojure.set :as set])
  (:import [edu.upc.modelvsdocument.ModelManager]
           [org.activiti.bpmn.model Task Event Gateway StartEvent EndEvent UserTask SequenceFlow BpmnModel
            ParallelGateway ExclusiveGateway FlowElement]
           [org.jbpt.algo.graph DirectedGraphAlgorithms]
           [org.jbpt.algo.tree.tctree TCType]
           [org.jbpt.algo.tree.rpst RPST]))

;;TODO: Since the addition of SESE regions for open-close gateway detection, this namespace is mixing
;; ::t/bpmn meaning. Sometimes it means "the model without hte SESEs", while sometimes the SESEs are
;; required.

(defmacro with-element-or-id [model id & body]
  `(let [~id (if (string? ~id)
               (.getFlowElement ~model ~id)
               ~id)]
     ~@body))

(defn id? "Returns wether an object represents a bpmn identifier"
  [maybe-id]
  (spec/valid? ::t/id maybe-id))

(spec-fn all-flow-elements ::t/bpmn)
#_(defn all-flow-elements [{model :model}]
    (sort-by #(.getId %)
             (flatten (map
                       #(seq (.getFlowElements %))
                       (.getProcesses model)))))

(defn all-flow-elements [{model :model}]
  (sort-by #(.getId %)
           (->> (.getProcesses model)
                (map #(seq (.getFlowElements %)))
                (remove nil?)
                flatten)))

(defn alignable-instance? [{model :model} element]
  (with-element-or-id model element
    (or (instance? Task element)
        (and (instance? Event element)
             (.getName element)
             (not= (.getName element) ""))
        (instance? Gateway element))))

(defn alignable?
  "Returns true if the given element id corresponds to an alignable element in this model.
   Alignable elements are: Tasks, Labeled Events and some Gateways. The alignable gateways are considered
   in split/join pairs, but we consider only the split Gateway as alignable, and its pair is treated
   implicitly. Malformed gateways (neither split nor join) are not considered for aligning"
  [bpmn element]
  (with-element-or-id bpmn element
    (and (alignable-instance? bpmn element)
         (or (not (instance? Gateway element))
             (and (= (count (.getIncomingFlows element)) 1)
                  (>= (count (.getOutgoingFlows element)) 1))))))

(defn task-instance? [{model :model} element]
  (with-element-or-id model element
    (instance? Task element)))

(defn all-elements [model]
  (filter #(alignable-instance? model %) (all-flow-elements model)))

(defn all-element-ids [model]
  (map #(.getId %) (all-elements model)))

(defn get-label [{model :model} element]
  (with-element-or-id model element
    (.getName element)))

(spec-fn get-task ::t/id ::t/bpmn)
(defn get-task [id bpmn]
  (.getFlowElement (:model bpmn) id))
(defn get-element [id bpmn]
  (get-task id bpmn))

(defn task-name [task]
  (.getName task))

(defn task-name-of-id [bpmn id]
  (.getName (.getFlowElement (:model bpmn) id)))

(defn get-task-id [task]
  (.getId task))

(defn all-flow-element-ids [bpmn]
  (map #(.getId %) (all-flow-elements bpmn)))

; Some utility methods to access BPMN sturctures
(spec-fn all-tasks ::t/bpmn)
(defn all-tasks [model]
  (filter #(instance? Task %) (all-flow-elements model)))

(spec-fn all-gateways ::t/bpmn)
(defn all-gateways [model]
  (filter #(instance? Gateway %) (all-flow-elements model)))

(spec-fn all-events ::t/bpmn)
(defn all-events [model]
  (filter #(instance? Event %) (all-flow-elements model)))

(spec-fn all-gateway-ids ::t/bpmn)
(defn all-gateway-ids [model]
  (map #(.getId %) (all-gateways model)))

(spec-fn all-task-ids ::t/bpmn)
(defn all-task-ids [model]
  (map #(.getId %) (all-tasks model)))

(spec-fn all-event-ids ::t/bpmn)
(defn all-event-ids [model]
  (map #(.getId %) (all-events model)))

(spec-fn all-sequenceflows ::t/bpmn)
(defn all-sequenceflows [model]
  (mapcat (fn [process]
            (filter #(instance? SequenceFlow %) (.getFlowElements process)))
          (select [:processes ALL :process] model)))

(defn all-sequenceflow-ids [model]
  (map #(.getId %) (all-sequenceflows model)))

(defn all-sequenceflow-labels [model]
  (map #(.getName %) (all-sequenceflows model)))

(spec-fn all-lanes ::t/bpmn)
(defn all-lanes [model]
  (sort-by #(.getId %) (select [:processes ALL :lanes ALL] model)))

(spec-fn all-lane-ids ::t/bpmn)
(defn all-lane-ids [model]
  (map #(.getId %) (all-lanes model)))

(spec-fn all-lane-labels ::t/bpmn)
(defn all-lane-labels [model]
  (map #(.getName %) (all-lanes model)))

(spec-fn all-pools ::t/bpmn)
(defn all-pools [model]
  (sort-by #(.getId %) (select [:processes ALL :pool not-nil?] model)))

(spec-fn all-pool-ids ::t/bpmn)
(defn all-pool-ids [model]
  (map #(.getId %) (all-pools model)))

(spec-fn all-pool-labels ::t/bpmn)
(defn all-pool-labels [model]
  (map #(.getName %) (all-pools model)))

;(spec-fn task? ::t/bpmn ::t/id)
(defn task?
  ([activiti-task]
   (instance? Task activiti-task))
  ([model task-id]
   (instance? Task (.getFlowElement (:model model) task-id))))

;(spec-fn exclusive-gateway? ::t/bpmn ::t/id)
(defn exclusive-gateway? [{model :model} element]
  (with-element-or-id model element
    (instance? ExclusiveGateway element)))

;(spec-fn parallel-gateway? ::t/bpmn ::t/id)
(defn parallel-gateway? [{model :model} element]
  (instance? ParallelGateway element))

(defn gateway?
  ([element]
   (instance? Gateway element))
  ([{model :model} element]
   (with-element-or-id model element
     (instance? Gateway element))))

(defn split-gateway?
  ([element]
   (and (instance? Gateway element)
        (>= (count (.getOutgoingFlows element)) 1)
        (= (count (.getIncomingFlows element)) 1))))

(defn event?
  ([element]
   (instance? Event element))
  ([model element]
   (with-element-or-id model element
     (instance? Event element))))

(defn named-event?
  ([element]
   (and (instance? Event element)
        (not-nil? (.getName element))))
  ([model element]
   (with-element-or-id model element
     (and (instance? Event element)
          (not-nil? (.getName element))))))

;(spec-fn inclusive-gateway? ::t/bpmn ::t/id)
(defn inclusive-gateway? [{model :model} element]
  (with-element-or-id model element
    (instance? org.activiti.bpmn.model.InclusiveGateway element)))

(spec-fn process-of-task ::t/id ::t/bpmn)
(defn process-of-task [task-id model]
  (select-one [:processes ALL (selected? [:process #(.getFlowElement % task-id)])] model))

(spec-fn process-id-of-task ::t/id ::t/bpmn)
(defn process-id-of-task [task-id model]
  (.getId (:process (process-of-task task-id model))))

(spec-fn pool-of-task ::t/id ::t/bpmn)
(defn pool-of-task [task-id model]
  (:pool (process-of-task task-id model)))

(spec-fn lane-of-task ::t/id ::t/bpmn)
(defn lane-of-task [task-id, model]
  (let [process (process-of-task task-id model)] 
    (when-not (nil? (:task-to-lane process)) 
      (.getLane (:model model) (get (:task-to-lane process) task-id)))))

(defn map-all-to-k [k vals]
  (reduce #(assoc %1 %2 k) {} vals))

(defn conj-into [G edges]
  "Adds all edges in edges to g"
  (reduce (fn [G' [u v]] (assoc G' u (conj (get G' u #{}) v)))  G  edges))

(defn reverse-graph [G]
  (reduce (fn [G [v adj]] (conj-into G (map-all-to-k v adj)))
          {}
          G)) 


; Parsing and creating BPMN models
(declare build-graph)
(declare build-process)
(declare task-to-lane)

(defn flow-element? [model id] 
  (not (nil? (.getFlowElement model id))))


(defn read-model [path]
  "Reads the BPMN model in path"
  (edu.upc.modelvsdocument.ModelManager/readModel path))

(defn read-model-xml-string [model-string]
  (edu.upc.modelvsdocument.ModelManager/readModelFromString model-string))

(defn all-messageflow-ids [bpmn]
  (keys (.getMessageFlows (:model bpmn))))

(spec-fn get-message-flows ::t/activiti-model -> ::t/message-flows)
(defn get-message-flows [model]
  (as-> (seq (.values (.getMessageFlows model))) messages
    (map (fn [m] [(.getSourceRef m) (.getTargetRef m)]) messages)
    ;; We remove those message flows that go into anything that's not
    ;; a flow element (i.e. a Pool) because they're not interesting to us
    (filter
     (fn [[src, dst]] (and (flow-element? model src)
                           (flow-element? model dst)))
     messages)
    ;; We also remove flow messages that come from, or go to the same task
    ;; because that makes it impossible to sort tasks.
    (first (reduce
            (fn [[ok-messages, used] [src, dst :as message]]
              [(if (or (contains? used dst) (contains? used src))
                 ok-messages
                 (conj ok-messages message)),
               (set/union used #{src dst})])
            [[], #{}]
            messages))))

(spec-fn get-open-gateways
         (spec/keys :req-un [::t/activiti-model ::t/processes ::t/message-flows])
         (spec/coll-of ::t/id))
(defn get-open-gateways [model]
  (filter #(and (instance? Gateway %)
                (> 1 (count (.getIncomingFlows %)))
                (= 1 (count (.getOutgoingFlows %))))
          (mapcat
           (fn [process] (.getFlowElements process))
           (.getProcesses model))))

(defn find-seses [{:keys [jbpt-process] :as jbpt-model}]
  (let [graph-algorithms (DirectedGraphAlgorithms.)]
    (if (.isMultiTerminal graph-algorithms jbpt-process)
      (let [rpst (RPST. jbpt-process)
            rpst-nodes (.getRPSTNodes rpst TCType/BOND)]
        ;; NOTE: In some cases, .getExit or .getEntry will return null (for malformed models)
        ;;       we ignore such cases and gracefully degrade to only consider open gateways.
        (remove #(every? identity %)
                (map #(vector (some-> % .getEntry .getName) (some-> % .getExit .getName)) rpst-nodes))))))

(spec-fn build-model' ::t/activiti-model -> (spec/keys :req-un [::t/activiti-model ::t/processes ::t/message-flows]))
(defn build-model' [model]
  "Builds a BPMN model as a collection of processes."
  {:model  model

   :processes
   (->> (.getProcesses model)
        (map (partial build-process model))
        (remove nil?)
        (into []))

   :message-flows
   (get-message-flows model)})

(declare process->jbpt-process)

(spec-fn build-model ::t/activiti-model -> ::t/bpmn)
(defn build-model [activiti-model]
  (let [model' (build-model' activiti-model)
        open-gateways (set (get-open-gateways (:model model')))]
    (transform [:processes ALL]
               (fn [process]
                 (assoc process
                        :open-close-pairs
                        (find-seses (process->jbpt-process model' process))))
               model')))


(defn construct-model [path]
  (build-model (read-model path)))

(spec-fn build-process ::t/activiti-model ::t/activiti-process -> ::t/process)
(defn build-process [model process]
  "Builds a process structure suitable for analyzing the process in the BPMN model"
  (let [;NOTE: There is only one start event, if that is
                                        ;      not the case we fail
        start-node (let [start-nodes (filter #(instance? StartEvent %) (.getFlowElements process))]
                     (cond
                       (and (config :model-read-strict) (> (count start-nodes) 1))
                       ,,(throw (java.lang.IllegalArgumentException. "A process can't have more than one start event"))
                       (and (config :model-read-strict) (< (count start-nodes) 1))
                       ,,(throw (java.lang.IllegalArgumentException. "All processes must have a start event"))
                       :else                     (first start-nodes)))
        end-nodes (filter #_(instance? EndEvent %)
                          #(try-or (= (count (.getOutgoingFlows %)) 0) false)
                          (.getFlowElements process))
        __ (if (and (config :model-read-strict)
                    (empty? end-nodes))
             (throw (Exception. "All processes must have at least an end event")))
        sequenceflow-nodes (filter #(instance? SequenceFlow %) (.getFlowElements process))]
    {:graph (if (empty? end-nodes)
              (build-graph sequenceflow-nodes)
              (apply assoc
                     (build-graph sequenceflow-nodes)
                     (flatten (map (fn [e] [(.getId e) #{}]) end-nodes))))
     :pool (first (filter #(= (.getProcessRef %) (.getId process)) (.getPools model)))
     :lanes (into [] (.getLanes process))
     :task-to-lane (task-to-lane process)
     :process-id (.getId process)
     :start-event (when-not (nil? start-node) (.getId start-node))
     :end-events (map #(.getId %) end-nodes)
     :process process}))

(spec-fn task-to-lane ::t/activiti-process)
(defn task-to-lane [process]
  (when (not (empty? (.getLanes process)))
    (let [lane-ids (map #(.getId %) (.getLanes process))
          nodes-by-lane (map #(.getFlowReferences %) (.getLanes process))]
      (apply merge (map
                    (fn [id nodes] (into {} (map vector nodes (repeat id))))
                    lane-ids
                    nodes-by-lane)))))

(spec-fn build-graph (spec/* (type-spec SequenceFlow)) -> ::t/graph)
(defn build-graph [sequenceflow-nodes] 
  "Given a list of SequenceFlow nodes, returns a graph as an adjacency list using 
  sets as the list with the nodes stored as the string ids in the BPMN model."
  (reduce 
        (fn [G, e]
          (let [src (.getSourceRef e), dst (.getTargetRef e)] ; in
            (update G src #(if (nil? %) #{dst} (conj % dst)))))
        {}
        sequenceflow-nodes))

(spec-fn vertices ::t/graph)
(defn vertices [G]
  "The vertex set of G"
  (keys G))

;; ===================================================
;; JBPT PART: This should go into a separate namespace
;; but that creates a cyclic depndency
;; ===================================================


;; NOTE: We define "tasks" as both tasks and events, because we need both in the sorting relation.
;; That's because MessageFlows can go between events and tasks and the MessageFlow algorithm
;; wouldn't be able to run otherwise.
;; NOTE2: CallActivities and SubProcess are not handled by our algorighm so we ignore them.

(spec-fn task-or-event? ::t/bpmn ::t/id)
(defn task-or-event? [model id]
  (let [flow-elem (.getFlowElement (:model model) id)]
    (or (instance? org.activiti.bpmn.model.Task flow-elem)
        (instance? org.activiti.bpmn.model.Event flow-elem))))

(spec-fn elements-of-process ::t/bpmn-process)
(defn elements-of-process [process]
  (without-repeated (select (walker id?) (:graph process))))

(spec-fn process->jbpt-process ::t/bpmn ::t/bpmn-process)
(defn process->jbpt-process [model process & {:keys [remove-back-edges?]}]
  (let [task-or-event? (partial task-or-event? model)
        [tasks gateways] (span task-or-event? (elements-of-process process))
        id->jbpt-tasks (into {} (map (fn [x] [x (org.jbpt.pm.bpmn.Task. x)]) tasks))
        id->jbpt-gateways (into {} (map (fn [x] [x (org.jbpt.pm.AndGateway. x)]) gateways))
        id->jbpt-elements (merge id->jbpt-tasks id->jbpt-gateways)
        jbpt-graph (transform (walker id?) id->jbpt-elements
                              (if remove-back-edges?
                                (:graph (back-edges/make-acyclic process))
                                (:graph process)))
        jbpt-process (org.jbpt.pm.ProcessModel.)]
    (doseq [t (vals id->jbpt-tasks)] (.addTask jbpt-process t))
    (doseq [g (vals id->jbpt-gateways)] (.addGateway jbpt-process g))
    (doseq [[src dests] jbpt-graph]
      (doseq [dest dests] (.addControlFlow jbpt-process src dest)))
    {:jbpt-process jbpt-process
     :id->jbpt-elements id->jbpt-elements
     :id->jbpt-tasks id->jbpt-tasks
     :id->jbpt-gateways id->jbpt-gateways}))

;;TODO: The jbpt model is already computed and thrown when building the model graph
;;      to find seses. Add it to the model so it can be retrieved here
;;TODO: Add a version that takes a process as well to convert processes other than the first
(spec-fn activiti->jbpt ::t/activiti-model -> ::t/jbpt-process)
(defn activiti->jbpt
  "Converts a raw activiti process model into jbpt"
  [activiti-model]
  (let [model (build-model' activiti-model)
        process (first (:processes model))]
    (process->jbpt-process model process)))
