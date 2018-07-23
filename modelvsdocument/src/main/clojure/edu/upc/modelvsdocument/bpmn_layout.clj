(ns edu.upc.modelvsdocument.bpmn-layout
  (:require [edu.upc.modelvsdocument.bpmn :as bpmn]
            [clojure.inspector :as inspector]
            [com.rpl.specter :as specter])
  (:import [org.activiti.bpmn BpmnAutoLayout]
           [org.activiti.bpmn.model BpmnModel]
           [org.activiti.bpmn.converter BpmnXMLConverter])
  (:gen-class
   :name edu.upc.modelvsdocument.BpmnLayout
   :methods [#^{:static true} [autoLayoutModel [String String] void]]))

(defn bpmn->xml [bpmn]
  (String. (.convertToXML (BpmnXMLConverter.) (:model bpmn))))

(defn copy-graphic-info! [src dst offset]
  (do (.setX dst (+ (.getX offset) (.getX src)))
      (.setY dst (+ (.getY offset) (.getY src)))
      (.setWidth dst (.getWidth src))
      (.setHeight dst (.getHeight src))))

(defn g-info [bpmn id]
  (.getGraphicInfo (:model bpmn) id))

(defn flow-g-info [bpmn id]
  (.getFlowLocationGraphicInfo (:model bpmn) id))

(defn g-add [src add]
  (.setX src (+ (.getX src) (.getX add)))
  (.setY src (+ (.getY src) (.getY add))))

(defn offset-waypoints [waypoints origin]
  (doseq [w waypoints]
    (g-add w origin)))

(defn compute-bounding-box [bpmn ids]
  (reduce
   (fn [{:keys [min-x, min-y, max-x, max-y]} id]
     (let [g (g-info bpmn id)]
       {:min-x (min min-x (.getX g))
        :min-y (min min-y (.getY g))
        :max-x (max max-x (+ (.getWidth g) (.getX g)))
        :max-y (max max-y (+ (.getHeight g) (.getY g)))}))
   {:min-x Float/POSITIVE_INFINITY :min-y Float/POSITIVE_INFINITY
    :max-x Float/NEGATIVE_INFINITY :max-y Float/NEGATIVE_INFINITY}
   ids))

(defn set-pool-dimensions! [bpmn pool-id bounding-box padding]
  (let [x (- (:min-x bounding-box) (:left padding))
        y (- (:min-y bounding-box) (:top padding))
        w (+ (- (:max-x bounding-box) (:min-x bounding-box)) (+ (:left padding) (:right padding)))
        h (+ (- (:max-y bounding-box) (:min-y bounding-box)) (+ (:top padding) (:bottom padding)))
        gfx (g-info bpmn pool-id)]
    (do (.setX gfx x) (.setY gfx y) (.setWidth gfx w) (.setHeight gfx h))))

(defn flow-node? [e]
  (instance? org.activiti.bpmn.model.FlowNode e))
(defn sequence-flow? [e]
  (instance? org.activiti.bpmn.model.SequenceFlow e))

(defn set-pool-relative-to! [bpmn reference-pool-id pool-id y-spacing]
  (let [ref-g    (g-info bpmn reference-pool-id)
        pool     (.getPool (:model bpmn) pool-id)
        process  (first (filter #(= (.getProcessRef pool) (.getId %)) (.getProcesses (:model bpmn))))
        pool-g   (g-info bpmn pool-id)
        y-offset (- (+ (.getY ref-g) (.getHeight ref-g) y-spacing) (.getY pool-g))]
    (.setY pool-g (+ (.getY pool-g) y-offset))
    (doseq [e (.getFlowElements process)]
      (cond (flow-node? e) (let [g (g-info bpmn (.getId e))]
                             (.setY g (+ (.getY g) y-offset)))
            (sequence-flow? e) (let [gs (flow-g-info bpmn (.getId e))]
                                 (doseq [g gs]
                                   (.setY g (+ (.getY g) y-offset))))))
    (doseq [l   (.getLanes process)
            :let [g (g-info bpmn (.getId l))]]
      (.setY g (+ (.getY g) y-offset)))))

;; TODO:
;; - Add events

(defn -autoLayoutModel
  "Fixes the layout of the bpmn model at path and saves it as a new layouted model in out-path"
  [model-path out-path]
  (let [model (bpmn/build-model (bpmn/read-model model-path))
        ;;__ (def model-test model)
        model-layouted (bpmn/build-model (bpmn/read-model model-path))
        layout (BpmnAutoLayout. (:model model-layouted))
        __ (.execute layout)
        task-ids (concat (bpmn/all-task-ids model) (bpmn/all-event-ids model) (bpmn/all-gateway-ids model))
        sequenceflow-ids (map #(.getId %) (bpmn/all-sequenceflows model))
        messageflow-ids (bpmn/all-messageflow-ids model)]
   ;; Task positions
   (doseq [t task-ids
           :let [task (g-info model t)
                 task' (g-info model-layouted t)
                 process (g-info model (.getId (bpmn/pool-of-task t model)))]]
     (copy-graphic-info! task' task process))
   ;; SequenceFlow positions
   (doseq [f sequenceflow-ids
           :let [waypoints (flow-g-info model f)
                 waypoints' (flow-g-info model-layouted f)
                 process (g-info model (.getId (bpmn/pool-of-task f model)))]]
     (offset-waypoints waypoints' process)
     (.addFlowGraphicInfoList (:model model) f waypoints'))
   ;; MessageFlow positions
   (doseq [f messageflow-ids
           :let [mf (.getMessageFlow (:model model) f)
                 [src dst] [(.getSourceRef mf) (.getTargetRef mf)]
                 pos-src (g-info model src)
                 pos-dst (g-info model dst)
                 waypoints [pos-src pos-dst]]]
     (.addFlowGraphicInfoList (:model model) f waypoints))
   ;; Pool positions
   (doseq [process (:processes model)
           :let [ids (keys (:graph process))
                 bounding-box (compute-bounding-box model ids)
                 pool-id (.getId (:pool process))]]
     (set-pool-dimensions! model pool-id bounding-box {:top 30 :bottom 30 :left 70 :right 30}))
   ;; Lane positions
   (doseq [lane (specter/select [:processes specter/ALL :lanes specter/ALL] model)
           :let [ids (.getFlowReferences lane)
                 bounding-box (compute-bounding-box model ids)
                 lane-id (.getId lane)]]
     (set-pool-dimensions! model lane-id bounding-box {:top 30 :bottom 30 :left 50 :right 30}))
   ;; Final repositioning
   (let [pool-ids (sort-by
                   (fn [pool-id] (.getY (g-info model pool-id)))
                   (map #(.getId (:pool %)) (:processes model)))]
     (doseq [[prev-pool pool] (partition 2 1 pool-ids)]
       (set-pool-relative-to! model  prev-pool pool 50)))
   (spit out-path (bpmn->xml model))))
