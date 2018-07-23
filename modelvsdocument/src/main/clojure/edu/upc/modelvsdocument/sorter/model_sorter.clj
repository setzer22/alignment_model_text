(ns edu.upc.modelvsdocument.sorter.model-sorter
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.alignable :as a]
            [clojure.set :as set]
            [edu.upc.modelvsdocument.sorter.behavioral-profiles :as bp]
            [clojure.math.combinatorics :as combinatorics])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [clojure.pprint]
        [edu.upc.modelvsdocument.bpmn])
  (:gen-class))

; :gt :<-
; :lt :->

(defn elements-after [order task-id]
  (map first (filter (fn [[id rel]] (= rel :->)) (get order task-id))))

(defn elements-before [order task-id]
  (map first (filter (fn [[id rel]] (= rel :<-)) (get order task-id))))

(defn adapt-to-messageflow [order [start end]]
  (let [before (conj (elements-before order start) start)
        after (conj (elements-after order end) end)]
    (reduce
     (fn [order [x y]]
       (-> order
           (assoc-in [x y] :->)
           (assoc-in [y x] :<-)))
     order
     (combinatorics/cartesian-product before after))))

(defn adapt-to-messageflows [order message-flows]
  (reduce adapt-to-messageflow order message-flows))

(spec-fn compute-order ::t/bpmn -> (spec/map-of ::t/id (spec/map-of ::t/id ::t/order-relation)))
(defn compute-order [model & {:keys [remove-back-edges?]}]
  (let [processes     (:processes model)
        process-order (apply merge (map #(bp/compute-process-order-relation model % :remove-back-edges? remove-back-edges?) processes))
        message-flows (:message-flows model)]
    (loop [order process-order, old-order nil, it 0]
      (cond
        (> it 100)          (throw (Exception. "Maximum number of iterations for the order matrix."))
        (= order old-order) order
        :else               (recur (adapt-to-messageflows order message-flows), order, (inc it))))))

(defn build-ordering
  "Returns the computed order transforming task-ids into alignables. Note that
   since all BPMN elements implement the alignable interface, it will compute the
   ordering between all elements, not just the ones returned by a/get-all-alignables"
  [model & {:keys [remove-back-edges?]}]
  (let [order (compute-order model :remove-back-edges? remove-back-edges?)]
    (transform (walker string?) #(a/get-element model %) order)))

;; ----------------
;; Topological sort TODO: This is unused...
;; ----------------

(defn no-incoming-edges [G]
  "Returns the set of G's nodes without incoming edges"
  (set/difference (set (vertices G)) (apply set/union (vals G))))

(defn remove-nodes [G nodes]
  "The nodes in nodes are removed from the graph. Nodes are string"
    (into {} (for [[v lst] (apply dissoc G nodes)] [v (set/difference lst nodes)])))

(defn topological-sort [G]
  "Returns the topological sorting of G. G must be acyclic"
  (loop [G G, levels []]
    (let [free (no-incoming-edges G)
          G' (remove-nodes G free)] ;in
      (if (empty? free)
        levels
        (recur G' (conj levels free))))))

;; ---------------------
;; Back-edge elimination
;; ---------------------


(defn initial-out [G start] ;;;;
  (assoc
    ;(apply conj (map #(into {} [[% (into #{} (vertices G))]]) (vertices G))) ; Out [B] = {all nodes} for all B except start
    (into {}
          (for [v (->> G vertices (remove #(= start %)))]
            [v (set (vertices G))]))
    start #{start})) ; Out[start] = {start}

(defn next-out [pred out start];;;;
  "Returns the next out for out given the predecessors in the graph"
  (into {} (map (fn [[v out-v]]
                  (if (= v start)
                    [v, out-v]
                    [v, (set/union #{v} (apply set/intersection (map #(out %) (pred v))))])
                 out))))

(defn dominators [G start max-it]
  "Returns the mapping for every node in the Graph to its set of dominators"
  ;; inv: out_(i-1) != out_i, it <= max-it
  (let [pred (reverse-graph G)]; in
    (loop [out_i-1 (initial-out G start),
           out_i (next-out pred out_i-1 start),
           it 0]
      ;;Error condition: Too many iterations, this is not converging.
      (if (> it max-it) (throw (Exception. "Maximum number of iterations for the dominators set reached.")))

      ;;Normal loop
      (if (.equals out_i-1 out_i)
        out_i
        (recur out_i (next-out pred out_i start) (inc it))))))

(defn back-edges [G start]
  "Returns all the back-edges in G"
  (let [dominators-of (dominators G start 100); TODO: Arbitrary 100
        edge-set (apply concat (map (fn [[v lst]] (zip (repeat v) lst)) G))]
    (filter (fn [[a b]] (contains? (dominators-of a) b)) edge-set)))

(defn remove-edges [G edges]
  (reduce (fn [G [u v]] (update G u #(disj % v))) G edges))

(defn make-acyclic [{G :graph start :start-event :as process}]
  (update process :graph #(remove-edges % (back-edges G start))))

(defn positions [{G :graph :as process}]
  "Maps each node in G to an integer according to the ordering of the nodes"
  (reduce (fn [G [level nodes]] (apply assoc G (flatten (zip nodes (repeat level)))))
          {}
          (map-indexed vector (topological-sort (:graph (make-acyclic process))))))
