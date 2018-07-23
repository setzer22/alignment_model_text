(ns edu.upc.modelvsdocument.back-edges
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.alignable :as a]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combinatorics])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [clojure.pprint]))

(defn vertices [G]
  "The vertex set of G"
  (keys G))

(defn conj-into [G edges]
  "Adds all edges in edges to g"
  (reduce (fn [G' [u v]] (assoc G' u (conj (get G' u #{}) v)))  G  edges))

(defn map-all-to-k [k vals]
  (reduce #(assoc %1 %2 k) {} vals))

(defn reverse-graph [G]
  (reduce (fn [G [v adj]] (conj-into G (map-all-to-k v adj)))
          {}
          G))

(defn initial-out [G start] ;;;;
  (assoc
   ;;(apply conj (map #(into {} [[% (into #{} (vertices G))]]) (vertices G))) ; Out [B] = {all nodes} for all B except start
   (into {}
         (for [v (->> G vertices (remove #(= start %)))]
           [v (set (vertices G))]))
   start #{start})) ; Out[start] = {start}

(defn next-out [pred out start];;;;
  "Returns the next out for out given the predecessors in the graph"
  (into {} (map (fn [[v out-v]]
                  (if (= v start)
                    [v, out-v]
                    [v, (set/union #{v}
                                   (try-or (apply set/intersection (map #(out %) (pred v))) #{}))]))
                out)))

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

#_(defn positions [{G :graph :as process}]
    "Maps each node in G to an integer according to the ordering of the nodes"
    (reduce (fn [G [level nodes]] (apply assoc G (flatten (zip nodes (repeat level)))))
            {}
            (map-indexed vector (topological-sort (:graph (make-acyclic process))))))

