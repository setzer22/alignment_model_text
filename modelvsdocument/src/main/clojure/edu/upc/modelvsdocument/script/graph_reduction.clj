(ns edu.upc.modelvsdocument.script.graph-reduction
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [clojure.set :as set]))

(spec/def ::edge (spec/cat :src any? :dst any?))
(spec/def ::edges (spec/* ::edge))
(spec/def ::predicate (spec/fspec :args (spec/cat :x any?)
                                  :ret boolean?))



(spec-fn incoming ::t/graph any? -> set?)
(defn incoming [G, u]
  "The incoming vertices of n"
  (as-> G $$
    (filter (fn [[v v-adj]] (contains? v-adj u)) $$)
    (map first $$)
    (set $$)))

(spec-fn outgoing ::t/graph any? -> set?)
(defn outgoing [G, u]
  (get G u))

(spec-fn cartesian seq? seq? -> seq?)
(defn cartesian [s1, s2]
  (for [x s1, y s2]
    [x y]))

(spec-fn nodes ::t/graph -> seq?)
(defn nodes [G]
  (keys G))


(spec-fn add-edges ::t/graph ::edges -> ::t/graph)
(defn add-edges [G edges]
  (reduce 
    (fn [G [src dst]]
      (merge-with into G {src #{dst}}))
    G
    edges))

(spec-fn remove-edges ::t/graph ::edges -> ::t/graph)
(defn remove-edges [G edges]
  (reduce 
    (fn [G [src dst]]
      (assoc G src (set/difference (outgoing G src) #{dst})))
    G
    edges))

(spec-fn filter-graph ::t/graph ::predicate -> ::t/graph)
(defn filter-graph [G need-to-filter?]
  "Removes the nodes from G that don't satisfy predicate f"
  (let [filtered-nodes (filter need-to-filter? (nodes G))
        remaining-nodes (filter (complement need-to-filter?) (nodes G))]
    (as-> G $$
      (apply dissoc $$ filtered-nodes)
      (reduce (fn [G, node] 
                (assoc G node (set (remove need-to-filter? (G node)))))
              $$
              remaining-nodes))))

; Assumptions: 
;   * A gateway node is never a source or a sink of the graph
(spec-fn reduce-graph ::t/graph ::predicate -> ::t/graph)
(defn reduce-graph [G need-to-remove?]
  (let [nodes-to-remove (filter need-to-remove? (nodes G))]
    (assert ; Check the assumption
      (every? 
        #(and (not (empty? (incoming G %))) (not (empty? (outgoing G %))))
        nodes-to-remove))
    (reduce 
      (fn [G node-to-remove]
        (let [in (incoming G node-to-remove)
              out (outgoing G node-to-remove)
              in-edges (map vector in (repeat node-to-remove))
              out-edges (map vector (repeat node-to-remove) out)
              new-edges (cartesian in out)]
          (-> G 
               (remove-edges in-edges)
               (remove-edges out-edges)
               (add-edges new-edges)
               (dissoc node-to-remove))))
      G
      nodes-to-remove)))

(comment 
  (def test-graph 
    {:s #{:a}
     :a #{:g1}
     :g1 #{:b :c}
     :b #{:g2}
     :c #{:g2}
     :g2 #{:e}})

  (def test-graph-2
    {:a #{:g1 :g2}
     :g1 #{:g2}
     :g2 #{:b}})

  (def test-graph-3
    {:a #{:g1}
     :g1 #{:d :g2}
     :g2 #{:b :c}
     :b #{:g3}
     :c #{:g3}
     :d #{:g3}
     :g3 #{:e}})

  (reduce-graph test-graph #(.startsWith (name %) "g"))
  (reduce-graph test-graph-2 #(.startsWith (name %) "g"))
  (reduce-graph test-graph-3 #(.startsWith (name %) "g")))
