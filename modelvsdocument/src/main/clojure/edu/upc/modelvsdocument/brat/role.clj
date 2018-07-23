(ns edu.upc.modelvsdocument.brat.role
  (:require [clj-pipeline.core :as pipeline]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.brat.utils :refer :all]
            [edu.upc.modelvsdocument.brat.specs :as brt]
            [clojure.set :as set]))

(defrecord Role [id main-mention-words important-words non-important-words])

(def superfluous-role-pos
  "The set of PoS for non-important roles"
  #{"pronoun" "punctuation", "preposition", "determiner", "modal", "conjunction", "particle"})

(defn compute-ccs [nodes, neighbors-fn]
  (let [nodes-set (set nodes)]
    (loop [ccs []
           current-cc #{}
           visited-nodes #{}
           [n & ns :as to-visit] [(first nodes)]]
      (cond
        (= (count nodes) (count visited-nodes)) (conj ccs current-cc)
        (nil? n) (recur (conj ccs current-cc)
                        #{}
                        visited-nodes
                        [(find-in-seq nodes (complement visited-nodes))])
        :else (recur ccs
                     (conj current-cc n)
                     (conj visited-nodes n)
                     (into (vec (filter (complement visited-nodes)
                                        (neighbors-fn n)))
                           ns))))))

(defn- is-role-important? [attrs role-tag]
  (let [important-attr (get-in attrs ["ImportantRole" (:id role-tag)])]
    ;;TODO: I made a simplification. I assume that the main word in a role
    ;;      annotation is the first selected token. That could be wrong.
    (cond (nil? important-attr) ((complement superfluous-role-pos)
                                 (-> role-tag :start-token :pos))
          (= "Yes" important-attr) true
          (= "No" important-attr) false)))

;; TODO: We should define what is the annotation convention. Annotate only the main tokens, or a range of words. In either case, do we get more tokens than the annotated ones based on parse tree techniques?
(pipeline/defpipe-step compute-roles [enriched-anns analyzed-text relation-graph attrs]
  (let [role-tags (filter #(#{"Object" "Role"} (:type %)) (::brt/tag enriched-anns))
        role-tags-by-id (mapify (pair-with-l :id role-tags))
        role-ccs (compute-ccs (map :id role-tags)
                              #(adj-with-type "CoreferencedWith" relation-graph %))
        roles (for [cc role-ccs
                    :let [cc-tags (map role-tags-by-id cc)
                          [important non-important] (span #(is-role-important? attrs %)
                                                          cc-tags)
                          main-mention-id (some #(get-in attrs ["MainMention" %]) cc)
                          main-mention (if main-mention-id
                                         (role-tags-by-id main-mention-id)
                                         (first important))
                          tokens-in-range #(get-tokens-in-ann-range analyzed-text %)]]
                (if-not main-mention
                  (println "[WARNING]: Annotation "
                           (mapv #(-> % :start-token :form)
                                 non-important)
                           " contains only stowords and will be ignored.")
                  (->Role (:id main-mention)
                          (tokens-in-range main-mention)
                          (mapcat tokens-in-range important)
                          (mapcat tokens-in-range non-important))))
        id->Role (mapify
                  (mapcat identity
                          (for [[ids role] (zip role-ccs roles)]
                            (for [id ids]
                              [id role]))))]
    (pipeline/env roles id->Role)))

