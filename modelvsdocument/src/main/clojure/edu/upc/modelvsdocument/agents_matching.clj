(ns edu.upc.modelvsdocument.agents-matching
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.extraction.common]
        [clojure.pprint])
  (:require [clojure.set :as set]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]))

(comment (def -text edu.upc.modelvsdocument.core/text-res)
         (def -model-an edu.upc.modelvsdocument.core/model-an))

(spec-fn match-agents ::t/text ::t/analyzed-model)
(defn match-agents [text model-an]
  (let [entity-id->sense (apply hash-map
                                (select
                                  [:semantic_graph :entities ALL (multi-path :id :sense)]
                                  text))
        text-entities (remove nil? (vals entity-id->sense))

        model-actor-sentences (select [:paragraphs ALL :sentences ALL
                                       #(or (not (nil? (:pool-id %))) (not (nil? (:lane-id %))))] model-an)
        model-actors (map (fn [sentence] {:id (some identity [(:pool-id sentence) (:lane-id sentence)])
                                          :tokens (remove nil? (select [:tokens ALL non-stopword?] sentence))})
                          model-actor-sentences)]
    (into {}
          (for [{actor-id :id actor-tokens :tokens} model-actors]
            [actor-id
             (first
               (find-in-seq
                 entity-id->sense
                 (fn [[id sense]]
                   (in? (map :wn actor-tokens) sense))))]))))

