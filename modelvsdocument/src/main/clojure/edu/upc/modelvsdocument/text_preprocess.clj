(ns edu.upc.modelvsdocument.text-preprocess
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint]
        [edu.upc.modelvsdocument.extraction.common :as text])
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [com.rpl.specter.macros :as specter-macros]))

; Per cada menció amb mes de 2 ocurrències:
; --> Agafem la principal 
; --> Se substitueixen totes les següents per aquesta. Arreglant possiblement els ids

(spec-fn copy-token-data ::t/text ::t/token ::t/id)
(defn copy-token-data
  [text src-token dst-token-id]  
  (identity {:src src-token
             :dst (text/find-token-in-text text dst-token-id)})
  (transform 
    [text/ALL-SENTENCES :tokens ALL #(= (:id %) dst-token-id)]
    (fn [dst-token]
      (merge dst-token (dissoc src-token :id :begin :end)))
    text))

(spec-fn substitute-entity ::t/text any?) ;TODO: entity spec
(defn substitute-entity [text entity]
  (let [find-token (fn [id] (text/find-token-in-text text id))
        main-token (find-token (:id (first (:mentions entity)))) 
        mentions (rest (:mentions entity))
        pronoun-mentions (filter #(= "pronoun" (:pos (find-token (:id %)))) mentions)]
    (reduce (fn [text id] (copy-token-data text main-token id)) 
            text 
            (map :id pronoun-mentions))))

(spec-fn resolve-anaphoras ::t/text)
(defn resolve-anaphoras [text]
  "Given a text, performs token substitution of its pronouns by the noun they're referencing."
  (if-not (-> text :semantic_graph :entities) 
    text
    (let [entities (-> text :semantic_graph :entities)
          valid-entities (filter #(>= (count (:mentions %)) 2) entities) ]
      (reduce substitute-entity text valid-entities))))
