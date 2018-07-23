(ns edu.upc.modelvsdocument.extraction.constituents-matching
  (:gen-class)
  (:require [edu.upc.modelvsdocument.extraction.macros :refer []]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.sorter.model-sorter :as modelsorter]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clj-http.client :as client]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [clojure.data.json :as json]
            [clj-xpath.core :as xpath])
  (:use [com.rpl.specter]
        [clojure.pprint]
        [edu.upc.modelvsdocument.extraction.feature]
        [edu.upc.modelvsdocument.extraction.macros]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.extraction.common]
        [clojure.core.match :as match :refer [match]])
  (:import [edu.upc.modelvsdocument.extraction.feature Feature]))

;TODO: More than one constituent?
;TODO: This should be rewritten to use the dependency parsing. 
;TODO: This work overlaps with Text2BPMN's rule language.

(defn token? [t]
  ;(and (map? t) (contains? t :lemma) (contains? t :form) (contains? t :id))
  (spec/valid? ::t/token t))

(defn leaf? [l]
  (and (map? l) (= "1" (:leaf l)) (contains? l :token)))

(defn enrich-with-tokens [sentence, structure]
  (transform 
    (walker #(and (string? %) (re-matches #"t\d+\.\d+" %)))
    (fn [tk-id] (token-of-id sentence tk-id))
    structure))

(defn constituents-tree-seq [T] 
  (tree-seq 
    (fn [node] (and (contains? node :children) (contains? node :label)))
    :children
    T))

(defn tokens-until-break [nodes]
  (let [nodes-seq (constituents-tree-seq {:label "root" :children nodes})]
    (map :token
         (filter leaf?
                 (take-while #(not= "sf-brk" (:label %)) 
                             nodes-seq)))))

(defn extract-tokens-in-condition [sentence]
  (tree-match [(enrich-with-tokens sentence (first (:constituents sentence)))]
      ["claus" ["adv" "if"] & rst] 
      (tokens-until-break rst)

      ["sub-cl" "if" & rst] 
      (tokens-until-break rst)))


;=================
;      REPL
;=================

(comment 

  (defn analyze-sentence [text]
    (let [text-res (edu.upc.modelvsdocument.textserver/textserver->json 
                     (edu.upc.modelvsdocument.textserver/analyze-cached 
                       :text text, :lang "en"))
          sentence (select-one [:paragraphs ALL :sentences FIRST] text-res)]
      sentence))

  (defn extract-tokens-in-condition-text [text]
    (let [sentence (analyze-sentence text)]
      (extract-tokens-in-condition sentence)))

  (defn constituents->tree 
    ([{:keys [label, children, leaf, token] :as constituents}]
     (cond
       leaf token
       (and label children) (apply list label (map constituents->tree children)))))

  (defn print-tree-of-sentence [text]
    (let [text-res (edu.upc.modelvsdocument.textserver/textserver->json 
                     (edu.upc.modelvsdocument.textserver/analyze-cached 
                       :text text
                       :lang "en"))
          sentence (select-one [:paragraphs ALL :sentences FIRST] text-res)
          constituents (first (:constituents sentence))]
      (pprint (transform (walker token?) :lemma (enrich-with-tokens sentence (constituents->tree constituents))))))

  (def path "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/trees/")
  (use '[clojure.java.shell :only [sh]])

  (def sentences 
    ["If goods shall be shipped, the secretary clarifies who will do the shipping."
     "If he is right, I simply close the case."
     "If the insurant disagrees with the recourse, I'll have to check the reasoning of that."
     "If the deadline for the disagreement is reached and we haven't received any money, I forward the case to the collection agency as well." ])

  (map :form (extract-tokens-in-condition-text "If customer wants a family membership instead, customer should prepare the information for its spouse and spawn as well."))



  (binding [*print-level* 3] 
    (doseq [s-txt sentences]
      (println s-txt)
      (println (map :form (extract-tokens-in-condition-text s-txt)))))

  (do 
    (doseq [[s i] (zip sentences (range)) ]
      (spit (str path "tree_" i ".txt") (str s "\n\n-----------------\n" (with-out-str (print-tree-of-sentence s)))))
    (apply sh "kate" (for [i (range (count sentences))] (str path "tree_" i ".txt"))))) 
