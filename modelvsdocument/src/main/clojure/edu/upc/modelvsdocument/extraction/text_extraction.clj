(ns edu.upc.modelvsdocument.extraction.text-extraction
  (:gen-class)
  (:require [clojure.spec :as spec]
            [clojure.string :as string]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clj-http.client :as client]
            [edu.upc.modelvsdocument.tf-idf :as idf]
            [edu.upc.modelvsdocument.extraction.macros :refer [text-extractor]]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.extraction.feature]
            [edu.upc.modelvsdocument.extraction.constituents-matching :as constituents]
            [clojure.data.json :as json]
            [clj-xpath.core :as xpath]
            [incanter.core :as mth]
            [edu.upc.modelvsdocument.wordnet :as wn]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.text :as text]
            [clojure.set :as set])
  (:import [edu.upc.modelvsdocument.extraction.feature Feature]
           [edu.upc.modelvsdocument.text Sentence])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.extraction.common]
        [edu.upc.modelvsdocument.extraction.feature]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.config]))

(def extractor-functions (atom {}))

(text-extractor extract-forms [Sentence]
  "Extracts the lower-case forms of a sentence"
  [s, tf-idf-table]
  (map #(has-form [(:lemma %)] (idf/get-tf-idf tf-idf-table % s)) (select [TOKENS] s)))

(text-extractor extract-lemmas [Sentence]
  "Extracts the lemmas of a sentence"
  [s, tf-idf-table]
  (map #(has-lemma [(:pos %) (:lemma %)] (idf/get-tf-idf tf-idf-table % s)) (select [TOKENS] s)))

;(text-extractor extract-synsets [s, tf-idf-table]
  ;(map #(has-synset [%]) (remove nil? (select [TOKENS :wn] s))))

(text-extractor extract-synonyms-and-hypernyms [Sentence]
  [s, tf-idf-table]
  (let [tokens              (select [TOKENS] s)
        ;@Copypasted
        synsets-and-weights (map
                             (fn [token]
                               [(senses-of-token token) (idf/get-tf-idf tf-idf-table token s)])
                             tokens)
        synsets-and-weights (as-> synsets-and-weights $$
                              (mapcat #(map vector (first %) (repeat (second %))) $$))]
    (make-synset-and-hypernym-features synsets-and-weights)))

(text-extractor extract-conditionals [Sentence]
  [s, tf-idf-table]
  (let [conditional-tokens (filter non-stopword? (constituents/extract-tokens-in-condition s))]
    (map #(lemma-conditional-follow [(:lemma %) (:pos %)]) conditional-tokens)))

;TODO Filter auxiliary verbs
(text-extractor extract-actions [Sentence]
  [s, tf-idf-table]
  (map
    (fn [tk-id] (has-action [(token-lemma s tk-id)]))
    (select [:predicates ALL :head_token] s)))

(text-extractor extract-agents [Sentence]
  [s, tf-idf-table]
  ;; We create two kinds of agent-features
  (concat
   (map (fn head-feature-list [tk]
          (agent-head [(:lemma tk) (:pos tk)]
                      (idf/get-tf-idf tf-idf-table tk s)))
        (heads-of-role-type s "A0"))
   (map (fn token-feature-list [tk]
          (in-agent [(:lemma tk) (:pos tk)]
                    (idf/get-tf-idf tf-idf-table tk s)))
        (tokens-of-role-type s "A0"))))


;;TODO: Copypasted // changed in-agent -> in-patient. Common pattern here...
(text-extractor extract-patients [Sentence]
  [s, tf-idf-table]
  ;; We create two kinds of agent-features
  (concat
   (map (fn head-feature-list [tk]
          (patient-head [(:lemma tk) (:pos tk)]
                        (idf/get-tf-idf tf-idf-table tk s)))
        (heads-of-role-type s "A1"))
   (map (fn token-feature-list [tk]
          (in-patient [(:lemma tk) (:pos tk)]
                      (idf/get-tf-idf tf-idf-table tk s)))
        (tokens-of-role-type s "A1"))))

(comment

  (def --text (edu.upc.modelvsdocument.text/analyze :text (slurp "/home/josep/ModelsBpmn/Zoo.txt")
                                                    :lang "en"))

  (def --tf-idf-table (idf/tf-idf-table (select ALL-SENTENCES --text)))

  (def --sentence (nth (select ALL-SENTENCES --text) 1))

  --sentence

  (extract-patients --sentence --tf-idf-table)

  (heads-of-role-type
   --sentence
   "A0"))

(def discourse-markers
  {:conditional  ["if" "whether" "in_case_of" "in_the_case_of" "in_case" "for_the_case" "whereas" "otherwise" "optionally" "depend" "or" "alternative"]
   :parallel   ["while" "meanwhile" "in_parallel" "simultaneously" "concurrently" "meantime" "in_the_meantime"]
   :sequential ["then" "after" "afterward" "afterwards" "subsequently" "based_on_this" "thus"]})

(text-extractor extract-discourse-markers [Sentence]
  [s, tf-idf-table]
  (mapcat
   (fn [marker-type]
     (let [lemmas (set (discourse-markers marker-type))]
       (when-not (empty? (set/intersection (set (select [:tokens ALL :lemma] s)) lemmas))
         [(has-discourse-marker [(name marker-type)])])))
   (keys discourse-markers)))

(spec-fn extract-features ::t/text -> ::t/feature-vector)
(defn extract-features [text]
  "Extracts all features from sentences in the text"
  (let [tf-idf-table (idf/tf-idf-table (select ALL-SENTENCES text))
        alignables (a/get-all-alignables text)]
    (mapify
     (map
      (fn [a] (let [s (select-one [ALL-SENTENCES #(= (:id %) (a/id a))] text)]
                [a (into [] (distinct-features (flatten (map #(% s tf-idf-table) (vals (@extractor-functions (type a)))))))]))
      alignables))))

(comment
  "REPL"
  (def --text (text/analyze :text (slurp "/home/josep/ModelsBpmn/Zoo.txt")
                            :lang "en"))

  (extract-features --text))

  
