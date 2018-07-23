(ns edu.upc.modelvsdocument.tf-idf
  (:require [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.extraction.common :as text-utils :refer [non-stopword?]]
            [edu.upc.modelvsdocument.config :refer [config]]
            [com.rpl.specter :as specter :refer :all]
            [clojure.pprint :as pprint :refer :all]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clojure.set :as set]
            [edu.upc.modelvsdocument.utils :refer :all]))

(spec/def ::token-histogram
  (spec/map-of string? nat?))
(spec/def ::set-sentences
  (spec/* (spec/and set? #(every? string? %))))

(spec-fn token-histogram (spec/* ::t/token) -> ::token-histogram)
(defn token-histogram [tokens]
  (map (fn [[lemma ocurrences]] [lemma (count ocurrences)])
       (group-by :lemma (filter non-stopword? tokens))))

(spec-fn count-text ::t/tokenized-text)
(defn count-text [text]
  (let [tokens (select [:paragraphs ALL :sentences ALL :tokens ALL] text)]
    (token-histogram tokens)))

(spec-fn count-sentence ::t/sentence)
(defn count-sentence [sentence]
  (let [tokens (select [:tokens ALL] sentence)]
    (token-histogram tokens)))

;; tf := (Number of times token t appears in a sentence) / (Total number of tokens in the sentence)
;; idf := log_e(Total numer of sentences / Number of sentences with term t in it)
(Math/log 2)

(defn tf [token sentence-tokens]
  (let [num-tokens (count sentence-tokens)
        num-appearances (count (filter #(= token %) sentence-tokens))]
    (/ num-appearances
       num-tokens)))

(defn idf [token sets-of-sentences]
  (def --token token)
  (def --sos sets-of-sentences)
  (let [num-sentences-with-token (count (filter #(% token) sets-of-sentences))]
    (try-or (Math/log
             (/ (count sets-of-sentences)
                num-sentences-with-token))
            0)))

(spec-fn enrich-text-with-tf-idf ::t/tokenized-text)
(defn tf-idf-table
  [sentences]
  (let [token-lemmas            (select [ALL :tokens ALL non-stopword? :lemma] sentences)
        num-sentences           (count sentences)
        sets-of-sentences-by-id (mapify (map (fn [sentence] [(:id sentence)
                                                             (into #{} (select [:tokens ALL non-stopword? :lemma] sentence))])
                                             sentences))
        idfs                    (mapify (map (fn [lemma] [lemma (idf lemma (vals sets-of-sentences-by-id))])
                                             (distinct token-lemmas)))
        tfs                     (mapify (map (fn [{:keys [id] :as sentence}]
                                               (let [tokens            (select [:tokens ALL non-stopword? :lemma] sentence)
                                                     num-tokens        (count tokens)
                                                     token-frequencies (frequencies tokens)]
                                        ;TODO: If I ever update specter, [ALL LAST] -> [MAP-VALS]
                                                 [id (transform [ALL LAST] #(/ % num-tokens) token-frequencies)]))
                                             sentences))]
    {::t/tfs  tfs
     ::t/idfs idfs}))

(spec-fn get-tf ::t/tf-idf-table string? string?)
(defn get-tf [table lemma sentence-id]
  (-> table ::t/tfs (get sentence-id) (get lemma)))

(spec-fn get-idf ::t/tf-idf-table string? string?)
(spec-fn get-idf )
(defn get-idf [table lemma]
  (-> table ::t/idfs (get lemma)))

(spec-fn get-tf-idf ::t/tf-idf-table (spec/or :lemma string? :token ::t/token) (spec/or :id string? :sentence ::t/sentence))
(defn get-tf-idf [table token sentence]
  (if (config :enable-tf-idf)
    (let [lemma (or (:lemma token) token)
          sentence-id (or (:id sentence) sentence)
          tf (get-tf table lemma sentence-id)
          idf (get-idf table lemma)]
      (cond
        (and tf idf) (* tf idf)
        :else 0.001
        #_(not (config :tf-idf-strict)) #_0.001
        #_:else
        #_(throw (Exception. (str  "TF-IDF table error. token=" token
                                   ", sentence=" (:id sentence)
                                   ", tf=" (if tf tf "nil") ", idf="
                                   (if idf idf nil))))
        ))
    1))

(comment
  "Some tests"

  (use '[clojure.test])

  (let [sample-text "Dog dog cat dog. Cat cat parrot dog."
        analyzed-text (textserver/textserver->json (textserver/analyze-with-textserver :text sample-text))]
    (is (tf-idf-table analyzed-text)
        {::t/tfs {"1" {"dog" 3/4, "cat" 1/4}, "2" {"cat" 1/2, "parrot" 1/4, "dog" 1/4}},
         ::t/idfs {"dog" 0.0, "parrot" 0.6931471805599453, "cat" 0.0}}
        )))
