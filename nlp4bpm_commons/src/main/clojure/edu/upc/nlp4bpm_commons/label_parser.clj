(ns edu.upc.nlp4bpm-commons.label-parser
  (:import [edu.upc.Jfreeling MacoOptions Tokenizer Splitter
            Maco HmmTagger ChartParser DepTxala Nec Senses
            Ukb ListSentenceIterator ListWordIterator
            ListAnalysisIterator OutputJson Document Paragraph Util]
           [edu.upc.nlp4bpm_commons AnalyzedLabel])
  (:require [edu.upc.nlp4bpm-commons.freeling-seq :refer :all]
            [edu.upc.nlp4bpm-commons.core :refer [init-freeling]]
            [edu.upc.nlp4bpm-commons.freeling-output-format :refer :all]
            [edu.upc.nlp4bpm-commons.config :as config]
            [edu.upc.nlp4bpm-commons.utils :as utils :refer :all]
            [clojure.data.json :as json]
            [com.rpl.specter :as specter :refer :all]))

(defn make-modules [freeling-path lang]
  (let [cfg (fn [path] (str-or-throw freeling-path lang "/" path)) 
        maco-options (doto (MacoOptions. lang)
                       (.setDataFiles
                        ""
                        (str-or-throw freeling-path "common/punct.dat")
                        (cfg "dicc.src")
                        (cfg "afixos.dat")
                        ""
                        (cfg "locucions.dat")
                        (cfg "np.dat")
                        (cfg "quantities.dat")
                        (cfg "probabilitats.dat")))
        FORCE_TAGGER 1
        KBEST 3
        parser (ChartParser. (str-or-throw (config/-getConfig "custom-grammars-folder")
                                           "/actiongram/actions.gram"))
        result-obj {:tokenizer (Tokenizer. (cfg "/tokenizer.dat"))
                    :splitter (Splitter. (cfg "/splitter.dat"))
                    :maco (doto (Maco. maco-options)
                            (.setActiveOptions
                             false, true, true, true,
                             true, true, false, true,
                             true, true, true, true))
                    :tagger (HmmTagger. (cfg "tagger.dat"), true,
                                        FORCE_TAGGER, KBEST)
                    :parser parser
                    :senses (Senses. (cfg "senses.dat"))
                    :ukb (Ukb. (cfg "ukb.dat"))}]
    (-> result-obj :splitter .openSession)
    result-obj))

;; Adapted from clojure.string
(defn ^String trim-pred
  "Removes characters matching pred from both ends of string."
  {:added "1.2"}
  [^CharSequence s, pred]
  (let [len (.length s)]
    (loop [rindex len]
      (if (zero? rindex)
        ""
        (if (pred (.charAt s (dec rindex)))
          (recur (dec rindex))
          ;; there is at least one non-whitespace char in the string,
          ;; so no need to check for lindex reaching len.
          (loop [lindex 0]
            (if (pred (.charAt s lindex))
              (recur (inc lindex))
              (.. s (subSequence lindex rindex) toString))))))))

(defn preprocess-label [label]
  (-> label
      (clojure.string/replace "\n" " ")
      (trim-pred #(or (Character/isWhitespace %) (#{\. \;} %)))))

(defn lowercase-capitalized [list-words]
  (fl-forall
   (fn [word]
     (when (==  (Util/capitalization (.getForm word)) 1)
       (.setForm word (Util/lowercase (.getForm word)))))
   list-words))

(defn analyze-with [module sentence-list]
  (.analyze module sentence-list)
  sentence-list)

(defn get-words [node]
  (mapv :token (filter :token (tree-seq :children :children node))))

(defn get-head [node]
  (:token (first (filter :head (:children node)))))

(defn add-to [sentence, type, branch]
  (assoc-in sentence [:label_analysis type]
            {:head (get-head branch)
             :words (get-words branch)}))

(defmulti extract-predicate #(-> % :constituents first :label))

(defmethod extract-predicate "action"
  [{[{branches :children}] :constituents, tokens :tokens :as sentence}]
  (reduce
   (fn [sentence branch]
     (case (:label branch)
       "noun-phrase" (add-to sentence :object branch)
       "verb-phrase" (add-to sentence :predicate branch)
       "n-prep-phrase" (add-to sentence :complement (-> branch :children second))
       "vb-prep-phrase" (add-to sentence :complement (-> branch :children second))
       sentence))
   sentence
   branches))

;; Question mark sign after type indicates 
#_(root
   ("noun-phrase"? :object)
   ("verb-phrase"? :predicate)
   ("n-prep-phrase"? (2 :complement))
   ("vb-prep-phrase"? (2 :complement)))

(defn find-token [sentence id]
  (select-one [:tokens ALL #(= (:id %) id)] sentence))


(defmethod extract-predicate "noun-action"
  [{[{branches :children}] :constituents, tokens :tokens :as sentence}]
  (cond
    (== 1 (count branches)) ;; single noun phrase e.g: "data transmission"
    ,,,,(let [noun-phrase (first branches)
              head (get-head noun-phrase)
              others (remove #(= % head) (get-words noun-phrase))]
          (-> sentence
              (assoc-in [:label_analysis :predicate] {:head head
                                                      :words [head]})
              (assoc-in [:label_analysis :object] {:head (last others)
                                                   :words (into [] others)})))
    :else ;; noun-ph + prep-ph e.g: "transmission of data"
    ,,,,(reduce
         (fn [sentence branch]
           (case (:label branch)
             "noun-action-phrase" (add-to sentence :predicate branch)
             "n-prep-phrase"
             ,,(let [prep (-> branch :children first :word)]
                 (add-to sentence (if (= prep "of")
                                    :object
                                    :complement)
                         (-> branch :children second)))))
         sentence
         branches)))

(defmethod extract-predicate "noun-phrase" [x] x)

(comment

  (parse-labels ["Check and suggest updates"] --modules)


  (merge-with #(merge-with (comp (fn [x] (into [] x)) flatten vector) %1 %2)
              {:predicate {:head "t1.1", :words ["t1.1"]},
               :object {:head "t1.3", :words ["t1.2" "t1.3"]}}
              {:predicate {:head "t1.4", :words ["t1.4"]},
               :object {:head "t1.5", :words ["t1.5" "t1.6"]}})

  END)

(defmethod extract-predicate "double-action"
  [{[{branches :children}] :constituents, tokens :tokens :as sentence}]
  ;; e.g. Check and suggest update
  (let [;; Create sub-sentences with fragments of the parse tree, but ignore
        ;; those that are not labelled sub-trees
        sentences' (filter
                    #(-> % :constituents first :label)
                    (map #(assoc sentence :constituents [%]) branches))
        ;; Extract predicates for those individual branches and get their analysis
        analysis (remove nil? (map :label_analysis
                                   (map extract-predicate sentences')))
        __ (def --analysis analysis)
        ;; Combine all the analysis into one
        ;; TODO: NOTE: XXX: This means that if there is a label like
        ;; "copy document and remove media", the extracted action will be
        ;; copy document and media + remove document and media instead.
        merge-analysis (fn [analysis]
                         (apply
                          merge-with #(merge-with
                                       (comp (fn [x] (into [] x)) flatten vector)
                                       %1 %2)
                          analysis))
        __ (def --merge-analysis merge-analysis)]
    (assoc sentence :label_analysis (merge-analysis analysis))))


(def implemented-patterns #{"action", "noun-action", "double-action"})

(defmethod extract-predicate "top"
  [{[{branches :children}] :constituents, tokens :tokens :as sentence}]
  (let [children-matching-pattern (filter #(implemented-patterns (:label %)) branches)]
    (if (== 1 (count children-matching-pattern))
      ;; If there's a single children matching a pattern, we treat it as a match
      (let [constituents' children-matching-pattern
            sentence' (assoc sentence :constituents constituents')
            analysis (:label_analysis (extract-predicate sentence'))]
        (assoc sentence :label_analysis analysis))
      sentence)))

(defmethod extract-predicate :default [x]
  (let [t (-> x :constituents first :label)]
    (println (format "[LABEL PARSER WARNING]: Unrecognized top-level node type: %s" t))
    x))

(defn select-best-seq [freeling-sentence]
  (let [N (.numKbest freeling-sentence)
        best-i (loop [i 0]
                 (cond
                   (= i N) 0
                   :else (let [parse-tree (.getParseTree freeling-sentence i)
                               label (-> parse-tree .begin .getLabel)]
                           (if (implemented-patterns label)
                             i
                             (recur (inc i))))))]
    (.setBestSeq freeling-sentence best-i)
    freeling-sentence))

(defn set-sentencelist-id [sentence-list id]
  (fl-forall
   (fn [sentence]
     (.setSentenceId sentence (str id)))
   sentence-list))

(defmacro only-if-type [label-type & body]
  `(if (= ~'label-type ~label-type)
     ~@body
     ~'$$))

;; NOTE: The parse-labels pipeline has been split in 3 due to the need for two
;;      separate APIs. The first API uses freeling JSON format, while the second
;;      API must return a list of java AnalyzedLabel objects

(defn- -parse-labels-document [labels modules label-type start-id]
  (as-> labels $$
    (map preprocess-label $$)
    (map #(.tokenize (modules :tokenizer) %) $$)
    (mapv lowercase-capitalized $$)
    (map #(.split (modules :splitter) %) $$)
    ;; NOTE: We set the same sentence ID for all sentences in a label.
    ;; This is OK because we assume all labels consists of exactly one
    ;; sentence and discard the extra ones. If this is no longer true,
    ;; sentence Ids will need to be set in a different way
    (map #(set-sentencelist-id %1 %2) $$ (range start-id (+ start-id (count $$))))
    (map #(analyze-with (modules :maco) %) $$)
    (map #(analyze-with (modules :tagger) %) $$)
    (only-if-type "task" (map #(analyze-with (modules :parser) %) $$))
    (only-if-type "task" (mapv #(fl-forall select-best-seq %) $$))
    (reduce (fn [doc ls] (.pushBack doc (Paragraph. ls)) doc)
            (Document.)
            $$)
    (analyze-with (modules :senses) $$)
    (analyze-with (modules :ukb) $$)))

(defn -parse-labels-json [document modules label-type start-id]
  (as-> document $$
    (((output-fn "en") "json") $$)
    (clojure.walk/keywordize-keys (json/read-str $$))
    (mapv (fn [paragraph]
            (if (> (count paragraph) 1)
              (printf "[WARNING]: Found label with more than one sentence."
                      "Ignoring all sentences but the first one."))
            (first (:sentences paragraph)))
          (:paragraphs $$))
    (only-if-type "task" (map extract-predicate $$))))

(defn- -parse-labels [labels modules label-type start-id]
  (as-> labels $$
    (map preprocess-label $$)
    (map #(.tokenize (modules :tokenizer) %) $$)
    (mapv lowercase-capitalized $$)
    (map #(.split (modules :splitter) %) $$)
    ;; NOTE: We set the same sentence ID for all sentences in a label.
    ;; This is OK because we assume all labels consists of exactly one
    ;; sentence and discard the extra ones. If this is no longer true,
    ;; sentence Ids will need to be set in a different way
    (map #(set-sentencelist-id %1 %2) $$ (range start-id (+ start-id (count $$))))
    (map #(analyze-with (modules :maco) %) $$)
    (map #(analyze-with (modules :tagger) %) $$)
    (only-if-type "task" (map #(analyze-with (modules :parser) %) $$))
    (only-if-type "task" (mapv #(fl-forall select-best-seq %) $$))
    (reduce (fn [doc ls] (.pushBack doc (Paragraph. ls)) doc)
            (Document.)
            $$)
    (analyze-with (modules :senses) $$)
    (analyze-with (modules :ukb) $$)
    (((output-fn "en") "json") $$)
    (clojure.walk/keywordize-keys (json/read-str $$))
    (mapv (fn [paragraph]
            (if (> (count paragraph) 1)
              (println "[WARNING]: Found label with more than one sentence."
                       "Ignoring all sentences but the first one."))
            (first (:sentences paragraph)))
          (:paragraphs $$))
    (only-if-type "task" (map extract-predicate $$))))

(defn id->int [id]
  (Integer/parseInt (nth (re-matches #"t(\d+).(\d+)" id) 2)))

(defrecord AnalyzedLabelImpl [sentence, sentence-json]
  AnalyzedLabel
  (getSentence [this] (:sentence this))
  (getHeadOf [this argument-type]
    (let [argk (keyword argument-type)]
      (-> sentence-json :label_analysis argk :head id->int)))
  (getWordsOf [this argument-type]
    (let [argk (keyword argument-type)]
      (mapv id->int
            (-> sentence-json :label_analysis argk :words)))))


(defn -parse-labels-java [document, json]
  (let [paragraphs (fl->seq document)
        sentences (map #(first (fl->seq %)) paragraphs)]
    (map ->AnalyzedLabelImpl sentences json)))

(def ^{:private true} modules-cache (atom {}))

(defn parse-labels [labels lang & {:keys [label-type, start-id]
                                   :or {label-type "task"
                                        start-id 1}}]
  (init-freeling)
  (let [modules (get @modules-cache lang)]
    (if-not modules
      (do (swap! modules-cache assoc lang
                 (make-modules (config/-getConfig "freeling-dir-path") lang))
          (parse-labels labels lang))
      (-> labels
          (-parse-labels-document modules label-type start-id)
          (-parse-labels-json modules label-type start-id)))))

(defn parse-labels-java [labels lang & {:keys [label-type]
                                        :or {label-type "task"}}]
  (init-freeling)
  (let [modules (get @modules-cache lang)]
    (if-not modules
      (do (swap! modules-cache assoc lang
                 (make-modules (config/-getConfig "freeling-dir-path") lang))
          (parse-labels-java labels lang))
      (let [doc (-parse-labels-document labels modules label-type 1)
            json (-parse-labels-json doc modules label-type 1)]
        (-parse-labels-java doc json)))))

(comment
  BEGIN
  (init-freeling)

  (def --parsed-labels
    (parse-labels ["decide about document" "deliver zooclub magazine"] "en" :label-type "task" :start-id 10))

  (def --parsed-labels-java
    (parse-labels-java ["decide about document" "deliver zooclub magazine"] "en" :label-type "task" ))

  (def --tree (first (:constituents (first --parsed-labels))))

  --tree

  (get-head (second (:children --tree)))

  (get-words (second (:children --tree)))

  (extract-predicate (first --parsed-labels))


  (spit "/tmp/test.json" --parsed-labels)

  (.pushBack --doc (first --parsed-labels))

  (defn --test [sentence]
    (map #(.getLabel (.begin (.getParseTree sentence %)))
         (range (.numKbest sentence))))

  (def --result
    (let [{:keys [tokenizer splitter maco tagger parser]} --modules
          result (->> "check and suggest updates"
                      (.tokenize tokenizer)
                      (.split splitter)
                      (analyze-with maco)
                      (analyze-with tagger)
                      (analyze-with parser))]
      result))

  (fl->seq --result)

  (def --result2 (lsentence-map select-best-seq --result))

  (def --sentences (it->vec (ListSentenceIterator. --result2)))

  (def --sentence (nth --sentences 0))

  (.getBestSeq --sentence)

  (map #(.getLemma %) (it->vec (ListWordIterator. --sentence)))

  END)

(comment
  "Parse all labels from the dataset"

  (def --labels-dataset
    (clojure.string/split (slurp "/home/josep/labels_all.csv") #"\n"))

  (def --parsed-labels (parse-labels --labels-dataset "en"))

  ;; 93% of labels matched some pattern!
  (float (/ (count (filter :label_analysis --parsed-labels))
            (count --parsed-labels)))

  (defn print-analysis [{analysis :label_analysis :as sentence}]
    (let-def [get-lemma (fn [t-id] (select [:tokens ALL #(= (:id %) t-id) :lemma]
                                           sentence))
              format-wordlist (fn [[word & rest]]
                                (apply str word
                                       (for [s rest]
                                         (str ", " s))))]
      (str 
       (-> analysis :predicate :head get-lemma format-wordlist) ";"
       (-> analysis :object :head get-lemma format-wordlist) ";"
       (-> analysis :complement :head get-lemma format-wordlist)"")))

  (->> --parsed-labels
       (map print-analysis)
       (clojure.string/join "\n")
       (spit "/home/josep/parsed_labels.csv"))

  END)
