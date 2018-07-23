(ns edu.upc.modelvsdocument.core
  (:use [com.rpl.specter]
        [clojure.pprint]
        [edu.upc.modelvsdocument.utils])
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [clojure.string :as string]
            [clojure.set :as set]
            [incanter.infix :refer [formula]]
            [clj-pipeline.core :as pipeline :refer [defpipe-input defpipe-output defpipe-step
                                                    run-pipeline env]]
            [edu.upc.modelvsdocument.text :as text]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.bpmn-alignable :as bpmn-al]
            [edu.upc.modelvsdocument.config :as config :refer [config with-config]]
            [edu.upc.modelvsdocument.gt-structure :as gt-structure]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.modelvsdocument.sorter.model-sorter :as model-sorter]
            [edu.upc.modelvsdocument.sorter.text-sorter :as text-sorter]
            [edu.upc.modelvsdocument.utils :as utils]
            [edu.upc.modelvsdocument.bpmn-analysis :as bpmn-eanalysis]
            [edu.upc.modelvsdocument.extraction.text-extraction :as textextraction]
            [edu.upc.modelvsdocument.extraction.model-extraction :as modelextraction]
            [edu.upc.modelvsdocument.extraction.model-extraction-old :as modelextraction-old]
            [edu.upc.modelvsdocument.extraction.feature :as f]
            [edu.upc.modelvsdocument.model-fuzzier :as fuzz]
            [edu.upc.modelvsdocument.similarity :as similarity]
            [edu.upc.modelvsdocument.predictors :as predictors]
            [edu.upc.modelvsdocument.log :as log]
            [edu.upc.modelvsdocument.solver :as solver]
            [edu.upc.modelvsdocument.agents-matching :as ag-match]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.verification.groundtruth :as groundtruth]
            [clojure.java.io :as io])
  (:import [edu.upc.modelvsdocument.extraction.feature Feature]
           [edu.upc.modelvsdocument ModelManager SentenceIdsPair Result Match]
           [org.activiti.bpmn.model Task StartEvent EndEvent UserTask SequenceFlow BpmnModel]
           #_[similaritycomputation.main SimilarityComputer] #_"NOTE: Defined below under an if-class macro")
  (:gen-class))

(def ^:dynamic p-list "foo")
(def ^:dynamic p-neighbour "bar")

(defpipe-step nop-step []
  {})

(defpipe-input files-input [model-file text-file]
  {:model-path (.getAbsolutePath model-file)
   :text (slurp text-file)})

(defpipe-input path-and-text-input [model-path text]
  (env model-path text))

(defpipe-step analyze-text [text]
  (let [text-res (text/analyze :text text :lang "en")
        text-alignables (a/get-all-alignables text-res)
        S (count text-alignables)]
    (env text-res, text-alignables, S)))

(defpipe-step analyze-model [model-path text]
  (let [model (bpmn-al/analyze (bpmn/read-model model-path) text)
        model-alignables (a/get-all-alignables model)
        T (count model-alignables)]
    (env model, model-alignables, T)))

(defpipe-step extract-features-old [text-res model]
  {:text-features (textextraction/extract-features text-res)
   :model-features (modelextraction-old/extract-features model)})

(defpipe-step extract-features [text-res model]
  {:text-features (textextraction/extract-features text-res)
   :model-features (modelextraction/extract-features model)})

(defpipe-step compute-similarities [text-features, text-alignables
                                    model-alignables, model-features]
  {:similarities
   (reduce
    (fn [similarities [s t]]
      (let [sim (float (similarity/default-similarity (text-features s)
                                                      (model-features t)))]
        (-> similarities (assoc-in [s t] sim) (assoc-in [t s] sim))))
    {}
    (for [s text-alignables, t model-alignables] [s t]))})

(if-class similaritycomputation.main.SimilarityComputer
          (do
            (import '[similaritycomputation.main SimilarityComputer])
            (defpipe-step compute-vu-similarities [model-alignables text-alignables
                                                   text-res T S]
              (do
                (when (config :enable-anchors)
                  (throw (Exception. "Anchors enabled but using VU similarity.")))
                (let [task-labels-map
                      (java.util.TreeMap.
                       (mapify (map (fn [task] [(.getId task) (.getName task)])
                                    model-alignables)))
                      result (.run (SimilarityComputer. (:text text-res)
                                                        task-labels-map))
                      scores (.getScores result)
                      vu-similarities (reduce
                                       (fn [similarities [s t sim]]
                                         (-> similarities
                                             (assoc-in [s t] sim)
                                             (assoc-in [t s] sim)))
                                       {}
                                       (for [i (range S), j (range T)
                                             :let [s (nth text-alignables i)
                                                   t (nth model-alignables j)
                                                   sim (-> scores (.get i) (.get j))]]
                                         [s t sim]))]
                  {:similarities vu-similarities}))))
          (defpipe-step compute-vu-similarities [model-alignables text-alignables
                                                 text-res T S]
            (throw (Exception. "[ERROR]: VU Similarity is set in modelvsdocument's config but is not available in this build."))))

(defpipe-step compute-text-order [text-res]
  {:sentence-order (text-sorter/build-order-matrix text-res)})

(defpipe-step remove-back-edges []
  {:remove-back-edges? true})

(defpipe-step compute-model-order [model remove-back-edges?]
  {:task-order (model-sorter/build-ordering model :remove-back-edges? remove-back-edges?)})

(defpipe-step compute-alignment [model text-res similarities task-order
                                 sentence-order model-alignables text-alignables]
  (let [alignment (solver/solve-with-gurobi model text-res similarities task-order sentence-order)
        alignment (let [unassigned-tasks (set/difference (set model-alignables)
                                                         (set (map second alignment)))]
                    (sort-by #(-> % second a/id) (concat alignment (zip (repeat (first text-alignables)) unassigned-tasks))))]

    (env alignment)))

(defpipe-step compute-trivial-alignment [model text-res similarities task-order
                                         sentence-order model-alignables text-alignables]
  (let [trivial-alignment (with-config {:disable-restrictions true}
                            (solver/solve-with-gurobi model text-res similarities task-order sentence-order))
        trivial-alignment (let [unassigned-tasks (set/difference (set model-alignables)
                                                                 (set (map second trivial-alignment)))]
                            (sort-by #(-> % second a/id) (concat trivial-alignment (zip (repeat (first text-alignables)) unassigned-tasks))))]
    (env trivial-alignment)))

(defpipe-step compute-max-constrained [similarities alignment trivial-alignment]
  {:max-constrained (let [alignment (sort-by #(a/id (first %)) alignment)
                          trivial-alignment (sort-by #(a/id (first %)) trivial-alignment)]
                      (apply max
                             (for [[[s t] [s' t]] (zip alignment trivial-alignment)
                                   :when (bpmn/task? t)]
                               (- (get-in similarities [s' t])
                                  (get-in similarities [s  t])))))})

(defpipe-step apply-wrong-order-correction [max-constrained alignment trivial-alignment]
  (if (> max-constrained (config :max-constrained-threshold))
    {:alignment trivial-alignment}
    {}))

(comment
  (def --result (with-config {:enable-cutoff true}
                  (main-with-files (io/file "/home/josep/test/model.bpmn")
                                   (io/file "/home/josep/test/text.txt"))))

  (transform
   (walker a/alignable?)
   a/label
   (:similarities (.getClojureExtraData --result)))

  (transform
   (walker a/alignable?)
   a/label
   (setval
    [ALL #(-> % first text/dummy?) LAST]
    ::predictors/missing
    (:alignment (.getClojureExtraData --result)))  )

  


  END)

(defpipe-step similarity-hack [similarities]
  {:similarities
   (setval
    (multi-path [ALL
                 (fn [[k v]] (text/dummy? k))
                 LAST MAP-VALS]
                [MAP-VALS ALL (fn [[k v]]
                                (text/dummy? k))
                 LAST])
    0.001
    similarities)})


;;ONLY EXECUTED IF (config :enable-cutoff)
(defpipe-step predictors-cutoff [alignment similarities]
  (let [similarities' (setval
                       (multi-path [MAP-KEYS bpmn/gateway?]
                                   [MAP-VALS MAP-KEYS bpmn/gateway?])
                       NONE
                       similarities)
        [gw-alignment ac+ev-alignment] (span #(-> % second bpmn/gateway?) alignment)
        __ (do (def --gw-alignment gw-alignment)
               (def --ac+ev-alignment ac+ev-alignment)
               (def --similarities' similarities'))
        ac+ev-alignment (predictors/cutoff-alignment (config :missing-predictor-type)
                                                     :max-constrained
                                                     (config :threshold-missing)
                                                     (config :threshold-misplaced)
                                                     similarities' ac+ev-alignment)]
    {:alignment (concat gw-alignment ac+ev-alignment)}))

(defpipe-step aligned-to-dummy-means-missing [alignment]
  (let [alignment-before alignment
        alignment-after (setval
                         [ALL FIRST text/dummy?]
                         ::predictors/missing
                         alignment)]
    (def --before alignment-before)
    (def --after alignment-after)
    {:alignment alignment-after}))

(defpipe-step classify-matches [alignment]
  (let [[bad-matches, good-matches]
        (span (fn [[s t]] (= ::predictors/missing s))
              alignment)]
    (env bad-matches good-matches)))

(defpipe-step compute-score [similarities good-matches S T]
  (let [score (float (* 2 (/ (reduce + (map (fn [[s t]] (get-in similarities [s t])) good-matches)) (+ S T))))]
    {:sentence-match-score score
     :total-score score}))

(defpipe-step compute-gt-structure [text-alignables model-alignables alignment]
  (gt-structure/compute-gt-structure text-alignables model-alignables alignment))

(defpipe-step compute-extended-gt-structure [text-alignables model-alignables alignment]
  (gt-structure/compute-extended-gt-structure text-alignables model-alignables alignment))

(defpipe-step alignment-cmp-def []
  {:alignment-cmp (fn [[s _]] (cond
                                (and (int? s) (>= s 0)) (-> s a/id Integer/parseInt)
                                (and (int? s) (== s -1)) Integer/MAX_VALUE
                                (= ::predictors/missing s) (dec (Integer/MAX_VALUE))))})

;; ONLY IF ENABLE LOG IS TRUE IN CONFIG


(defpipe-step compute-java-matches [alignment alignment-cmp text-features model-features
                                    similarities good-matches]
  (let [good-match? (set good-matches)]
    {:matches
     (mapv
      (fn [[s t]]
        (Match. (if (a/alignable? s) (a/id s) "0")
                (if (a/alignable? s) (a/label s) "Missing"),
                (a/id t), (a/label t)
                (mapv f/explain-feature
                      (f/fv-intersection (text-features s) (model-features t)))
                (if (a/alignable? s)
                  (get-in similarities [s t])
                  0)
                (= true (good-match? [s t]))))
      (sort-by alignment-cmp
               alignment))}))

(defpipe-output output-for-server
  [similarities matches new-match-struct max-constrained
   text-features model-features model-alignables
   text-alignables model text-res total-score log alignment
   extended-match-struct]
  (let+ [extra-data :dbg (env similarities matches new-match-struct max-constrained
                              text-features model-features model-alignables
                              text-alignables model text-res alignment
                              extended-match-struct)]
    (Result. total-score log matches
             extra-data)))

(defpipe-output output-for-experiments
  [similarities matches new-match-struct max-constrained
   text-features model-features model-alignables
   text-alignables model text-res total-score log alignment
   extended-match-struct]
  (let+ [extra-data :dbg (env similarities matches new-match-struct max-constrained
                              text-features model-features model-alignables
                              text-alignables model text-res alignment
                              extended-match-struct)]
    extra-data))

(defn standard-pipeline []
  [analyze-text
   analyze-model
   (if (config :use-old-label-parser)
     extract-features-old
     extract-features)
   (if (config :use-vu-similarity)
     compute-vu-similarities
     compute-similarities)
   (if (config :remove-back-edges?)
     remove-back-edges
     nop-step)
   similarity-hack ;;TODO: ???
   compute-text-order
   compute-model-order
   compute-alignment
   compute-trivial-alignment
   compute-max-constrained
   (if (config :enable-wrong-order-cutoff)
     apply-wrong-order-correction
     nop-step)
   (if (config :enable-cutoff)
     predictors-cutoff
     nop-step)
   (if (config :enable-cutoff)
     aligned-to-dummy-means-missing
     nop-step)
   classify-matches
   compute-score
   compute-gt-structure
   compute-extended-gt-structure
   alignment-cmp-def
   compute-java-matches
   (if (config :create-log)
     log/generate-log
     nop-step)])

(defn main-with-files [model-file, text-file]
  (apply
   run-pipeline
   [model-file text-file]
   (concat [files-input]
           (standard-pipeline)
           [output-for-server])))

(defn main [model-path text]
  (apply
   run-pipeline
   [model-path text]
   (concat [path-and-text-input]
           (standard-pipeline)
           [output-for-server])))
