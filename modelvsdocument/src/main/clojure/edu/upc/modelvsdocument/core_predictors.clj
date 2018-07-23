(ns edu.upc.modelvsdocument.core-predictors
  (:use [com.rpl.specter]
        [clojure.pprint]
        [edu.upc.modelvsdocument.utils])
  (:require [edu.upc.modelvsdocument.core :as core]
            [clj-pipeline.core :as pipeline :refer [defpipe-input defpipe-output defpipe-step
                                                    run-pipeline env]]
            [edu.upc.modelvsdocument.predictors :as predictors]
            [edu.upc.modelvsdocument.gt-structure :as gt-structure]
            [edu.upc.modelvsdocument.text :as text]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.bpmn :as bpmn])
  (:gen-class))

(defpipe-input input-thresholds-and-predictor-types
  [model-file text-file predictor-types thresholds]
  (merge
   (env predictor-types thresholds)
   {:model-path (.getAbsolutePath model-file)
    :text (slurp text-file)}))

(defpipe-step cut-alignments [predictor-types alignment similarities thresholds]
  {:cut-alignments
   (mapify
    (for [predictor-type predictor-types]
      [predictor-type
       (map
        (fn [threshold]
          (let [similarities' (setval
                               (multi-path [MAP-KEYS bpmn/gateway?]
                                           [MAP-VALS MAP-KEYS bpmn/gateway?])
                               NONE
                               similarities)
                [gw-alignment ac+ev-alignment] (span #(-> % second bpmn/gateway?) alignment)
                ac+ev-alignment (predictors/cutoff-alignment predictor-type :max-constrained
                                                             threshold threshold
                                                             similarities alignment)]
            (concat gw-alignment ac+ev-alignment)))
        thresholds)]))})

(defpipe-output output-comparisons [cut-alignments text-alignables model-alignables
                                    similarities]
  (let+ [similarities :dbg similarities]
    (transform [MAP-VALS ALL]
               (fn [alignment]
                 (:extended-match-struct
                  (gt-structure/compute-extended-gt-structure text-alignables model-alignables alignment)))
               #_(fn [alignment]
                   (let [[bad-matches, good-matches]
                         (span (fn [[s t]] (= ::predictors/missing s))
                               alignment)]
                     (let [sentences (filter text/sentence? text-alignables)
                           tasks     (filter bpmn/task? model-alignables)]
                       {:gt/tasks         (vec (map a/id tasks))
                        :gt/num-sentences (count sentences)
                        :gt/alignment     (mapify (->> alignment
                                                       (filter #(-> % second bpmn/task?))
                                                       (transform [ALL] (fn [[a b]] [b a]))
                                                       (transform [ALL ALL a/alignable?] a/id)
                                                       (transform [ALL LAST #(= ::predictors/missing %)]
                                                                  (constantly :gt/no-match))
                                                       ;; NOTE: Dummy sentence will get an id of -1
                                                       ;; NOTE: Groundtruth ids are 0-based, but freeling ids are 1-based
                                                       (transform [ALL LAST string?]
                                                                  #(dec (Integer/parseInt %)))))
                        :gt/errors (mapify (->> alignment
                                                (filter #(-> % second bpmn/task?))
                                                (filter #(= ::predictors/missing (-> % first)))
                                                (transform [ALL] (fn [[a b]] [b a]))
                                                (transform [ALL FIRST] a/id)
                                                (transform [ALL LAST] (constantly "m"))))})))
               cut-alignments)))

(defn main-with-files [model-file text-file predictor-types thresholds]
  (run-pipeline
   [model-file text-file predictor-types thresholds]
   input-thresholds-and-predictor-types
   core/analyze-text
   core/analyze-model
   core/extract-features
   core/compute-similarities
   core/compute-text-order
   core/compute-model-order
   core/compute-alignment
   cut-alignments
   core/aligned-to-dummy-means-missing
   output-comparisons))

(comment

  (main-with-files
   (clojure.java.io/file "/home/josep/ModelsBpmn/Zoo.bpmn")
   (clojure.java.io/file "/home/josep/ModelsBpmn/Zoo.txt")
   [:p-sim :S-diff]
   [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9])

  END)
