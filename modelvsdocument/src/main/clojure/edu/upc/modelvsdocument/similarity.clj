(ns edu.upc.modelvsdocument.similarity
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.extraction.text-extraction :as txt]
            [incanter.core :as mth]
            [clojure.set :as set])
  (:use [clojure.pprint]
        [edu.upc.modelvsdocument.extraction.feature]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.config])
  (:import [edu.upc.modelvsdocument.extraction.feature Feature])
  (:gen-class))

(spec-fn magnitude ::t/feature-vector -> number?)
(defn magnitude [v]
  "Computes the magnitude for the given feature vector"
  (Math/sqrt (reduce (fn [acc, {w :weight}] (+ acc (* w w))) 0 v)))

(spec-fn normalize-fv ::t/feature-vector -> (spec/and ::t/feature-vector #(== 1.0 (magnitude %))))
(defn normalize-fv [v]
  "Normalize-Fvs the given feature vector"
  (let [len (magnitude v)] 
    (map (fn [{w :weight :as feature}] (assoc feature :weight (/ w len))) v)))

(do  
  (spec-fn cosine-similarity ::t/feature-vector ::t/feature-vector -> number?)
  (defn cosine-similarity [v1 v2]
    "Computes the cosine similarity between two feature vectors"
    (loop [[{x-weight :weight :as x} & xs :as fv1] (normalize-fv (sort v1)), 
           [{y-weight :weight :as y} & ys :as fv2] (normalize-fv (sort v2)), 
           sum 0]
      (if (or (nil? x) (nil? y))
        (if (> sum 1) 1 sum) ; Result might be > than 1 because FP precision error
        (comp-if (compare x y)
                 (recur xs fv2 sum)
                 (recur xs ys (+ sum (* x-weight y-weight)))
                 (recur fv1 ys sum)))))

  (spec-fn jaccard-similarity ::t/feature-vector ::t/feature-vector -> number?)
  (defn jaccard-similarity [v1 v2]
    (/ (count (fv-intersection v1 v2))
       (count (fv-union v1 v2))))

  (spec-fn fake-jaccard ::t/feature-vector ::t/feature-vector -> number?)
  (defn fake-jaccard [v1 v2]
    (let [sh (shortest v1 v2)
          ln (longest v1 v2)]
      (/ (count (fv-intersection sh ln))
         (count sh))))

  (spec-fn raw-weighted-overlapping ::t/feature-vector ::t/feature-vector -> number?)
  (defn raw-weighted-overlapping [v1 v2]
    (let [I (reduce + (map :weight (fv-intersection v1 v2)))]
      I))

  (spec-fn weighted-fake-jaccard ::t/feature-vector ::t/feature-vector -> number?)
  (defn weighted-fake-jaccard [v1 v2]
    (let [sh (shortest v1 v2)
          ln (longest v1 v2)
          S (reduce + (map :weight sh))
          I (reduce + (map :weight (fv-intersection sh ln)))]
      (if (== S 0)
        0
        (/ I S))))

  (spec-fn weighted-jaccard-similarity ::t/feature-vector ::t/feature-vector -> number?)
  (defn weighted-jaccard-similarity [v1 v2]
    (let [U (reduce + (map :weight (fv-union v1 v2)))
          I (reduce + (map :weight (fv-intersection v1 v2)))]
      (/ I U)))

  (def similarity-functions
    {:jaccard jaccard-similarity
     :fake-jaccard fake-jaccard
     :weighted-fake-jaccard weighted-fake-jaccard
     :weighted-jaccard weighted-jaccard-similarity
     :raw-weighted-overlapping raw-weighted-overlapping
     :cosine cosine-similarity})

  (spec-fn default-similarity ::t/feature-vector ::t/feature-vector -> number?)
  (defn default-similarity [v1, v2]
    (if (or (= (count v1) 0) (= (count v2) 0)) 
      (do (binding [*out* *err*] 
            (comment ; TODO Uncomment warning
              (println "WARNING: Comparing two similarity vectors and at least one is empty. This should never happen."
                       "\n" "Vector1: " v1
                       "\n" "Vector2: " v2))
            0 ; Avoid a division by zero and discard the feature.
            ))
      (((keyword (:similarity-function config)) similarity-functions) v1 v2))))

