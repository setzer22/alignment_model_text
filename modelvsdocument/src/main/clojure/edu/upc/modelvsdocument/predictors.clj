(ns edu.upc.modelvsdocument.predictors
  (:require [edu.upc.modelvsdocument.utils :refer :all]
            [clojure.math.combinatorics :as cmb]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.text :refer [sentence?]]
            [edu.upc.modelvsdocument.bpmn-alignable :refer [flow-element?]]
            [clojure.spec :as spec]
            [edu.upc.modelvsdocument.schemas :as t]
            [com.rpl.specter :as specter :refer :all]))

(defmulti predictor
  "A predictor is a function receiving a similarity matrix and
   returning a matrix of predictor scores. The first argument
   is always the predictor type, and it is the dispatch value
  [disp-type, similarities, alignment task] -> similarities"
  (fn [type, similarities, alignment t] type))

(defn- t-sim' [similarities alignment t]
  (let [[s, _] (find-in-seq alignment #(= t (second %)))]
    (get-in similarities [s t])))

(defn- s-sim' [similarities alignment s]
  (let [[_, t] (find-in-seq alignment #(= s (first %)))]
    (get-in similarities [s t])))

(defmacro defpredictor
  "Defines a predictor multimethod. The body is enclosed in a context where
   some useful variables and functions are implicitly defined."
  [name & body]
  `(defmethod predictor ~name
     [~'_, ~'similarities, ~'alignment, ~'t]
     (let [~'sentences (filter sentence? (keys ~'similarities))
           ~'tasks (filter flow-element? (keys ~'similarities))
           ~'t-sim (fn [t#] (t-sim' ~'similarities ~'alignment t#))
           ~'s-sim (fn [s#] (t-sim' ~'similarities ~'alignment s#))
           ~'corresp-t (fn [s#] (second (find-in-seq ~'alignment #(= s# (first %)))))
           ~'corresp-s (fn [s#] (first (find-in-seq ~'alignment #(= s# (second %)))))]
       ~@body)))

(defpredictor :p-sim
  ;; The likelihood that activity a represents a missing activity, given as the
  ;; similarity score sim(a, s) of the correspondence a ∼ s ∈ σ̂
  (t-sim t))

(defn div-or-zero [a b]
  (if (zero? b)
    0
    (/ a b)))

(defpredictor :p-rel
  ;; The ratio between sim(a,s) and global max sim for any a',s'
  (div-or-zero (t-sim t)
               (apply max (select [MAP-VALS MAP-VALS] similarities))))

(defpredictor :p-rel-T
  (div-or-zero (t-sim t)
               (apply max (vals (get similarities t)))))

(defpredictor :p-avg-T
  (div-or-zero (t-sim t)
               (avg (vals (get similarities t)))))

(defpredictor :p-avgN-T
  (div-or-zero (t-sim t)
               (reduce + (vals (get similarities t)))))

(defpredictor :p-rel-S
  (div-or-zero (t-sim t)
               (apply max (vals (get similarities (corresp-s t))))))

(defpredictor :p-avgN-S
  (div-or-zero (t-sim t)
               (reduce + (vals (get similarities (corresp-s t))))))

(defpredictor :p-avg-S
  (div-or-zero (t-sim t)
               (avg (vals (get similarities (corresp-s t))))))

(defpredictor :S-diff
  ;; The difference between sim(a, s) for a ∼ s ∈ σ̂ and the average similarity
  ;; score of a to sentences in S;"
  (- (t-sim t)
     (avg (map #(get-in similarities [% t]) sentences))))

(defpredictor :A-diff
  ;; The difference between sim(a, s) for a ∼ s ∈ σ̂ and the average similarity
  ;; scores of the correspondences in σ̂
  (- (t-sim t)
     (avg (for [[s t] alignment] (get-in similarities [s t])))))

(defn entropy [probs]
  (- (reduce + (map #(* % (Math/log %)) probs))))

(defpredictor :entropy-S
  (let [normalized-similarities (normalize
                                 (remove
                                  zero?
                                  (for [s sentences]
                                    (get-in similarities [s t]))))]
    (entropy normalized-similarities))) ;; General case

(defpredictor :entropy-ensemble
  (harmonic-mean
   [(predictor :entropy-S similarities alignment t)
    (predictor :p-rel-S similarities alignment t)]))

(defpredictor :students-missing
  (harmonic-mean
   [(predictor :p-rel-S similarities alignment t)
    (predictor :p-sim similarities alignment t)]))

(def ensemble-predictor-names (atom []))

(defmacro make-ensembles []
  (let [ensemble-combination-size 3
        predictors [:p-sim :p-rel :p-rel-S :p-rel-T :p-avg-S :p-avg-T]]
    (concat
     [`do]
     (doall
      (for [preds-subset (cmb/combinations predictors ensemble-combination-size)
            :let [pred-name (keyword (clojure.string/join "+" (map name preds-subset)))
                  preds-subset (vec preds-subset)]]
        (do
          (swap! ensemble-predictor-names conj pred-name)
          `(defpredictor ~pred-name
             (avg
              (map #(predictor % ~'similarities ~'alignment ~'t)
                   ~preds-subset)))))))))

;;(make-ensembles)

(defpredictor :max-constrained
  "Max-constrained")

(defpredictor :default
  (throw (Exception. (str "There is no predictor function registered as " _))))

(defn compute-predictor
  [predictor-type similarities alignment]
  (let [tasks (filter flow-element? (keys similarities))]
    (mapify
     (for [t tasks]
       [t (predictor predictor-type similarities alignment t)]))))

(defn cutoff-alignment
  [missing-type misplaced-type missing-threshold misplaced-threshold
   similarities alignment]
  (let [predictions-missing (compute-predictor missing-type similarities alignment)
        #_predictions-misplaced #_(compute-predictor misplaced-type similarities alignment)
        missing-tasks (set (keys-such-that #(< % missing-threshold) predictions-missing))
        #_misplaced-tasks #_(set (keys-such-that #(< % misplaced-threshold) predictions-misplaced))]
    (as-> alignment $$
      (remove (fn [[s t]] (or (missing-tasks t) #_(misplaced-tasks t))) $$)
      (concat $$ (map vector (repeat ::missing) missing-tasks))
      #_(concat $$ (map vector misplaced-tasks (repeat :misplaced))))))


(comment

  (def --result (edu.upc.modelvsdocument.core/main-with-files
                 (clojure.java.io/file "/home/josep/ModelsBpmn/Zoo.bpmn")
                 (clojure.java.io/file "/home/josep/ModelsBpmn/Zoo.txt")))

  (let [{:keys [similarities alignment model-alignables]}
        (.getClojureExtraData --result)]
    (predictor :entropy-S similarities alignment (first model-alignables)))

  )
