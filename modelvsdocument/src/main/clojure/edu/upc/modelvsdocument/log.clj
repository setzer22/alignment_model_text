(ns edu.upc.modelvsdocument.log
  (:require [clj-pipeline.core :as pipeline :refer [defpipe-input defpipe-output defpipe-step run-pipeline env]]
            [edu.upc.modelvsdocument.predictors :as predictors]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.text :as text]
            [edu.upc.modelvsdocument.config :as config :refer [config with-config]]
            [edu.upc.modelvsdocument.extraction.feature :as f]
            [edu.upc.modelvsdocument.alignable :as a]))

(defn print-title [title]
  (println (apply str (take (count title) (repeat "="))))
  (println title)
  (println (apply str (take (count title) (repeat "=")))))

(defn print-subtitle [title]
  (println (str title ":"))
  (println (apply str (take (inc (count title)) (repeat "-")))))

(defmacro section [title & body]
  (concat `(do) `((print-title ~title)) body `((println ""))))

(defmacro subsection [title & body]
  (concat `(do) `((print-subtitle ~title)) body `((println ""))))

(defpipe-step generate-log [good-matches model-alignables text-alignables
                            alignment similarities model text-res text-features
                            model-features alignment-cmp]
  (let [good-match? (set good-matches)
        model-display? (set (filter bpmn/task? model-alignables))
        text-display?  (set (filter text/sentence? text-alignables))
        print-sentence (fn [s]
                         (cond
                           ;; This means s is a match, and s is the matching sentence id
                           (a/alignable? s) (println (str "Sentence " (a/id s) " \"" (a/label s) "\""))
                           ;; This means the task is missing
                           (= ::predictors/missing s) (println "The element was not found in the text")
                           :else (println "ERROR: This should never be printed" s)))
        print-task     (fn [t] (println (str (a/id t) " \"" (a/label t model) "\"")))
        print-features (fn [features] (doseq [f features] (println (str "    * " (f/explain-feature f)))))]

    ;; Sort the matches by sentence, but keep the dummy matches and missing tasks at the end
    {:log
     (when (config :create-log)
       (with-out-str
         (section "Matching Summary"
                  (doseq [[s t] (sort-by alignment-cmp good-matches #_alignment)
                          :when (and (model-display? t) (text-display? s))]
                    (println (str (a/id t) " \"" (a/label t model) "\" <-> Sentence " (a/id s) " \"" (a/label s) "\""))))
         (section "Detailed Matchings"
                  (doseq [[s t] (sort-by alignment-cmp alignment)
                          :when (model-display? t)]
                    (let [common-features (f/fv-intersection (text-features s) (model-features t))]
                      (print-task t)
                      (print-sentence s)
                      (print-features common-features)
                      (println ""))))
         (section "Gateway Matchings"
                  (doseq [[s t] (sort-by alignment-cmp alignment)
                          :when (not (model-display? t))]
                    (let [common-features (f/fv-intersection (text-features s) (model-features t))]
                      (print-task t)
                      (print-sentence s)
                      (print-features common-features)
                      (println ""))))
         (section "Sentences"
                  (doseq [s text-alignables :when (text-display? s)]
                    (print-sentence s)
                    (print-features (text-features s))
                    (println "")))
         (section "Tasks"
                  (doseq [t model-alignables :when (model-display? t)]
                    (print-task t)
                    (print-features (model-features t))
                    (println "")))
         (section "Gateways"
                  (doseq [t model-alignables :when (not (model-display? t))]
                    (print-task t)
                    (print-features (model-features t))
                    (println "")))))}))

