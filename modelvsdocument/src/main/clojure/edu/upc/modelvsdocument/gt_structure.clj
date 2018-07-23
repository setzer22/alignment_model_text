(ns edu.upc.modelvsdocument.gt-structure
  (:require
   [com.rpl.specter :refer :all]
   [edu.upc.modelvsdocument.predictors :as predictors]
   [edu.upc.modelvsdocument.text :as text]
   [edu.upc.modelvsdocument.bpmn :as bpmn]
   [edu.upc.modelvsdocument.alignable :as a]
   [edu.upc.modelvsdocument.utils :as utils :refer :all]))

(defn compute-gt-structure [text-alignables model-alignables alignment]
  {:new-match-struct
   ;; TODO: If I implement predicates, they should be mapped back to sentences here
   ;; NOTE: For compatibility with the JSON groundtruth standard, sentences do not have
   ;;       ids and are considered in order of appearance.
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
                              (transform [ALL LAST] (constantly "m"))))})})

(defn valid-model-element? [element]
  (or
   (bpmn/task? element)
   (bpmn/split-gateway? element)
   (bpmn/named-event? element)))

(defn compute-extended-gt-structure [text-alignables model-alignables alignment]
  {:extended-match-struct
   (let [sentences (filter text/sentence? text-alignables)
         tasks (filter valid-model-element? model-alignables)]
     {:gt/tasks (vec (map a/id tasks))
      :gt/num-sentences (count sentences)
      :gt/alignment (mapify (->> alignment
                                 (filter #(-> % second valid-model-element?))
                                 (transform [ALL] (fn [[a b]] [b a]))
                                 (transform [ALL ALL a/alignable?] a/id)
                                 (transform [ALL LAST #(= ::predictors/missing %)] (constantly :gt/no-match))
                                 (transform [ALL LAST string?]
                                            #(dec (Integer/parseInt %)))
                                 ))
      :gt/errors (mapify (concat (->> alignment
                                      (filter #(-> % second valid-model-element?))
                                      (filter #(= ::predictors/missing (-> % first)))
                                      (transform [ALL] (fn [[a b]] [b a]))
                                      (transform [ALL FIRST] a/id)
                                      (transform [ALL LAST] (constantly "m")))))})})

(comment TEST


         (def --model-alignables (a/get-all-alignables  edu.upc.modelvsdocument.exec.alignment-editor/--model))

         (def --text-alignables (a/get-all-alignables edu.upc.modelvsdocument.exec.alignment-editor/--text))

         (def --alignment (edu.upc.modelvsdocument.exec.alignment-editor/alignment-fix
                           @edu.upc.modelvsdocument.exec.alignment-editor/--alignment
                           --model-alignables))

         END)

