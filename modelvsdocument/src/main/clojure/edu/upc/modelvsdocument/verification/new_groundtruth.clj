(ns edu.upc.modelvsdocument.verification.new-groundtruth
  (:require [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [clojure.set :as set]
            [clojure.spec :as spec]
            [clojure.data.json :as json]
            [com.rpl.specter :as specter]))

(spec/def :gt/task-id string?)

(spec/def :gt/sentence-id int?)

(spec/def :gt/tasks (spec/* :gt/task-id))

(spec/def :gt/num-sentences int?)

(spec/def :gt/no-match #(= % :gt/no-match))

(spec/def :gt/alignment
  (spec/map-of :gt/task-id (spec/or :found :gt/sentence-id
                                    :not-found :gt/no-match)))

(spec/def :gt/error string?)

(spec/def :gt/errors (spec/map-of :gt/task-id :gt/error))

(defn good-sentence-range [{:keys [:gt/alignment :gt/num-sentences] :as wat}]
  (println wat)
  (set/subset? (set (vals alignment))
               (set (range num-sentences))))

(defn all-tasks-defined [{:keys [:gt/tasks :gt/alignment]}]
  (= (sort tasks)
     (sort (vals alignment))))

(spec/def :gt/groundtruth
  (spec/keys :req [:gt/tasks :gt/num-sentences :gt/alignment]))


(defn export-json [edn]
  (json/write-str edn))

(defn keywordize-1 [map]
  (reduce
   (fn [m [k v]] (assoc m (keyword k) v))
   {}
   map))

(defn load-json [json-str]
  (->> (json/read-str json-str)
       (keywordize-1)
       (specter/transform
        (specter/walker keyword?)
        (fn [k] (keyword "gt" (name k))))
       (specter/transform
        (specter/walker #{"error-repeated" "not-found" "no-match" "wrong-order"}) #(keyword "gt" %))))

(defn compare-alignments [alignments errors]
  (let [pair-lists (map #(sort-by first (vec %)) alignments)
        zipped-pairs (apply map vector pair-lists)
        ;; Transform zipped pairs so :gt/no-match is transformed by the actual error
        zipped-pairs (map
                      (fn [row]
                        (vec
                         (map-indexed
                          (fn [idx [t-id s-id]]
                            ;; NOTE: Only "m" errors are treated as actual errors for now.
                            (if (= s-id :gt/no-match) [t-id ((nth errors idx) t-id)]
                                #_else [t-id s-id]))
                          row)))
                      zipped-pairs)
        correct-pairs (map #(apply = %) zipped-pairs)
        differences (into [] (remove nil? (map #(if %2 nil %1) zipped-pairs correct-pairs)))]
    {:accuracy (if (pos? (count zipped-pairs))
                 (/ (count (filter identity correct-pairs))
                    (count zipped-pairs))
                 "NA")
     :correct (count (filter identity correct-pairs))
     :total (count zipped-pairs)
     :differences differences}))

(defn compare-groundtruths
  "Compares any number of groundtruth objects, printing the values at which any of them differ"
  [& groundtruths]
  ;;TODO: Check assumption: All the groundtruths are from the same case:
  ;; -> They have the same mappings (maybe to different sentences)
  ;;TODO: What do we do with missing values?
  (assert (apply = (map #(sort (:gt/tasks %)) groundtruths))
          (apply str "All groundtruths must use the same set of task ids, but they differ:\n"
                 (map #(str "- " (into [] (sort (:gt/tasks %))) "\n") groundtruths)))
  (let [alignments (map :gt/alignment groundtruths)
        errors (map :gt/errors groundtruths)]
    (compare-alignments alignments errors)))

(defn keep-element
  [model pred groundtruth]
  (def --model1 model)
  (def --pred pred)
  (def --gt groundtruth)

  (let [pred #(not (pred (bpmn/get-element % model)))]
    (specter/setval (specter/multi-path
                     [:gt/tasks specter/ALL pred]
                     [:gt/alignment specter/ALL #(-> % first pred)]
                     [:gt/errors specter/ALL #(-> % first pred)])
                    specter/NONE
                    groundtruth)))

(defn keep-tasks "Removes all non-task elements from the given groundtruth" [model groundtruth]
  (keep-element model bpmn/task? groundtruth))

(defn keep-gateways "Removes all non-task elements from the given groundtruth" [model groundtruth]
  (keep-element model bpmn/gateway? groundtruth))

(defn keep-events "Removes all non-task elements from the given groundtruth" [model groundtruth]
  (keep-element model bpmn/event? groundtruth))

(defn compare-extended-groundtruths
  "Same as `compare-groundtruths`, but takes extended groundtruths instead."
  [anchors? model & ext-groundtruths]
  (if anchors?
    {:all (apply compare-groundtruths ext-groundtruths)
     :task (apply compare-groundtruths (map #(keep-tasks model %) ext-groundtruths))
     :gateway (apply compare-groundtruths (map #(keep-gateways model %) ext-groundtruths))
     :event (apply compare-groundtruths (map #(keep-events model %) ext-groundtruths))}
    {:task (apply compare-groundtruths (map #(keep-tasks model %) ext-groundtruths))}))

(comment
  "Verify all stored groundtruths"
  (let [path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth/"
        json-files (.listFiles (java.io.File. path))
        jsons (map #(load-json (slurp %)) json-files)]
    (filter identity
            (map #(and
                   (good-sentence-range %)
                   (all-tasks-defined %)
                   (spec/valid? :gt/groundtruth %)) jsons))))

(comment
  "Comparison test"
  (compare-groundtruths
   edu.upc.modelvsdocument.repl.comparison-queries/--gt
   (load-json (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth/Dispatch-of-goods.json")))

  )
