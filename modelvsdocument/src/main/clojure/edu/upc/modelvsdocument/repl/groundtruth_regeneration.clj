(ns edu.upc.modelvsdocument.repl.groundtruth-regeneration
  (:require [clojure.walk :as walk]
            [clojure.java.io :as io]
            [edu.upc.modelvsdocument.verification.groundtruth :as groundtruth]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [com.rpl.specter :as specter]
            [clojure.data.json :as json]
            [clojure.data.csv :as csv]))

"The current groundtruth validation scheme is not optimal as of now.
 We want to regenerate all the groundtruth files using a more suitable
 comparison format. One that's easily parsable and portable. For that
 we will be using the following JSON schema (converted from edn):"

{:gt/tasks ["Task_0vaxgaa" "Task_0e6hvnj" "Task_0s79ile" "Task_0jsoxba"
          "Task_12j0pib" "Task_05ftug5" "Task_0sl26uo"]
 :gt/num-sentences 7
 :gt/alignment
 {"Task_0vaxgaa" 1
  "Task_0e6hvnj" 3
  "Task_0s79ile" 3
  "Task_0jsoxba" 5
  "Task_12j0pib" 5
  "Task_05ftug5" 6
  "Task_0sl26uo" 7}}

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
        (specter/walker #{"error-repeated" "not-found" "no-match"}) #(keyword "gt" %))))

"While doing this, we want to get rid of the comparison by string distance
 we used to do with the older groundtruth files. In order to not waste time,
 we want to automatically generate all the matchings (s,t) that we can determine
 with certainty. For an (s,t) pair to be automatically translated, we need:

 1) The string distance between t's label in the modle and t's label in the groundtruth
    is exactly 1.
 2) No other task in the groundtruth has the label of task t."


;; Property 2
(defn repeated-values? [lst]
  (not= (count lst) (count (distinct lst))))

(defn has-repeated-task-labels? [groundtruth]
  (repeated-values? (vals (::groundtruth/tasks groundtruth))))

(do (def case-name nil)
    (def benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/")
    (def groundtruths-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/groundtruths/")
    (def groundtruth-string nil)
    (defn show-csv-table [groundtruth-string]
      (clojure.inspector/inspect-table (csv/read-csv groundtruth-string :separator \;)))
    (defn inspect-model [] (future (clojure.java.shell/sh "evince" (str "/home/josep/tmp/Models/" case-name ".pdf"))))
    (defn inspect-text [] (future (clojure.java.shell/sh "kate" (str benchmark-path "/" case-name ".txt"))))
    (defn inspect-csv [] (show-csv-table groundtruth-string)))

(defn ask-question [msg]
  (println msg)
  (loop [response (read-line)]
    (if (not (#{"yes" "no" "Y" "N" "y" "n" "model" "text" "csv"} response))
      (recur (read-line))
      (cond (#{"yes" "Y" "y"} response) true
            (#{"no" "N" "n"} response)  false
            (= "model" response) (do (inspect-model) (recur (read-line)))
            (= "text" response) (do (inspect-text) (recur (read-line)))
            (= "csv" response) (do (inspect-csv) (recur (read-line)))
            :else (recur (read-line))))))

(defn best-candidate [word candidates]
  (when (empty? candidates) (throw (Exception. "Candidate list is empty")))
  (apply min-key second (pair-with-r #(levenshtein (.toLowerCase word) (.toLowerCase %)) candidates)))

(defn find-perfect-match-or-ask [word candidates]
  (let [[best-candidate score] (best-candidate word candidates)]
    (if (not= score 0)
      (let [response (ask-question (str "Is \"" word "\" the same as \"" best-candidate"\""))]
        (if response best-candidate :gt/not-found))
      best-candidate)))

(defn find-id-mapping [m:ids m:labels g:ids g:labels]
  (let [m:label->id (mapify (zip m:labels m:ids))
        g:label->id (mapify (zip g:labels g:ids))]
    (mapify
     (mapv
      (fn [m:label]
        (let [g:label (find-perfect-match-or-ask m:label g:labels)]
          [(m:label->id m:label)
           (if (= g:label :gt/not-found) :gt/not-found (g:label->id g:label))]))
      m:labels))))

(defn translate-groundtruth [groundtruth model text]
  (when (or (has-repeated-task-labels? model)
            (repeated-values? (vals (::groundtruth/tasks groundtruth))))
    (do
      (println (str "[WARNING] The case \"" (subs text 0 35) "...\" "
                    "has repeated tasks values. Some values won't be properly translated"))
      nil))

  (let [m:ids    (bpmn/all-task-ids model)
        m:labels (map #(.getName %) (bpmn/all-tasks model))
        g:ids    (keys (::groundtruth/tasks groundtruth))
        g:labels (vals (::groundtruth/tasks groundtruth))

        g:repeated-indices (repeated-indices g:labels)
        m:repeated-indices (repeated-indices m:labels)

        g:repeated-id? (set (filter-indices g:ids g:repeated-indices))
        m:repeated-id? (set (filter-indices m:ids m:repeated-indices))

        m:id->g:id* (find-id-mapping m:ids m:labels g:ids g:labels)
        m:id->g:id  (reduce
                     (fn [m:id->g:id [m:id g:id]]
                       (cond
                         (g:repeated-id? g:id) (assoc m:id->g:id m:id :gt/error-repeated)
                         (m:repeated-id? m:id) (assoc m:id->g:id m:id :gt/error-repeated)
                         :else                 (assoc m:id->g:id m:id g:id)))
                     {}
                     m:id->g:id*)
        g:sentences (vals (::groundtruth/sentences groundtruth))
        m:sentences g:sentences;(textserver/split-sentences text)

        __ (if (not= (count m:sentences) (count g:sentences))
             (throw (Exception. (str "Fatal error. Sentence count mismatch: " m:sentences g:sentences))))

        map-or-err (fn [map] (fn [x] (cond (contains? map x)                       (map x)
                                           (#{:gt/error-repeated :gt/not-found} x) x
                                           :else                                   nil)))

        m:id->g:sentence (comp (map-or-err (::groundtruth/task->sentence groundtruth)) m:id->g:id)
        m:id->g:error    (comp (map-or-err (::groundtruth/task->error groundtruth)) m:id->g:id)
        alignment        (mapify (pair-with-r m:id->g:sentence (bpmn/all-task-ids model)))
        errors           (mapify (remove #(-> % second nil?)
                                         (pair-with-r m:id->g:error (bpmn/all-task-ids model))))
        ]
    {:gt/tasks         (bpmn/all-task-ids model)
     :gt/num-sentences (count m:sentences)
     :gt/alignment     alignment
     :gt/errors        errors}))



(comment ;REPL

  (defn exec [& args]
    (let [p (.exec (Runtime/getRuntime) (into-array String args))]
      p))

  (defn regen-case [case-name]
    (let [model (bpmn/build-model (bpmn/read-model (str benchmark-path "/" case-name ".bpmn")))
          text (slurp (str benchmark-path "/" case-name ".txt"))
          groundtruth-string (slurp (str groundtruths-path "/" case-name ".csv"))
          groundtruth (groundtruth/parse-groundtruth groundtruth-string)
          __ (def case-name case-name)
          __ (def groundtruth-string groundtruth-string)]
      (let [translated-groundtruth (translate-groundtruth groundtruth model text)]
        (spit (str "/home/josep/regenerated-groundtruths/" case-name ".json")
              (export-json translated-groundtruth)))))

  (defn sentence-ids [case-name]
    (let [groundtruth (groundtruth/parse-groundtruth (slurp (str groundtruths-path "/" case-name ".csv")))]
      (keys (::groundtruth/sentences groundtruth))))

  (def case-names (map strip-extension (.listFiles (io/file groundtruths-path))))


  (doseq [case-name case-names]
    (if (not (.exists (java.io.File. (str "/home/josep/regenerated-groundtruths/" case-name ".json"))))
      (try
        (println "Processing" case-name)
        (regen-case case-name)
        (catch Exception e (println "Error: " (.getMessage e))))))

  (def need-to-fix (filter #(let [ids (sentence-ids %)] (not= ids (range (count ids)))) case-names))
  (doseq [case-name need-to-fix]
    (let [path (str "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth/" case-name ".json")
          json (load-json (slurp path))
          fixed-json (specter/transform [:gt/alignment specter/ALL specter/LAST] #(if (number? %) (dec %) %) json)]
      (spit path (export-json fixed-json))))


  )
(comment
  "We will work from a couple test groundtruth files."
  (def --test-model (bpmn/build-model (bpmn/read-model "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/BicycleManufacturing.bpmn")))
  (def --test-task-labels (map #(.getName %) (bpmn/all-tasks --test-model)))
  (def --test-text (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/BicycleManufacturing.txt"))
  (def --test-sentences (textserver/split-sentences --test-text))
  (def --test-groundtruth-good (groundtruth/parse-groundtruth (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/groundtruths_for_testing/bicyclemanufacturing_good.csv")))
  (def --test-groundtruth-bad (groundtruth/parse-groundtruth (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/groundtruths_for_testing/bicyclemanufacturing_bad.csv")))

  (translate-groundtruth --test-groundtruth-good --test-model --test-text)
  (translate-groundtruth --test-groundtruth-bad --test-model --test-text))


(comment
  "Fixing problems"

  (def --test-model (bpmn/build-model (bpmn/read-model "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/Model10-4.bpmn")))

  (defn get-name [{:keys [model]} id]
    (.getName (.getFlowElement model id)))

  (get-name --test-model "Task_1")


  (spit "/home/josep/regenerated-groundtruths/Model2-1.json"
        (export-json

         ;;GROUNDTRUTH MAP GOES HERE
         #:gt{:tasks ["Task_0" "Task_1" "Task_10" "Task_11" "Task_12" "Task_13" "Task_14" "Task_15" "Task_16" "Task_17" "Task_18" "Task_19" "Task_2" "Task_20" "Task_21" "Task_22" "Task_23" "Task_24" "Task_25" "Task_3" "Task_4" "Task_5" "Task_6" "Task_7" "Task_8" "Task_9"]
              :num-sentences 38
              :alignment {"Task_12" 18
                          "Task_2" 5
                          "Task_13" 19
                          "Task_9" 11
                          "Task_14" 21
                          "Task_23" 28
                          "Task_17" 23
                          "Task_7" 34
                          "Task_25" 30
                          "Task_8" 1
                          "Task_1" :gt/no-match
                          "Task_3" 3
                          "Task_0" 2
                          "Task_4" 4
                          "Task_18" 14
                          "Task_24" 25
                          "Task_5" 8
                          "Task_20" 16
                          "Task_15" 16
                          "Task_11" 13
                          "Task_19" 15
                          "Task_10" 13
                          "Task_21" 16
                          "Task_16" 25
                          "Task_6" 34
                          "Task_22" 17}
              :errors {"Task_2" "w"
                       "Task_1" "m"
                       "Task_3" "w"
                       "Task_4" "w"}}
         ))

  (load-json (slurp "/home/josep/regenerated-groundtruths/Model2-1.json"))

)

