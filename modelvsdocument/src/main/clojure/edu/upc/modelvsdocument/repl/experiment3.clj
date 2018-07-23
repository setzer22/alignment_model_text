(ns edu.upc.modelvsdocument.repl.experiment3
  (:require [edu.upc.modelvsdocument.verification.groundtruth :as groundtruth]
            [clojure.java.io :as io]
            [edu.upc.modelvsdocument.utils :as utils :refer :all]
            [edu.upc.modelvsdocument.core :as core]
            [clojure.set :as set]))

(defn compare-results [r1 r2]
  (groundtruth/compare-to-groundtruth r1 r2))


(comment
  (comment
    (def our-results
      (future
        (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/eindhoven/"
              model-files (sort (get-files-with-extension "bpmn" benchmark-path))
              text-files (sort (get-files-with-extension "txt" benchmark-path))]
          (into [] (for [[model text] (zip model-files text-files)
                         :let [__ (clojure.java.shell/sh "notify-send" (strip-extension model))]]
                     [(strip-extension model)
                      (try (:match-struct (.getClojureExtraData (core/main-with-files model text)))
                           (catch Throwable e {:error (.getMessage e)}))])))))

    @our-computed-results

    (future
      (let [result @our-results]
        (clojure.java.shell/sh "notify-send" "JOB IS DONE")))
    (spit "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/eindhoven-ours.clj"
          (mapv (fn [[case-name result]]
                 (assoc result :case-name case-name))
               @our-computed-results))

    )

  (def their-results
    (read-string
     (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/eindhoven-theirs.clj")))

  (def our-results
    (read-string
     (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/eindhoven-ours.clj")))

  (count (set/difference
          (set (map :case-name our-results))
          (set (map :case-name their-results))))

  (def case-name (first (map :case-name their-results)))

  (def our-model (first (filter #(= (:case-name %) case-name)
                                our-results)))
  (def their-model (first (filter #(= (:case-name %) case-name)
                                their-results)))

  (def result
    (into []
         (for [case-name (map :case-name their-results)
               :let [our-model (first (filter #(= (:case-name %) case-name)
                                              our-results))
                     their-model (first (filter #(= (:case-name %) case-name)
                                                their-results))]]
           (compare-results our-model their-model))))

  (clojure.pprint/pprint (nth their-results 3))

  (apply min (map ::groundtruth/not-ok result))

  (println (groundtruth/explain-comparison
    (nth result 3)))

  (compare-results our-model their-model)



  )
