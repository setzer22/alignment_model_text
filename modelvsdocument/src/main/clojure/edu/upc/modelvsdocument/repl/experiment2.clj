(ns edu.upc.modelvsdocument.repl.experiment2
  (:require [incanter.core :refer :all]
            [incanter.charts :refer :all]))

(def our-models (read-string (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/time-benchmark-data.clj")))
(def their-models (read-string (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/time-benchmark-theirs.clj")))
(comment "CSV FOR LLUIS"
         (spit "/home/josep/data-theirs.csv"
               (with-out-str (let [their-sizes (map #(Integer/parseInt (second (re-matches #"model_(\d+)(-.*)?" (str (:case-name %))))) their-models)
                              their-times (map #(/ (:total-time %) 1000) their-models)]
                          (doseq [[s t] (map vector their-sizes their-times)]
                            (println (str s ", " (double t)))))))
         )

(defn mk-time-graph [our-models their-models]
  (let [our-sizes (map #(Integer/parseInt (second (re-matches #"model_(\d+)(-.*)?" (str (:case-name %))))) our-models)
        our-times (map #(/ (:time %) 1000) our-models)
        their-sizes (map #(Integer/parseInt (second (re-matches #"model_(\d+)(-.*)?" (str (:case-name %))))) their-models)
        their-times (map #(/ (:total-time %) 1000) their-models)]
    (doto
        (scatter-plot our-sizes our-times
                   :x-label "Input Size"
                   :y-label "Time (s)")
      (add-points their-sizes their-times))))

(view (mk-time-graph our-models their-models))


  (def their-time-benchmark-data
    (read-string (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/inconsistenciesmodeltext-output.clj")))


  (let [sizes (map #(Integer/parseInt (second (re-matches #"model_(\d+)" (:case-name %))))
                   their-time-benchmark-data)
        times (map #(/ (:total-time %) 1000) their-time-benchmark-data)]
    (add-points plot sizes times))

  (set-axis plot :y (log-axis :label "Log Time (s)" :base 10))

  (view plot)

