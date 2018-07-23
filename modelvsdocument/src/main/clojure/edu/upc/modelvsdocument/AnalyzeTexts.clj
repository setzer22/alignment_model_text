(ns edu.upc.modelvsdocument.AnalyzeTexts
  (:require [clojure.java.io :as io]
            [edu.upc.nlp4bpm-commons.freeling-api :as freeling]
            [edu.upc.modelvsdocument.config :as config]
            [edu.upc.modelvsdocument.utils :refer :all])
  (:gen-class
   :name edu.upc.modelvsdocument.AnalyzeTexts
   :main true))

(comment
 (defn -main [student-folder freeling-dir-path json-cfg-path & args]
  (freeling/set-mode "local")
  (binding [edu.upc.nlp4bpm-commons.Freeling/freeling-dir-path freeling-dir-path
            edu.upc.nlp4bpm-commons.Freeling/json-cfg-path json-cfg-path]
    (let [student-files (filter #(and (= (extension %) "txt")
                                      (not (.exists (io/file (str student-folder (strip-extension %) ".json")))))
                                (seq (.listFiles (io/file student-folder))))]
      (doseq [f student-files
              :let [__ (println "Analyzing " (.getName f))
                    text (slurp f)
                    json-filename (str (strip-extension f) ".json")
                    json (freeling/analyze :text text :lang "en" :output "json"
                                           :level "semgraph" :level "dep")
                    __ (println "Done!")]]
        (spit (str student-folder "/" json-filename) json))))))
