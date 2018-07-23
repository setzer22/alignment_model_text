(ns edu.upc.modelvsdocument.script.model-tags
  (:require [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [clojure.data.csv :as csv]))

(comment
  BEGIN

  (let [data (->> (get-files-with-extension "bpmn" "/home/josep/ModelsBpmn")
                  (map #(.getAbsolutePath %))
                  (map bpmn/read-model)
                  (mapcat (fn [model]
                            (filter
                             #(instance? org.activiti.bpmn.model.Task %)
                             (mapcat #(.getFlowElements %) (.getProcesses model)))))
                  (map #(.getName %))
                  (remove empty?)
                  (remove #(.contains % ";"))
                  (remove #(.contains % "\n"))
                  (map #(str "0;" % ";" % ";" %))
                  (clojure.string/join "\n"))]
    (spit "/home/josep/labels.csv" data))


  (let [models (map bpmn/construct-model (get-files-with-extension "bpmn" "/home/josep/ModelsBpmn"))]
    models)


  END)

