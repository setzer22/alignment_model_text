(ns edu.upc.modelvsdocument.repl.simple-xml-formatter
  (:require [edu.upc.modelvsdocument.bpmn :as bpmn :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [clojure.data.xml :as xml]))
(comment
  (defn read-full-model [model-filename]
    (let [path (str "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/" model-filename)] 
      (when (.exists (java.io.File. path)) 
        (-> path bpmn/read-model bpmn/build-model))))

  (def bicycle-model (read-full-model "Bicycle_Manufacturer.bpmn"))
  (xml/emit-str (model->xml bicycle-model))

  (defn make-id-mapping [model-struct]
    (mapify (zip (bpmn/all-task-ids model-struct) (range))))

  (defn graph->xml-pairs [id-mapping graph]
    (reduce 
      (fn [edges-so-far [pred successors]]
        (apply conj edges-so-far 
               (map #(xml/element "pair" {} 
                                  (xml/element "pred" {} (id-mapping pred))
                                  (xml/element "succ" {} (id-mapping %))) 
                    successors)))
      []
      graph))

  (defn model->xml [model-struct text]
    (let [task-id? (set (bpmn/all-task-ids model-struct))
          not-task-id? (fn [id] (not (task-id? id)))
          id-mapping (make-id-mapping model-struct)
          task-nodes 
          (as-> model-struct $$
            (all-tasks $$)
            (map (fn [task] (xml/element "activity" {:id (.getId task)} 
                                         (xml/element "label" {} (.getName task))
                                         (xml/element "verbBase" {} "")
                                         (xml/element "object" {} ""))) $$)
            (apply xml/element "activities" {} $$))
          graph-edges
          (apply xml/element "follows" {} 
                       (mapcat 
                         #(graph->xml-pairs id-mapping %)
                         (map #(reduce-graph (:graph %) not-task-id?) (:processes model-struct))))]
      (xml/element "process" {}
                   task-nodes
                   graph-edges
                   (xml/element "text" {} text))
      ))

  (spit "/home/josep/big-model.xml" (xml/indent-str (model->xml edu.upc.modelvsdocument.core/model-struct "Hello world!")))
  (print *e)
  (println (xml/indent-str (clojure.pprint/pprint (model->xml bicycle-model)))))
