(ns edu.upc.modelvsdocument.script.model-generator
  (:require [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.utils :as utils :refer :all]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [clojure.java.shell :refer [sh]]))

(defn mk-random-sentence-pool []
  (clojure.string/split-lines
   (:out (sh "python2" "/home/josep/Repositories/markov-sentence-generator/sentence-generator-N.py"
             "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/markov-text/out/Nonsense.txt"))))

(defonce random-sentence-pool (atom []))

(defn generate-random-sentence []
  (let [sentence (first @random-sentence-pool)]
    (if sentence
      (let [sentence-up-to-point (first (clojure.string/split sentence #"\."))]
        (swap! random-sentence-pool next)
        (if (and (> (count sentence-up-to-point) 5))
          sentence-up-to-point
          (recur)))
      (do (swap! random-sentence-pool (fn [x] (mk-random-sentence-pool)))
          (recur)))))

(defn save-model-as [model path]
  (let [activiti-model (:model model)
        model-buffer (.convertToXML (org.activiti.bpmn.converter.BpmnXMLConverter.) activiti-model)]
    (spit path (String. model-buffer "UTF-8"))))

(defn generate-all-task-names! [model]
  (let [task-ids (bpmn/all-task-ids model)]
    (doseq [id task-ids]
      (.setName (.getFlowElement (:model model) id)
                (generate-random-sentence)))))

;(def model (-> "/home/josep/BigModel2.bpmn" bpmn/read-model bpmn/build-model))

;(generate-all-task-names! model)

;(save-model-as model "/home/josep/wat.bpmn")

;(def bs-model (-> "/home/josep/wat.bpmn" bpmn/read-model bpmn/build-model))

;(def text (clojure.string/join ".\n\n" (repeatedly 52 generate-random-sentence)))

;(println text)

;(time (edu.upc.modelvsdocument.core/main "/home/josep/wat.bpmn" text))

;edu.upc.modelvsdocument.core/model-features
;(clojure.inspector/inspect-table edu.upc.modelvsdocument.core/sim-matrix)


;(println text)

(defn mk-random-config [max-depth]
  (doto (plg.generator.process.RandomizationConfiguration. 5 5 0.1 0.2 0.1 0.7 0.3 0.3 3 0.0)
    (.setDepth max-depth)))

(defn generate-model [config]
  (let [model-name (str (Math/abs (rand-int 10000)) ".bpmn")
        p (doto (plg.model.Process. "Test")
            (plg.generator.process.ProcessGenerator/randomizeProcess
             config))]
    (.exportModel (plg.io.exporter.BPMNExporter.) p (str "/tmp/" model-name))
    (-> (str "/tmp/" model-name) bpmn/read-model bpmn/build-model)))

(defn generate-benchmark-models [min-depth max-depth num-models]
  (apply concat
        (for [depth (range min-depth max-depth)]
          (repeatedly num-models #(generate-model (mk-random-config depth))))))

(comment
  (time
   (do
     (def the-models (generate-benchmark-models 3 6 100))

     (use '(incanter core charts))

     (view (histogram (sort (map #(count (bpmn/all-tasks %)) the-models))
                      :nbins 100
                      :x-label "Number of tasks"
                      :title "Frequency of model sizes"))))

  (doseq [model the-models]
    (generate-all-task-names! model))

  (def out-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/time_benchmark_2")

  ;; Generate the models
  (doseq [model (sort-by #(count (bpmn/all-tasks %))
                         the-models)]
    (save-model-as model (str out-path "/" "model_" (format "%03d" (count (bpmn/all-tasks model))) "-"
                              (format "%03d" (mod (hash model) 1000))".bpmn")))

  ;; Generate the texts
  (doseq [model (sort-by #(count (bpmn/all-tasks %))
                         disticnt-models)
          :let [size (count (bpmn/all-tasks model))
                text-size (max 1 (+ size (- (rand-int 6) 3)))]]
    (spit (str out-path "/" "model_" (format "%03d" (count (bpmn/all-tasks model))) "-"
               (format "%03d" (mod (hash model) 1000))".txt")
          (clojure.string/join ".\n\n"
                               (repeatedly text-size generate-random-sentence))))

  (doseq [file (file-seq (clojure.java.io/file out-path))
          :when (= "bpmn" (extension file))]
    (-> (.getAbsolutePath file) bpmn/read-model bpmn/build-model)
    (println file "was read OK"))

  )
