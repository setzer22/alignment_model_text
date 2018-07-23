(ns edu.upc.modelvsdocument.script.run-big-benchmark
  (:require [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.wordnet :as wordnet]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.modelvsdocument.solver :as solver]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.config :as config :refer [config with-weights]])
  (:gen-class
   :name edu.upc.modelvsdocument.script.RunBigBenchmark
   :main true))

(comment (def good-weights
           {:has-lemma 0.11651, :has-parent-synset 0.19204, :in-agent 0.9843, :agent-head 0.2408, :has-form 0.48355, :patient-head 0.59129, :in-patient 0.04503, :lemma-conditional-pred 0.55536, :has-synset 0.42809, :has-action 0.69586, :lemma-conditional-follow 0.82183}))

(comment (defn -main [config-path credentials-path & _]
           (let [{:keys [benchmark-path cache-path wordnet-path
                         gurobi-models-folder output-path] :as config}
                 (read-string (slurp config-path))]

             (config/set-creds-path! credentials-path)
             (wordnet/read-wordnet-dictionaries wordnet-path)

             (with-weights good-weights
               (binding [solver/gurobi-models-folder gurobi-models-folder
                         config/config (assoc config/config
                                              :enable-cache false
                                              :create-log false)]
                 (let [models (sort (get-files-with-extension "bpmn" benchmark-path))
                       texts (sort (get-files-with-extension "txt" benchmark-path))]
                   (spit output-path "[\n")
                   (doseq [[model text] (zip models texts)
                           :let [case-name (strip-extension model)
                                 __ (println case-name)
                                 output (try (let [time0 (System/currentTimeMillis)
                                                   __ (core/main-with-files model text)
                                                   time1 (System/currentTimeMillis)]
                                               {:case-name case-name
                                                :time (- time1 time0)
                                                :error nil})
                                             (catch Throwable e {:case-name case-name
                                                                 :time nil
                                                                 :error (str (.getMessage e)
                                                                             "\n\nCAUSE:"
                                                                             (.getCause e))}))]]
                     (spit output-path (str output "\n") :append true)
                     (println output))
                   (spit output-path "\n]")))))))


(comment
  (-main "/home/josep/time-benchmark-config.clj"
         "/home/josep/.textserver-credentials")


  )

