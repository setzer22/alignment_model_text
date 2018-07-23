"In this file, an utility function is provided that will ensure all usual services
 required for testing are running."

(ns edu.upc.modelvsdocument.environment
  (:require [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.nlp4bpm-commons.freeling-api :as freeling-api]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.modelvsdocument.config :as config]))

(defn enable-test-environment! []
  (freeling-api/set-mode "local")
  (alter-var-root #'config/config assoc
         :enable-cache true
         :use-gurobi true)
  (cache/initialize (:cache-path config/config))
  (freeling-api/analyze-cached :text "dummy text"))
