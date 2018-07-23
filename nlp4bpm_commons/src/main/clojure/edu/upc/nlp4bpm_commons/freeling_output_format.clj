(ns edu.upc.nlp4bpm-commons.freeling-output-format
  (:import [edu.upc.Jfreeling OutputJson OutputXml])
  (:require
   [edu.upc.nlp4bpm-commons.utils :refer :all]
   [edu.upc.nlp4bpm-commons.config :as config]))

(defonce outputs (atom {}))

(defn output-fn [lang]
  ;; TODO: XML output config
  (if (not (get @outputs lang))
    (swap! outputs assoc lang (OutputJson. (if (config/-getConfig :json-cfg-path-override)
                                             (str-or-throw (config/-getConfig :json-cfg-path-override))
                                             (str-or-throw (config/-getConfig :freeling-dir-path) "/" lang "/json.cfg")))))
  {"json" #(.printResults (get @outputs lang) %)
   "xml" #(.printResults (OutputXml.) %)})
