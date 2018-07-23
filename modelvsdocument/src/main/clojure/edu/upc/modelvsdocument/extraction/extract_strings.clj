(ns edu.upc.modelvsdocument.extraction.extract-strings
  (:require [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.alignable :as a]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.bpmn-alignable :as bpmn-al]
            [edu.upc.modelvsdocument.extraction.model-extraction :as extraction]))

(defn extract-features
  "Analyzes strings as model labels and returns the list of extracted features"
  [activiti-model, lang]
  (let [model (bpmn-al/analyze activiti-model nil)]
    (transform [ALL FIRST]
               a/id
               (extraction/extract-features model))))

#_(edu.upc.modelvsdocument.config/with-config {:model-read-strict false}
    (extract-features (bpmn/read-model "/home/josep/Downloads/Telegram Desktop/1354695194773.bpmn")
                      "en"))


