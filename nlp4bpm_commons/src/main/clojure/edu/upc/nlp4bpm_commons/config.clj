(ns edu.upc.nlp4bpm-commons.config
  (:require [clojure.spec :as spec]))

(def ^:dynamic config
  {:mode "online"
   :freeling-dir-path "/usr/local/share/freeling/"
   :custom-grammars-folder (str (System/getProperty "user.home") "/BPMN/grammars/")
   :json-cfg-path-override nil})

(spec/def ::mode (spec/or :local #(= "local" %) :online #(= "online" %)))
(spec/def ::freeling-dir-path string?)
(spec/def ::json-cfg-path-override (spec/nilable string?))
(spec/def ::config (spec/keys :req-un [::mode ::freeling-dir-path ::json-cfg-path-override]))

(defn -setConfig [property val]
  (let [property (if (keyword? property) property (keyword property))
        updated-config (assoc config property val)]
    (if (spec/valid? ::config updated-config)
      (alter-var-root #'config (constantly updated-config))
      (throw (Exception. (spec/explain-str ::config updated-config))))))

(defn -getConfig [property]
  (let [property (if (keyword? property) property (keyword property))]
    (get config property)))
