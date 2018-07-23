(ns edu.upc.nlp4bpm-commons.freeling-api
  (:require [edu.upc.nlp4bpm-commons.textserver :as textserver]
            [edu.upc.nlp4bpm-commons.config :as config]
            [edu.upc.nlp4bpm-commons.Freeling :as freeling]
            [edu.upc.nlp4bpm-commons.label-parser :as label-parser]
            [edu.upc.nlp4bpm-commons.cache :as cache]))

(defn set-mode [the-mode]
  (assert (#{"online" "local"} the-mode) "Mode must be one of: online, local")
  (config/-setConfig "mode" the-mode))

(defn analyze [& {:keys [text lang output level mode-override]
                  :or   {output "json", lang "en", level "semgraph"}}]
  (let [mode (or mode-override (config/-getConfig "mode"))]
    (cond
      (= mode "online") (textserver/analyze :text text :output output :lang lang :level level)
      (= mode "local")  (freeling/analyze :text text :output output :lang lang :level level))))

(defn analyze-labels-java [& {:keys [labels lang label-type]}]
  (label-parser/parse-labels-java
   labels lang
   :label-type label-type))

(defn analyze-labels [& {:keys [labels lang label-type start-id]
                         :or {lang "en" labels []
                              label-type "task" start-id 1}}]
  (label-parser/parse-labels
   labels lang
   :label-type label-type :start-id start-id))

(defn analyze-labels-cached [& {:keys [labels lang label-type start-id]
                                :or {lang "en" labels []
                                     label-type "task" start-id 1}}]
  (if-let [maybe-response (cache/retrieveItem [labels lang label-type start-id])]
    maybe-response
    (let [response (analyze-labels :labels labels :lang lang :label-type
                                   label-type :start-id start-id)]
      (cache/addItemIfNotExists [labels lang label-type start-id] response)
      response)))

(defn analyze-cached [& {:keys [text lang output level]
                         :or {output "json", lang "en", level "semgraph"}}]
  "Same as analyze, cached version."
  (if-let [maybe-response (cache/retrieveItem [text lang output level])]
    maybe-response
    (let [response (analyze :text text :lang lang :output output :level level)]
      (cache/addItemIfNotExists [text lang output level] response)
      response)))

(defn analyze-cached-if-available [& {:keys [text lang output level]
                                      :or {output "json", lang "en", level "semgraph"}}]
  (cache/if-cache
   (analyze-cached :text text :lang lang :output output :level level)
   (analyze :text text :lang lang :output output :level level)))

(defn set-credentials-path! [path]
  (swap! edu.upc.nlp4bpm-commons.textserver/credentials-path (constantly path)))

