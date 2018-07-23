(ns edu.upc.modelvsdocument.textserver
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils])
  (:require [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.nlp4bpm-commons.freeling-api :as freeling]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.walk :as walk]
            [edu.upc.modelvsdocument.history :as h]
            [edu.upc.modelvsdocument.config :as config :refer [config]])
  (:gen-class))

(defn textserver->json [result]
  (walk/keywordize-keys
    (json/read-str result)))

(defn analyze-with-textserver [& {:keys [text lang output level]
                                  :or {output "json", lang "en", level "semgraph"}}]
  (freeling/analyze :text text :lang lang :output output :level level))

(defn analyze-cached [& {:keys [text lang output level]
                         :or {output "json", lang "en", level "semgraph"}}]
  "Same as analyze-with-textserver, cached version."
  (if (config :enable-cache)
    (freeling/analyze-cached :text text :lang lang :output output :level level)
    (freeling/analyze :text text :lang lang :output output :level level)))

(defn split-sentences [text]
  (let [result (textserver->json (analyze-cached :text text :level "morfo"))
        sentence-ranges (zip (map #(Integer/parseInt %)
                                  (select [:paragraphs ALL :sentences ALL
                                           :tokens FIRST :begin] result))
                             (map #(Integer/parseInt %)
                                  (select [:paragraphs ALL :sentences ALL
                                           :tokens LAST :end] result)))]
    (map #(apply subs text %) sentence-ranges)))

(comment

  (def text1 (textserver->json
              (analyze-with-textserver :text (slurp "/home/josep/ModelsBpmn/Zoo.txt"))))

  (def text2 (textserver->json
              (analyze-with-textserver :text (slurp "/home/josep/ModelsBpmn/Dispatch-of-goods.txt"))))

  (def text3 (textserver->json
              (analyze-with-textserver :text (slurp "/home/josep/ModelsBpmn/Model2-1.txt"))))

  text1

  text2

  text3

  )
