(ns edu.upc.nlp4bpm-commons.textserver
  (:use [com.rpl.specter])
  (:require [edu.upc.nlp4bpm-commons.cache :as cache]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.walk :as walk]))

(def url
  "The textserver url"
  "http://frodo.lsi.upc.edu:8080/TextWS/textservlet/ws/processQuery/freeling")

(defn text-body [k v]
  "Returns a multipart text-body parameter"
  {:name k, :content v})

(def ^:dynamic credentials-path
  (atom (str (System/getProperty "user.home") "/.textserver-credentials")))

(defn analyze [& {:keys [text lang output level]
                  :or {output "json", lang "en", level "semgraph"}}]
  "Returns an json or xml formatted string with the TextServer answer"
  (let [{:keys [username password]} (read-string (slurp @credentials-path))]
    (assert (not-any? nil? [username password]))
    (let [body  (:body
                 (client/post url
                              {:multipart
                               [(text-body "username" username)
                                (text-body "password" password)
                                (text-body "text_input" text)
                                (text-body "language" lang)
                                (text-body "output" output)
                                (text-body "OutputLevel" level)
                                (text-body "interactive" "1")
                                (text-body "MultiwordDetection"  "1")
                                (text-body "NumbersDetection"  "1")
                                (text-body "DatesDetection"  "1")
                                (text-body "QuantitiesDetection"  "1")
                                (text-body "NERecognition"  "1")
                                (text-body "NEClassification"  "1")
                                (text-body "Phonetics"  "0")
                                (text-body "SenseAnnotation"  "ukb")
                                ]}))]
      (if (.startsWith body "A problem was found")
        (throw (Exception. (str "Textserver error: " body)))
        body))))
