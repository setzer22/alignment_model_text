(ns edu.upc.modelvsdocument.brat.parser
  (:require [clj-pipeline.core :as pipeline]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.brat.utils :refer :all]
            [edu.upc.modelvsdocument.brat.specs :as brt]
            [clojure.set :as set]))

(def brat-types [::brt/tag ::brt/relation ::brt/attribute])

(def brat-type-regexps
  {::brt/tag #"T(\d+)"
   ::brt/relation #"R(\d+)"
   ::brt/attribute #"A(\d+)"})

(defn dispatch-brat-type [[id & _]]
  (first (keys-such-that
           #(re-matches % id)
           brat-type-regexps)))

(defmulti brat-parse-line
  "Multimethod to parse a brat line. The dispatch is done based on the annotation type.
   Annotation types are defined alongside their regular expressions in 'brat-type-regexes'"
  dispatch-brat-type)

(defprotocol Typed
  (type-key [this]))
(defrecord Tag [id type start end text]
  Typed (type-key [this] ::brt/tag))
(defrecord Attribute [id type referenced-tag value]
  Typed (type-key [this] ::brt/attribute))
(defrecord Relation [id type src-id dst-id]
  Typed (type-key [this] ::brt/relation))

(defmethod brat-parse-line ::brt/tag
  [[id type start end text]]
  (->Tag id type (Integer/parseInt start) (Integer/parseInt end) text))

(defmethod brat-parse-line ::brt/relation
  [[id type src-id-unparsed dst-id-unparsed]]
  (let [dst-id (second (string/split dst-id-unparsed #":"))
        src-id (second (string/split src-id-unparsed #":"))]
    (->Relation id type src-id dst-id)))

(defmethod brat-parse-line ::brt/attribute
  [[id type ref value]]
  (->Attribute id type ref value))

(defmethod brat-parse-line :default
  [line]
  (throw (Exception. (str "[BRAT PARSER ERROR]: Extraneous input line: " line))))

(defn tokenize-line [line]
  (string/split line #"\s+"))

(pipeline/defpipe-step parse-lines [data]
  {:anns (map #(brat-parse-line (tokenize-line %)) data)})
