(ns edu.upc.modelvsdocument.instrumentation
  (:require [clojure.string :as str]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]))

(defn all-specd-functions [] 
  (as-> (all-ns) $$
    (map str $$)
    (filter #(str/includes? % "edu.upc.modelvsdocument.") $$)
    (map symbol $$)
    (mapcat stest/enumerate-namespace $$)))

(defn instrument-all! []
  (stest/instrument (all-specd-functions)))

(defn unstrument-all! []
  (stest/unstrument (all-specd-functions)))
