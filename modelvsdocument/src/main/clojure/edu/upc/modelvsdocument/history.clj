(ns edu.upc.modelvsdocument.history
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [schema.core :as s :refer [fn defn defrecord]]))

(def history-path "/var/tmp/")

(defn ls-history-indexed [] 
  (let [history-folder (clojure.java.io/file history-path)] (map 
    (fn [file] (let [groups (re-find #"(\d+).*\.clj" (.getName file))]
                 [file (Integer. (second groups))]))
    (filter #(not (.startsWith (.getName %) ".")) (.listFiles history-folder)))))

(defn history-last-index []
  (let [indices (map second (ls-history-indexed))] 
    (if-not (empty? indices) 
      (apply max indices)
      0)))

(defn get-hist-last 
  ([] (get-hist-last #".*"))
  ([filter-regex] (binding [*read-eval* false] ; to avoid code injection
    (read-string 
      (slurp (first (first (sort-by 
                             #(- (second %))
                             (filter #(re-find filter-regex (.getName (first %))) 
                                     (ls-history-indexed))))))))))

(defn when-hist-last? 
  ([] (when-hist-last? #".*"))
  ([filter-regex] (:time (get-hist-last filter-regex))))

(defn hist-last-data 
  ([] (hist-last-data #".*"))
  ([filter-regex] (:data (get-hist-last filter-regex))))

(defn append-time [data]
  {:time (java.util.Date.)
   :data data})

(defn history-push 
  ([data] (history-push data "data"))
  ([data name] (spit (str history-path (inc (history-last-index)) "-" name ".clj") (append-time data))))

