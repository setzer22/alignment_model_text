(ns edu.upc.modelvsdocument.Main
  (:require [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.config :as config :refer [config]]
            [edu.upc.modelvsdocument.wordnet :as wordnet]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.nlp4bpm-commons.freeling-api :as freeling]
            [edu.upc.modelvsdocument.extraction.feature :as f])
  (:import [edu.upc.nlp4bpm_commons Cache])
  (:gen-class
   :name edu.upc.modelvsdocument.Main
   :methods [#^{:static true} [executeModule [String String] edu.upc.modelvsdocument.Result]
             #^{:static true} [executeModule [String String java.util.Map] edu.upc.modelvsdocument.Result]
             #^{:static true} [loadConfig [String String] void]]
   :main true))

(defn file-in-path-exists? [path]
  (and (string? path)
       (.exists (clojure.java.io/file path))))

(defn -main [config-path credentials-path model-path text-path & _]
  (throw (Exception. "Main now takes a string, not a path, fix it!"))
  (try
    (freeling/set-credentials-path! credentials-path)
    (config/override-config-from-file config-path)
    (when (:enable-cache config)
      (let [cache-path (home-relative (:cache-path config))
            cache-file (clojure.java.io/file cache-path)]
        (if (.exists (.getParentFile cache-file))
          (Cache/initialize cache-path)
          (println "WARNING: Cache path" cache-path " doesn't exist. Proceeding without cache."))))
    (cond (file-in-path-exists? (home-relative (:wordnet-path config)))
          ,,(wordnet/read-wordnet-dictionaries (home-relative (:wordnet-path config)))
          (file-in-path-exists? (System/getenv "NLP4BPM_WORDNET_PATH"))
          ,,(wordnet/read-wordnet-dictionaries (System/getenv "NLP4BPM_WORDNET_PATH"))
          :else
          ,,(throw (Exception. "Wordnet path not specified in the config file or NLP4BPM_WORDNET_PATH variable. Please set this path pointing to the wordnet folder in the installation directory.")))
    (println (.getLog (core/main model-path (slurp text-path))))
    (if (config :enable-cache) (Cache/saveCache))
    (catch Throwable e
      (println "There was an error in the execution of the program. Please contact the authors. INFO:" (.getMessage e)))))

(defn -loadConfig
  "Must be run once when the server starts."
  [config-path credentials-path]
  (freeling/set-credentials-path! credentials-path)
  (config/override-config-from-file config-path)
  (cond (file-in-path-exists? (home-relative (:wordnet-path config)))
        ,,(wordnet/read-wordnet-dictionaries (home-relative (:wordnet-path config)))
        (file-in-path-exists? (System/getenv "NLP4BPM_WORDNET_PATH"))
        ,,(wordnet/read-wordnet-dictionaries (System/getenv "NLP4BPM_WORDNET_PATH"))
        :else
        ,,(throw (java.io.FileNotFoundException.
                  (str "Wordnet path not specified in the config file or NLP4BPM_WORDNET_PATH variable."
                       "Please set this path pointing to the wordnet folder in the installation directory.")))))

(defn keywordize-keys-1 [map]
  (reduce
   (fn [map [k v]] (assoc map (keyword k) v))
   {} map))

(defn -executeModule
  "Executes the main module with the given model and text and returns a Result object."
  ([model-path text] (-executeModule model-path text {}))
  ([model-path text per-exec-config]
   (binding [config (merge config (keywordize-keys-1 per-exec-config))]
     (core/main model-path text))))

