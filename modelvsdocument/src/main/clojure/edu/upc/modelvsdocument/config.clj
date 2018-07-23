(ns edu.upc.modelvsdocument.config
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [clojure.java.io :as io]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clojure.walk :as walk]))

(def ^:dynamic config
  {;TOGGLES
   :enable-tf-idf true
   :enable-anchors true
   :create-log true
   :verbose-log false
   :use-gurobi false

   :missing-predictor-type :p-rel-S
   :max-constrained-threshold 0.95
   :enable-cutoff false
   :enable-wrong-order-cutoff false
   :threshold-missing 0.2
   :threshold-misplaced "NA"

   :disable-restrictions false
   :return-trivial-alignment false

   :use-old-label-parser false

   :use-vu-similarity false

   :remove-back-edges false

   ;CACHE
   :enable-cache false
   :cache-path nil

   ;WORDNET PATH
   :wordnet-path nil

   ;THESE CAN BE DISABLED TO HANDLE WRONG INPUT
   :model-read-strict true
   :tf-idf-strict true

   ;;GUROBI
   :gurobi-tmp-path "/tmp/"

   ;SIMILARITY AND FEATURES
   :agent-match-coef 0.2
   :hiperonimy-chain-length 3
   :hiperonimy-multiplier 0.5
   :synset-candidate-width 2
   :similarity-function "weighted-fake-jaccard"
   :feature-weight-overrides {}
   :feature-explain-overrides {}})

(spec/def ::enable-tf-idf boolean?)
(spec/def ::create-log boolean?)
(spec/def ::verbose-log boolean?)
(spec/def ::enable-cache boolean?)

(spec/def ::agent-match-coef number?)
(spec/def ::hiperonimy-chain-length nat?)
(spec/def ::hiperonimy-multiplier number?)
(spec/def ::synset-candidate-width nat?)
(spec/def ::similarity-function string?)
(spec/def ::feature-weight-overrides (spec/map-of keyword? number?))
(spec/def ::feature-explain-overrides (spec/map-of keyword string?))
(spec/def ::config (spec/keys :req-un [::agent-match-coef ::hiperonimy-chain-length ::synset-candidate-width
                                       ::hiperonimy-multiplier ::feature-weight-overrides ::feature-explain-overrides
                                       ::similarity-function ::create-log ::verbose-log
                                       ::enable-tf-idf ::enable-cache]))

(defmacro with-weights [weight-set & body]
  `(binding [config (assoc config :feature-weight-overrides ~weight-set)]
     ~@body))

(defmacro with-config [config-override & body]
  `(binding [config (merge config ~config-override)]
     ~@body))

(defn validate-config [config]
  (when-not (spec/valid? ::config config)
    (throw (Exception. (str "Config file has invalid syntax: " (spec/explain-str ::config config))))))


(spec-fn set-value-in-config string? any?)
(defn set-value-in-config [str-key value]
  (if-not (contains? config (keyword str-key)) (throw (Exception. (str "No valid config key: " str-key))))
  (let [new-value (if (map? value) (walk/keywordize-keys value) value)
        new-config (assoc config (keyword str-key) value)]
    (validate-config new-config)
    (def config new-config)))

(spec-fn get-value-in-config string?)
(defn get-value-in-config [str-key]
  (assert (contains? config (keyword str-key)))
  ((keyword str-key) config))

(spec-fn load-config-from-file string?)
(defn load-config-from-file [path]
  (binding [*read-eval* false] ; Avoid code execution when reading config file
    (when-not (.exists (io/file path)) (java.io.FileNotFoundException. (str "Config file: " path ", doesn't exist.")))
    (let [config-str (slurp (io/file path))
          new-config (walk/keywordize-keys (read-string config-str))]
      (validate-config new-config)
      (def config new-config))))

(defn override-config-from-file [path]
  (binding [*read-eval* false]
    (when-not (.exists (io/file path)) (java.io.FileNotFoundException. (str "Config file: " path ", doesn't exist.")))
    (let [new-config (merge-with (fn [_ x] x)
                                 config
                                 (read-string (slurp (io/file path))))]
      (validate-config new-config)
      (alter-var-root #'config (constantly new-config)))))

