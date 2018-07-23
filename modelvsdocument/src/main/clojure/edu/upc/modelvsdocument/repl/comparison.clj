(ns edu.upc.modelvsdocument.repl.comparison
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.core :refer :all]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.modelvsdocument.wordnet :as wordnet]
            [edu.upc.modelvsdocument.verification.groundtruth :as groundtruth]))

; CACHE INITIALIZATION
(comment (cache/initialize (str (System/getProperty "user.home") "/.textserver-cache.clj")))

; READ WORDNET DICTIONARIES
(comment (if (and (.exists (clojure.java.io/as-file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/wn30.src"))
                  (.exists (clojure.java.io/as-file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/senses30.src")))
           (wordnet/read-wordnet-dictionaries "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/")
           (throw (Exception. "Wordnet dictionaries not found"))))

(defn execute-algorithm 
  ([model text] (execute-algorithm "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/" model text))
  ([b-path model text]
   (main (str b-path model) (slurp (str b-path text)))))

(defn print-log 
  ([model text] (print-log "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/" model text))
  ([b-path model text] (println (.getLog (execute-algorithm b-path model text)))))

(defn get-matching
  ([model text] (get-matching "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/" model text))
  ([b-path model text] (:match-struct (.getClojureExtraData (execute-algorithm b-path model text)))))

(defn name->groundtruth [case-name]
    (let [groundtruth-files (file-seq (io/file "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/"))
          files-with-names (remove #(nil? (first %)) (pair-with strip-extension groundtruth-files))]
      (second (apply min-key #(levenshtein (first %) case-name) files-with-names))))

(defn benchmark->groundtruth []
  (mapify 
    (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"] 
      (for [bpmn-file (filter #(= (extension %) "bpmn") (file-seq (io/file benchmark-path)))]
        [bpmn-file (name->groundtruth (strip-extension bpmn-file))]))))

(defn make-comparison-theirs []
  (let [their-results (binding [*read-eval* false] 
                        (read-string (slurp (io/file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/theirs.clj"))))
        results-table (io/file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/theirs.csv")]
    (spit results-table "")
    (doseq [{:keys [case-name] :as result} their-results
            :let [groundtruth-file (name->groundtruth case-name)
                  groundtruth (groundtruth/parse-groundtruth (slurp groundtruth-file))]]
      (try 
        (let [comparison (groundtruth/compare-to-groundtruth result groundtruth)]
          (spit results-table
                (str (:case-name result) ", " 
                     (::groundtruth/ok comparison) "/" (::groundtruth/total comparison) ", " 
                     (- (:total-time result) (:preprocessing-time result)) "\n")
                :append true))
        (println case-name "\n-------------\n" (groundtruth/explain-comparison (groundtruth/compare-to-groundtruth result groundtruth)))
           (catch Throwable e (println "Exception in " case-name))))))
    
(make-comparison-theirs)
