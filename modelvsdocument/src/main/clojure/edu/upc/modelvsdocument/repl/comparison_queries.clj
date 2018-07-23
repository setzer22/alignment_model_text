(ns edu.upc.modelvsdocument.repl.comparison-queries
  (:require [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.alignable :as a]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer :all]
            [clojure.data.csv :as csv]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.config :refer [config with-weights with-config]]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.core :refer :all]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.modelvsdocument.wordnet :as wordnet]
            [edu.upc.modelvsdocument.verification.groundtruth :as groundtruth]
            [clojure.string :as string]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [clojure.set :as set]))



(def ^:dynamic benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/")
(def ^:dynamic groundtruths-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/groundtruths/")

(defn best-file-match [case-name file-type files]
  (apply min-key #(levenshtein (str case-name "." file-type) (.getName %))
         files))

(defn get-case-file-in [file-type case-name path]
  (let [models (get-files-with-extension file-type path)]
    (best-file-match case-name file-type models)))

(def get-case-model-in (partial get-case-file-in "bpmn"))
(def get-case-text-in (partial get-case-file-in "txt"))
(def get-case-groundtruth-in (partial get-case-file-in "csv"))

(def get-case-model #(get-case-model-in % benchmark-path))
(def get-case-text #(get-case-text-in % benchmark-path))
(def get-case-groundtruth #(get-case-groundtruth-in % groundtruths-path))

(defn groundtruth-results [case-name]
  (let [model (get-case-model case-name)
        text (get-case-text case-name)]
    (-> (core/main-with-files model text)
        .getClojureExtraData
        :match-struct)))

;TODO: Allow changing the query path
(defn groundtruth-differences [case-name]
  (let [model (get-case-model case-name)
        text (get-case-text case-name)
        groundtruth (get-case-groundtruth case-name)]
    (groundtruth/compare-to-groundtruth
     (-> (core/main-with-files model text)
         .getClojureExtraData
         :match-struct)
     (groundtruth/parse-groundtruth
      (slurp groundtruth)))))


(defn explain-differences [case-name]
  (pprint
   (groundtruth/explain-comparison
    (groundtruth-differences case-name))))

(defn summarize-results [case-name]
  (let [model (get-case-model case-name)
        text (get-case-text case-name)]
    (groundtruth/print-assignment
      (-> (core/main-with-files model text)
          .getClojureExtraData
          :match-struct))))

(defn full-execution-log [case-name]
  (let [model (get-case-model case-name)
        text (get-case-text case-name)]
    (println
      (.getLog (core/main-with-files model text)))))

(defn groundtruth-csv-table [case-name]
  (let [groundtruth (get-case-groundtruth case-name)]
    (clojure.inspector/inspect-table (csv/read-csv (slurp groundtruth) :separator \;))))

(defn analyzed-json [case-name]
  (let [text-file (get-case-text case-name)]
    (edu.upc.modelvsdocument.textserver/analyze-cached :text (slurp text-file))))

(defn analyzed-json-non-cached [case-name]
  (let [text-file (get-case-text case-name)]
    (edu.upc.modelvsdocument.textserver/analyze-with-textserver :text (slurp text-file))))

(defn run-main [case-name]
  (let [text-file (get-case-text case-name)
        model-file (get-case-model case-name)]
    (core/main-with-files model-file text-file)))

(defn all-case-names []
  (->> benchmark-path io/file file-seq (map strip-extension) (remove nil?) distinct))

(def --new-weights
  {:has-lemma (* 10 0.11651) 
   :has-parent-synset (* 0.25 0.19204)
   :has-synset (* 0.25 0.42809)

   :in-agent (* 1.75 0.0783)
   :agent-head (* 1.75 0.125)

   :in-patient (* 10 0.0783)
   :patient-head (* 10 0.125)

   :has-form (* 0.5 0.48355)
   :lemma-conditional-pred (* 2 0.55536)
   :has-action (* 10 0.69586)
   :lemma-conditional-follow (* 2 0.82183)
   :has-discourse-marker 1.0})

(defn init-default []
  ;; LOCAL FREELING
  (edu.upc.nlp4bpm-commons.freeling-api/set-mode "local")

  (edu.upc.nlp4bpm-commons.core/init-freeling)

  ;; CACHE INITIALIZATION
  (cache/initialize (str (System/getProperty "user.home") "/.textserver-cache.clj"))

  ;; READ WORDNET DICTIONARIES
  (wordnet/read-wordnet-dictionaries "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/")

  ;; CONFIG
  (alter-var-root #'edu.upc.modelvsdocument.config/config assoc
                  :use-gurobi true, :enable-cache true, :enable-anchors true, :use-vu-similarity false
                  :feature-weight-overrides --new-weights
                  :gurobi-tmp-path "BPMN/tmp"))

(comment
  "REPL"

  (init-default)


  (def --result (with-config
                  (merge (read-string (slurp "/home/josep/BPMN/config/modelvsdocument-conf.clj"))
                         {:create-log true
                          :verbose-log true})
                  (core/main-with-files (io/file "/home/josep/ModelsBpmn/Zoo.bpmn")
                                        (io/file "/home/josep/ModelsBpmn/Zoo.txt"))))

  (spit "/tmp/log" (.getLog --result))

  ;; Example1. Run a case
  (def --result (with-config
                  {:feature-weight-overrides {:has-lemma 0.33388, :has-parent-synset 0.11518, :in-agent 0.14035, :agent-head 0.08782, :has-form 0.82331, :patient-head 0.70631, :in-patient 0.00724, :lemma-conditional-pred 0.71281, :has-discourse-marker 0.94884, :has-synset 0.6623, :has-action 0.73167, :lemma-conditional-follow 0.30683}
                   :enable-cutoff true
                   :enable-anchors false
                   :threshold-missing 0.2
                   :threshold-misplaced 0.2}
                  (core/main-with-files (io/file "/home/josep/ModelsBpmn/Zoo.bpmn")
                                        (io/file "/home/josep/ModelsBpmn/Zoo.txt"))))

  (spit "/tmp/log.txt" (.getLog --result))

  (def --result2
    (with-config
                  {:feature-weight-overrides {:has-lemma 0.33388, :has-parent-synset 0.11518, :in-agent 0.14035, :agent-head 0.08782, :has-form 0.82331, :patient-head 0.70631, :in-patient 0.00724, :lemma-conditional-pred 0.71281, :has-discourse-marker 0.94884, :has-synset 0.6623, :has-action 0.73167, :lemma-conditional-follow 0.30683}
                   :enable-cutoff true
                   :enable-anchors true
                   :threshold-missing 0.2
                   :threshold-misplaced 0.2}
                  (core/main-with-files (io/file "/home/josep/NewBenchmark/Hospital2.bpmn")
                                        (io/file "/home/josep/NewBenchmark/Hospital2.txt"))))

  (doseq [m (get-files-with-extension "bpmn" "/home/josep/DSS_BENCHMARK/Models/")]
    (bpmn/construct-model (.getAbsolutePath m)))

  (ns-unalias *ns* 'groundtruth)

  (require '[edu.upc.modelvsdocument.verification.new-groundtruth :as groundtruth])

  (def --gt1 (:new-match-struct (.getClojureExtraData --result)))

  (def --gt2 (groundtruth/load-json (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth/Zoo.json")))

  (groundtruth/compare-groundtruths --gt1 --gt2)

  (spit "/tmp/log1.txt" (.getLog --result))

  (->> --result
       .getClojureExtraData
       :new-match-struct)


  (defn get-fn-name [fn]
    (second (re-matches #".*\$(.*)__.*" (str fn))))

  (do (require '[edu.upc.modelvsdocument.similarity :as sim])
      (require '[edu.upc.modelvsdocument.extraction.feature :as f])
      (require '[edu.upc.modelvsdocument.alignable :as a]))

  (do (def models (sort-by #(.getName %) (get-files-with-extension "bpmn" "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/auto-benchmark")))
      (def texts (sort-by #(.getName %) (get-files-with-extension "txt" "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/auto-benchmark"))))

  (def cached-main (memoize core/main-with-files))

  (doseq [similarity-fn [(fn sqrt-all-min [a b] (Math/sqrt (* (count (f/fv-intersection a b)) (sim/weighted-fake-jaccard a b))))
                         (fn sqrt-all-intersection [a b] (Math/sqrt (* (count (f/fv-intersection a b)) (sim/weighted-fake-jaccard a b))))
                         (fn weighted-overlapping [a b] (sim/weighted-fake-jaccard a b))
                         (fn weighted-overlapping [a b] (sim/weighted-fake-jaccard a b))
                         (fn times-intersection [a b] (* (count (f/fv-intersection a b)) (sim/weighted-fake-jaccard a b)))
                         (fn times-union [a b] (* (count (f/fv-union a b)) (sim/weighted-fake-jaccard a b)))
                         (fn times-longest [a b] (* (max (count a) (count b)) (sim/weighted-fake-jaccard a b)))
                         (fn times-shortest [a b] (* (min (count a) (count b)) (sim/weighted-fake-jaccard a b)))
                         (fn times-log-shortest [a b] (* (Math/log (+ Math/E (min (count a) (count b)))) (sim/weighted-fake-jaccard a b)))
                         (fn times-log-intersection [a b] (* (Math/log (+ Math/E (count (f/fv-intersection a b)))) (sim/weighted-fake-jaccard a b)))
                         (fn times-sqrt-intersection [a b] (* (Math/sqrt (count (f/fv-intersection a b))) (sim/weighted-fake-jaccard a b)))
                         (fn times-sqrt-shortest [a b] (* (Math/sqrt(min (count a) (count b))) (sim/weighted-fake-jaccard a b)))]]
    (spit (io/file "/home/josep/similarity_functions/" (str (get-fn-name similarity-fn) ".csv"))
          (apply
           str
           (for [[m t] (map vector models texts)
                 :let [result (cached-main m t)
                       text-alignables  (:text-alignables (.getClojureExtraData result))
                       model-alignables (:model-alignables (.getClojureExtraData result))
                       text-features    (:text-features (.getClojureExtraData result))
                       model-features   (:model-features (.getClojureExtraData result))]]
             (do
               (println "Processing " (.getName m))
               (with-out-str
                 (doseq [s text-alignables
                         t model-alignables
                         :let [A (text-features s)
                               B (model-features t)
                               |A| (count A)
                               |B| (count B)
                               sim (similarity-fn A B)]]
                   (println (str |A| "," |B| "," (format "%.4f" (float sim)) "," (.getName m) ":" (a/id s) ":" (a/id t))))))))))


  END)
