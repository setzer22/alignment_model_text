(ns edu.upc.modelvsdocument.experiments
  (:require [edu.upc.modelvsdocument.core :as core]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.config :as config]
            [edu.upc.modelvsdocument.utils :refer :all]
            [clojure.data.csv :as csv]
            [edu.upc.modelvsdocument.verification.new-groundtruth :as groundtruth]
            [edu.upc.modelvsdocument.core-predictors :as core-predictors]
            [edu.upc.modelvsdocument.verification.benchmark :as benchmark]
            [edu.upc.modelvsdocument.alignable :as al]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.text :as text]
            [com.rpl.specter :refer :all]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]))

(defn bpmn-file? [f] (= (extension f) "bpmn"))
(defn text-file? [f] (= (extension f) "txt"))

(defmacro measure-time-of
  "Small utility macro to measure execution timthe finale of code"
  [& body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)]
     {:elapsed-time (- (System/nanoTime) start#)
      :result result#}))

(defn get-benchmark-files [models-path texts-path groundtruths-path]
  (let [srt (fn srt [coll] (sort-by #(.getName %) coll))
        model-files (srt (get-files-with-extension "bpmn" models-path))
        text-files (srt (get-files-with-extension "txt" texts-path))
        groundtruth-files (srt (get-files-with-extension "json" groundtruths-path))
        common (set/intersection (into #{} (map strip-extension) model-files)
                                 (into #{} (map strip-extension) text-files)
                                 (into #{} (map strip-extension) groundtruth-files))
        model-files (filter #(common (strip-extension %)) model-files)
        text-files (filter #(common (strip-extension %)) text-files)
        groundtruth-files (filter #(common (strip-extension %)) groundtruth-files)]
    {:model-files model-files
     :text-files text-files
     :groundtruth-files groundtruth-files}))

(defn predictors-experiment
  "In this experiments we are going to evaluate how different cap paperpredictors influence
   alignment quality (i.e. accuracy) when varying their threshold."
  []
  (let+ [#_"Paths definition"
         models-path "/home/josep/DSS_BENCHMARK/OldDataset/Models/"
         texts-path "/home/josep/DSS_BENCHMARK/OldDataset/Texts/"
         groundtruths-path "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth"

         output-path "/home/josep/experiment-predictors-new/"
         rel-path (fn [filename] (.getAbsolutePath (io/file output-path filename)))

         #_"We first load the data to run the benchmark"
         {:keys [model-files text-files groundtruth-files]}
         (get-benchmark-files models-path texts-path groundtruths-path)

         groundtruths (->> groundtruth-files (map slurp) (map groundtruth/load-json))
         __ (assert (every? #(apply = %) (apply map vector (map #(map strip-extension %) [model-files text-files groundtruth-files])))
                    "There must be the same set of text, bpmn and json files in the benchmark folders.")

         config {:use-vu-similarity false
                 :gurobi-tmp-path "BPMN/tmp/"
                 :enable-anchors true
                 :enable-cutoff true
                 :enable-wrong-order-cutoff true
                 :return-trivial-alignment false
                 :similarity-function "weighted-fake-jaccard"
                 :remove-back-edges? true
                 :enable-cache true
                 :feature-weight-overrides {:has-lemma 0.01226, :has-parent-synset 0.03927, :in-agent 0.19403,
                                            :agent-head 0.03089, :has-form 0.82536, :patient-head 0.37729,
                                            :in-patient 0.84569, :has-discourse-marker 0.83401, :has-synset 0.5691,
                                            :has-action 0.95106}}

         #_"Then, we define a function that given a predictor type will return the list of accuracies"
         run-benchmark
         (fn run-benchmark [predictor-types, thresholds]
           #_"@CopyPasted from benchmark.clj"
           (let [pred-comparisons (doall
                                   (map-progress
                                    (fn [model-file text-file groundtruth]
                                      (let [results (config/with-config config
                                                      (core-predictors/main-with-files model-file
                                                                                       text-file
                                                                                       predictor-types
                                                                                       thresholds))]
                                        (transform [MAP-VALS ALL]
                                                   (fn [gt-structure]
                                                     (def --gt-structure gt-structure)
                                                     (def --groundtruth groundtruth)
                                                     (groundtruth/compare-extended-groundtruths
                                                      true
                                                      (bpmn/construct-model (.getAbsolutePath model-file))
                                                      groundtruth
                                                      gt-structure))
                                                   results)))
                                    model-files text-files groundtruths))
                 __ (def --pred-comparisons pred-comparisons)]
             (for [predictor predictor-types
                   :let [comparisons (transpose (map predictor pred-comparisons))]]
               (transform [ALL]
                          (fn [comparison]
                            (float (avg (map #(-> % :all :accuracy) comparison))))
                          comparisons))))

         #_"We will also need a function to export the results as csv data."
         export-csv
         (fn export-csv [predictor-outputs]
           (with-open [w (io/writer (io/file output-path "data.csv"))]
             (let [outputs (transform [ALL ALL] #(format "%.5f" (float %)) predictor-outputs)]
               (csv/write-csv w
                              (transpose outputs)))))

         r-colors
         (into ["black" "red" "green" "blue" "orange"  "purple" "brown" "forestgreen"]
               (sort-by hash
                        ["aliceblue", "antiquewhite", "aquamarine","azure", "beige", "bisque", "black", "blanchedalmond", "blue", "blueviolet", "brown", "burlywood", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "deeppink", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick", "floralwhite", "forestgreen", "gainsboro", "ghostwhite", "gold" "goldenrod"]))

         #_"We dynamically generate the R script that generates the plot using the csv data"
         generate-plot-script
         (fn generate-plot-script [predictor-types thresholds]
           (let+ [colors r-colors
                  l (fn [& args] (println (apply format args)))
                  r-script-str :dbg
                  (with-out-str
                    (l "data <- read.csv('%s', header = FALSE)" (rel-path "data.csv"))
                    (l "x <- c(%s)" (clojure.string/join "," thresholds))
                    (l "png('%s', width=%d, height=800)" (rel-path "plot.png")
                       (int (* 800 (max 1 (- (apply max thresholds) (apply min thresholds))))))
                    (l "plot(data$V1, x=x, type='l', col='%s', ylim = c(0.0,1.0),
                     main='Accuracy with varying thresholds', xlab = 'Threshold',
                     ylab = 'Accuracy Micro-Average')" (first colors))
                    (doseq [i (->> predictor-types count range (drop 1))]
                      (l "lines(data$V%d, x=x, col='%s')" (inc i) (colors i)))
                    (doseq [i (->> predictor-types count range)]
                      (l "text(x=-0.1, y=%f, labels = '%s', col = '%s')"
                         (- 0.98 (* i 0.04)) (name (predictor-types i)) (colors i)))
                    (l "dev.off"))]
             (spit (rel-path "plot.R") r-script-str)
             (sh "Rscript" "plot.R" :dir output-path)))

         open-plot
         (fn open-plot []
           (future (sh "xdg-open" (rel-path "plot.png"))))]

    (let+ [thresholds (range -0.1 1.1 0.05)
           predictor-types [:p-sim :p-rel :p-rel-S]
           result :dbg (run-benchmark predictor-types thresholds)]
      (export-csv result)
      (generate-plot-script predictor-types thresholds)
      (open-plot))))

(comment

  (do
    (require '[edu.upc.modelvsdocument.repl.comparison-queries])
    (edu.upc.modelvsdocument.repl.comparison-queries/init-default))

  (predictors-experiment)

  --gt-structure

  --groundtruth

  END)


(defn section5-experiment-new-benchmark
  "In this experiments we are going to evaluate the quality of the technique for the new
   benchmark. Alignment accuracy will be grouped by element type (activity, event, gateway)
  and also at the global level"
  [config]
  (let+ [{:keys [model-files text-files groundtruth-files]}
         (get-benchmark-files
          "/home/josep/DSS_BENCHMARK/Models"
          "/home/josep/DSS_BENCHMARK/Texts"
          "/home/josep/DSS_BENCHMARK/AutoGroundtruth")]
    (benchmark/run-extended-benchmark model-files text-files groundtruth-files config)))

(defn section5-experiment-old-benchmark
  "In this experiments we are going to evaluate the quality of the technique for the original
   benchmark, extended with gateway and event groundtruth alignments. Alignment accuracy will be
   grouped by element type (activity, event, gateway) and also at the global level"
  [config]
  (let+ [{:keys [model-files text-files groundtruth-files] :as m}
         (get-benchmark-files
          "/home/josep/DSS_BENCHMARK/OldDataset/Models"
          "/home/josep/DSS_BENCHMARK/OldDataset/Texts"
          "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth")]
    (benchmark/run-extended-benchmark model-files text-files groundtruth-files config)))

(defn generate-latex-table
  "This function runs the experiment with several parameter configurations and prints a LaTeX
   table to stdout that is suitable for pasting into the DSS paper."
  []
  (let [#_"OLD DATASET"
        {:keys [old-model-files old-text-files old-groundtruth-files] :as m}
        (get-benchmark-files
         "/home/josep/DSS_BENCHMARK/OldDataset/Models"
         "/home/josep/DSS_BENCHMARK/OldDataset/Texts"
         "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth")

        #_"NEW DATASET"
        {:keys [model-files text-files groundtruth-files] :as m}
        (get-benchmark-files
         "/home/josep/DSS_BENCHMARK/OldDataset/Models"
         "/home/josep/DSS_BENCHMARK/OldDataset/Texts"
         "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth")

        base-config {:enable-cache true
                     :enable-tf-idf true}
        approaches {#_"Van der Aa et al.~\\cite{vanderaa2017comparing}"
                    #_{:use-vu-similarity true
                       :enable-anchors false
                       :enable-wrong-order-cutoff false
                       :enable-cutoff false}
                    #_"Sanchez et al.~\\cite{sanchez2017aligning}"
                    #_{:use-vu-similarity false
                       :enable-anchors false
                       :enable-cutoff false
                       :enable-wrong-order-cutoff false
                       :use-old-label-parser true
                       :feature-weight-overrides
                       {:has-lemma 0.11651, :has-parent-synset 0.19204, :in-agent 0.9843, :agent-head 0.2408,
                        :has-form 0.48355, :patient-head 0.59129, :in-patient 0.04503, :lemma-conditional-pred 0.55536,
                        :has-synset 0.42809, :has-action 0.69586, :lemma-conditional-follow 0.82183}}
                    "Our approach"
                    {:use-vu-similarity false
                     :enable-anchors true
                     :enable-cutoff true
                     :enable-wrong-order-cutoff true
                     :missing-predictor-type :p-rel-S
                     :threshold-missing 0.1
                     :max-constrained-threshold 0.99
                     :remove-back-edges? true}
                    "Our approach (no anchors)"
                    {:use-vu-similarity false
                     :enable-anchors false
                     :enable-cutoff false
                     :enable-wrong-order-cutoff false
                     :remove-back-edges? true}
                    "Our Approach (without predictors)"
                    {:use-vu-similarity false
                     :enable-anchors true
                     :enable-cutoff false
                     :enable-wrong-order-cutoff false
                     :remove-back-edges? true}}
        fmt #(try-or (format "%.3f" (double %)) "--")]
    (println "====== OLD DATASET =========")
    (eager-for [[name config] approaches]
               (let [cmp (section5-experiment-old-benchmark config)]
                 (println name "&"
                          (-> cmp :task fmt) "&"
                          (-> cmp :event fmt) "&"
                          (-> cmp :gateway fmt) "&"
                          (-> cmp :all fmt) "\\\\")))
    (println "======= NEW DATASET ========")
    (eager-for [[name config] approaches]
               (let [cmp (section5-experiment-new-benchmark config)]
                 (println name "&"
                          (-> cmp :task fmt) "&"
                          (-> cmp :event fmt) "&"
                          (-> cmp :gateway fmt) "&"
                          (-> cmp :all fmt) "\\\\")))))

(comment

  (do
    (require '[edu.upc.modelvsdocument.repl.comparison-queries])
    (edu.upc.modelvsdocument.repl.comparison-queries/init-default))

  (section5-experiment-old-benchmark
   {:use-vu-similarity true
    :enable-anchors false
    :enable-cutoff true
    :enable-wrong-order-cutoff false
    :missing-predictor-type :p-rel-S
    :threshold-missing 0.1
    :max-constrained-threshold 0.90
    :remove-back-edges? false
    :feature-weight-overrides {:has-lemma 0.01226, :has-parent-synset 0.03927, :in-agent 0.19403,
                               :agent-head 0.03089, :has-form 0.82536, :patient-head 0.37729,
                               :in-patient 0.84569, :has-discourse-marker 0.83401, :has-synset 0.5691,
                               :has-action 0.95106}})

  
  ;; Old dataset:
  ;; VU: {:task 0.7802666476238704}
  ;; UPC: {:task 0.7646813464002713}

  ;; New dataset:
  ;; VU: {:task 0.7733665223665224}
  ;; UPC: {:task 0.7891486291486293}

  (generate-latex-table)

  (transform
   (walker :accuracy)
   (fn [m] (assoc m :accuracy (try-or (float (:accuracy m)) m)))
   --case-results)
  END)

(defn dataset-overview [models-path texts-path groundtruths-path]
  (let [{:keys [model-files text-files groundtruth-files]}
        (get-benchmark-files models-path texts-path groundtruths-path)
        how-many? (fn [pred coll] (count (filter pred coll)))
        models-data (map
                     (fn get-model-data [m]
                       (let [model (bpmn/construct-model (.getAbsolutePath m))
                             nodes (filter (partial bpmn/alignable? model) (bpmn/all-flow-elements model))]
                         {:num-tasks (how-many? bpmn/task? nodes)
                          :num-events (how-many? bpmn/event? nodes)
                          :num-gateways (how-many? bpmn/gateway? nodes)
                          :num-nodes (count nodes)}))
                     model-files)
        texts-data (map
                    (fn get-text-data [t]
                      (let [text (text/analyze :text (slurp t))]
                        {:num-sentences (how-many? text/sentence? (al/get-all-alignables text))}))
                    text-files)
        #_global-counts #_(reduce
                           (fn [{:keys [avg-tasks avg-events avg-gateways avg-sentences]
                                 :or {avg-tasks 0 avg-events 0 avg-gateways 0 avg-sentences 0}}
                                [{:keys [num-tasks num-events num-gateways] :as model-data}
                                 {:keys [num-sentences] :as text-data}]]
                             {:avg-tasks (+ avg-tasks num-tasks)
                              :avg-events (+ avg-events num-events)
                              :avg-gateways (+ avg-gateways num-gateways)
                              :avg-sentences (+ avg-sentences num-sentences)})
                           (map vector models-data texts-data))
        #_avg-sentences-per-node #_(avg
                                    (map (fn [{:keys [num-nodes]} {:keys [num-sentences]}]
                                           (/ num-sentences num-nodes))
                                         models-data texts-data))
        N (count model-files)]
    (map merge models-data texts-data)
    #_{:avg-tasks (double (/ (:avg-tasks global-counts) N))
       :avg-events (double (/ (:avg-events global-counts) N))
       :avg-gateways (double (/ (:avg-gateways global-counts) N))
       :avg-sentences (double (/ (:avg-sentences global-counts) N))
       :avg-sentences-per-node (double avg-sentences-per-node)}))

(comment
  ;; NEW DATASET

  (defn summary [data]
    (let [min (try-or (apply min data) nil)
          max (try-or (apply max data) nil)
          avg (try-or (float (/ (reduce + data) (count data))) nil)
          median (try-or (nth (sort data) (/ (count data) 2)) nil)]
      {:min min
       :max max
       :avg avg
       :median median}))

  (spit
   "/tmp/boxplot-data.csv"
   (clojure.string/join
    "\n"
    (map #(clojure.string/join "," %)
         (concat
          (map (juxt (constantly "Original") :num-nodes :num-tasks :num-gateways :num-events :num-sentences)
               (dataset-overview "/home/josep/DSS_BENCHMARK/OldDataset/Models"
                                 "/home/josep/DSS_BENCHMARK/OldDataset/Texts"
                                 "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth"))
          (map (juxt (constantly "New") :num-nodes :num-tasks :num-gateways :num-events :num-sentences)
               (dataset-overview "/home/josep/DSS_BENCHMARK/Models"
                                 "/home/josep/DSS_BENCHMARK/Texts"
                                 "/home/josep/DSS_BENCHMARK/AutoGroundtruth"))))))

  (spit
   "/tmp/boxplot-data.csv"
   (clojure.string/join
    "\n"
    (map #(clojure.string/join "," %)
         )))


  (summary (map :num-gateways))

  (summary (map :num-tasks (dataset-overview "/home/josep/DSS_BENCHMARK/Models"
                                             "/home/josep/DSS_BENCHMARK/Texts"
                                             "/home/josep/DSS_BENCHMARK/AutoGroundtruth")))

  (summary (map :num-events (dataset-overview "/home/josep/DSS_BENCHMARK/Models"
                                              "/home/josep/DSS_BENCHMARK/Texts"
                                              "/home/josep/DSS_BENCHMARK/AutoGroundtruth")))



  (+ 1 1)

  (dataset-overview "/home/josep/DSS_BENCHMARK/OldDataset/Models"
                    "/home/josep/DSS_BENCHMARK/OldDataset/Texts"
                    "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth")


  (map (fn [a b] (float (/ (+ (* 49 a) (* 25 b)) (+ 49 25))))
       [8.12 , 1.66 , 2.21 , 9.13 , 0.76]
       [7.44 , 2.80 , 2.16 , 7.72 , 0.65])

  (def --missing-in-new (count
                         (select
                          [ALL :gt/errors MAP-VALS #(= % "m")]
                          (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/AutoGroundtruth")
                               (map slurp)
                               (map groundtruth/load-json)))))

  (select
   [ALL :gt/errors ALL #(.contains (first %) "Gateway")]
   (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/AutoGroundtruth")
        (map slurp)
        (map groundtruth/load-json)))

  (float (/ 

          (count
           (select
            [ALL :gt/errors MAP-VALS #(= % "m")]
            (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth")
                 (map slurp)
                 (map groundtruth/load-json))))
          (count (select
                  [ALL :gt/tasks ALL]
                  (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth")
                       (map slurp)
                       (map groundtruth/load-json))))))

  ;; 0.0825
  ;; 0.133

  (.contains "astrophysics" "a")

  (float (/
          (count (select
                  [ALL MAP-KEYS #(or (.contains % "Event") (.contains % "Task"))]
                  (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/AutoGroundtruth")
                       (map slurp)
                       (map groundtruth/load-json)
                       (map :gt/errors))))
          (count (select
                  [ALL MAP-KEYS #(or (.contains % "Event") (.contains % "Task"))]
                  (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/AutoGroundtruth")
                       (map slurp)
                       (map groundtruth/load-json)
                       (map :gt/alignment))))))


  (def --missing-in-new (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/AutoGroundtruth")
                             (map slurp)
                             (map groundtruth/load-json)
                             (map :gt/errors)
                             (filter (fn any-m? [map]
                                       (some #(= "m" %) (vals map))))
                             (count)))

  (def --missing-in-old (->> (get-files-with-extension "json" "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth")
                             (map slurp)
                             (map groundtruth/load-json)
                             (map :gt/errors)
                             (filter (fn any-m? [map]
                                       (some #(= "m" %) (vals map))))
                             (count)))

  --missing-in-new
  --missing-in-old

  --new-gts

  END)

(defn genetic-plot []
  (let [data (->> (read-string (slurp "/home/josep/Experiment_Genetic/fitnesses"))
                  (map (fn [seq] [(apply max seq) (apply min seq) (avg seq)])))]
    (with-open [w (io/writer "/home/josep/Experiment_Genetic/data.csv")]
      (csv/write-csv w data))))


(comment
  (genetic-plot)

  END)


(comment

  "We are going to check the accuracy of the tool vs model size"

  (def
    --new-results
    (config/with-config
      {:use-vu-similarity false
       :enable-anchors true
       :enable-cutoff true
       :enable-wrong-order-cutoff false
       :missing-predictor-type :p-rel-S
       :threshold-missing 0.1
       :max-constrained-threshold 0.90
       :remove-back-edges? true
       :feature-weight-overrides {:has-lemma 0.01226, :has-parent-synset 0.03927, :in-agent 0.19403,
                                  :agent-head 0.03089, :has-form 0.82536, :patient-head 0.37729,
                                  :in-patient 0.84569, :has-discourse-marker 0.83401, :has-synset 0.5691,
                                  :has-action 0.95106}}


      (let [#_"We first load the data to run the benchmark"
            {:keys [model-files text-files groundtruth-files]}
            (get-benchmark-files
             "/home/josep/DSS_BENCHMARK/Models/"
             "/home/josep/DSS_BENCHMARK/Texts/"
             "/home/josep/DSS_BENCHMARK/AutoGroundtruth/")]

        #_"Run the benchmark to generate the --case-results debug variable"
        (benchmark/run-extended-benchmark model-files text-files groundtruth-files {})

        #_"Get what we want from --case-results"
        (map #(-> [(strip-extension %1)
                   (count
                    (edu.upc.modelvsdocument.alignable/get-all-alignables
                     (edu.upc.modelvsdocument.bpmn-alignable/analyze (.getAbsolutePath %1) "")))
                   (-> %2 :all :accuracy float)])
             model-files
             benchmark/--case-results))))

  (def
    --old-results
    (config/with-config
      {:use-vu-similarity false
       :enable-anchors true
       :enable-cutoff true
       :enable-wrong-order-cutoff false
       :missing-predictor-type :p-rel-S
       :threshold-missing 0.1
       :max-constrained-threshold 0.90
       :remove-back-edges? true
       :feature-weight-overrides {:has-lemma 0.01226, :has-parent-synset 0.03927, :in-agent 0.19403,
                                  :agent-head 0.03089, :has-form 0.82536, :patient-head 0.37729,
                                  :in-patient 0.84569, :has-discourse-marker 0.83401, :has-synset 0.5691,
                                  :has-action 0.95106}}


      (let [#_"We first load the data to run the benchmark"
            {:keys [model-files text-files groundtruth-files]}
            (get-benchmark-files
             "/home/josep/DSS_BENCHMARK/OldDataset/Models"
             "/home/josep/DSS_BENCHMARK/OldDataset/Texts/"
             "/home/josep/DSS_BENCHMARK/OldDataset/AutoExpandedGroundtruth/")]

        #_"Run the benchmark to generate the --case-results debug variable"
        (benchmark/run-extended-benchmark model-files text-files groundtruth-files {})

        #_"Get what we want from --case-results"
        (map #(-> [(strip-extension %1)
                   (count
                    (edu.upc.modelvsdocument.alignable/get-all-alignables
                     (edu.upc.modelvsdocument.bpmn-alignable/analyze (.getAbsolutePath %1) "")))
                   (-> %2 :all :accuracy float)])
             model-files
             benchmark/--case-results))))


  (spit "/home/josep/DSS_BENCHMARK/accuracy_vs_size.csv"
   (clojure.string/join "\n"
                        (map
                         #(clojure.string/join "," %)
                         (concat --new-results
                                 --old-results))))

  (map vector
       (map # benchmark/--case-results))

  (get-benchmark-files models-path texts-path groundtruths-path)

  )
