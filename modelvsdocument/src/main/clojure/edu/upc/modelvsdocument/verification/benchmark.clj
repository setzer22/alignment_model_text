(ns edu.upc.modelvsdocument.verification.benchmark
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [random-seed.core :refer [set-random-seed!]]
            [edu.upc.nlp4bpm-commons.cache]
            [edu.upc.modelvsdocument.verification.new-groundtruth :as groundtruth]
            [edu.upc.modelvsdocument.utils :as utils :refer :all]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.config :as config :refer [with-config config]]
            [edu.upc.modelvsdocument.bpmn-alignable :as b]
            [edu.upc.modelvsdocument.text :as t]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.config :as config :refer [config with-config]]
            [com.rpl.specter :refer :all]))

(defn mean [x]
  (if (seq x) (/ (reduce + x) (count x)) 0))

(defn median [x]
  (let [n (count x)
        sx (sort x)]
    (cond (zero? n) 0
          (odd? n) (nth sx (/ n 2))
          (even? n) (/ (+ (nth sx (- (/ n 2) 1)) (nth sx (/ n 2))) 2))))

(defn my-pmap
  "Like map, except f is applied in parallel. Semi-lazy in that the
  parallel computation stays ahead of the consumption, but doesn't
  realize the entire result unless required. Only useful for
  computationally intensive functions where the time of f dominates
  the coordination overhead."
  ([n f coll]
   (let [rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))
  ([n f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (my-pmap n #(apply f %) (step (cons coll colls))))))

(defn run-benchmark
  "Runs the benchmark, consisting of two paths:
   - The benchmark path contains model-text pa(setq parinfer-auto-switch-indent-mode-when-closing nil)irs in .txt and .bpmn formats
   - The groundtruth path contains a json groundtruth file for each model-text pair.
   Optionally, also takes:
   - A list of config overrides. Defaults to a single config with no overrides
   - A different function for the algorithm. Should be equivalent to core/main-with-files
   Returns a list containing, for each config override:
   - The individual accuracies for each of the models.
   - Micro/Macro averages and meadian.
   - Fine-grained differences between all the cases."
  ([benchmark-path groundtruths-path]
   (run-benchmark benchmark-path groundtruths-path [{}]))
  ([benchmark-path groundtruths-path config-overrides]
   (run-benchmark benchmark-path groundtruths-path config-overrides #'core/main-with-files))
  ([benchmark-path groundtruths-path config-overrides result-fn]
   (let [model-files (sort (get-files-with-extension "bpmn" benchmark-path))
         text-files (sort (get-files-with-extension "txt" benchmark-path))
         groundtruth-files (sort (get-files-with-extension "json" groundtruths-path))

         __ (assert (every? #(apply = %) (apply map vector (map #(map strip-extension %) [model-files text-files groundtruth-files])))
                    "There must be the same set of text, bpmn and json files in the benchmark folders.")

         groundtruths (->> groundtruth-files (map slurp) (map groundtruth/load-json))

         comparisons
         (doall
          (for [config-override config-overrides]
            (with-config config-override
              (doall
               (map
                (fn [model-file text-file groundtruth]
                  (let [start-time (System/currentTimeMillis)
                        ;;__ (println "Processing " (strip-extension model-file))
                        result (result-fn model-file text-file)
                        elapsed-time (- (System/currentTimeMillis) start-time)
                        result-gt (:new-match-struct (.getClojureExtraData result))]
                    (assoc (groundtruth/compare-groundtruths groundtruth result-gt)
                           :case-name (strip-extension model-file)
                           :execution-time elapsed-time
                           :max-constrained (:max-constrained (.getClojureExtraData result)))))
                model-files text-files groundtruths)))))
         micro-avg (for [comparison comparisons]
                     (mean (map :accuracy comparison)))
         macro-avg (for [comparison comparisons]
                     (/ (reduce + (map :correct comparison))
                        (reduce + (map :total comparison))))
         mdn       (for [comparison comparisons]
                     (median (map :accuracy comparison)))]

     {:comparisons (into [] comparisons)
      :micro-average micro-avg
      :macro-average macro-avg
      :median mdn})))

(defn export-to-csv [results out-path]
  (with-open [out-file (clojure.java.io/writer out-path)]
    (csv/write-csv out-file
                   (concat
                    [(concat
                      ["Case Name" "#Total"]
                      (flatten (map-indexed
                                (fn [idx _]
                                  (vector (str "Correct (" idx")")
                                          (str "Accuracy (" idx")")
                                          (str "Max-Constrained (" idx")")))
                                (:comparisons results))))]
                    (apply
                     map concat
                     (mapv #(vector (-> % first :case-name)
                                    (-> % first :total))
                           (apply map vector (:comparisons results)))
                     (for [comparison (:comparisons results)]
                       (map #(vector (-> % :correct)
                                     (format "%.3f" (-> % :accuracy float))
                                     (format "%.3f" (-> % :max-constrained float)))
                            comparison)))
                    [(concat ["Micro-Average"] (map #(format "%.4f" (float %)) (:micro-average results)))]
                    [(concat ["Macro-Average"] (map #(format "%.4f" (float %)) (:macro-average results)))]
                    [(concat ["Median"] (map #(format "%.4f" (float %)) (:median results)))]))))

(defn run-extended-benchmark
  ([model-files text-files groundtruth-files config]
   (run-extended-benchmark model-files text-files groundtruth-files config 1))
  ([model-files text-files groundtruth-files config num-cores]
   (let [merge-case-results
         (fn merge-case-results [case-results]
           (def --case-results case-results)
           (transform
            [MAP-VALS]
            (fn [{:keys [accuracy count]}] (if (= "NA" accuracy)
                                             "NA"
                                             (double (/ accuracy count))))
            (apply merge-with
                   (fn [case case']
                     (cond
                       (= "NA" (:accuracy case)) case'
                       (= "NA" (:accuracy case')) case
                       :else
                       {:accuracy (+ (:accuracy case) (:accuracy case'))
                        :count (+ (get case :count 1.0) (get case' :count 1.0))}))
                   case-results)))

         case-results
         (config/with-config config
           (doall
            (map-progress
             (fn [[m t g]]
               (let [{:keys [model extended-match-struct error]}
                     (do
                       (println "Executing" (strip-extension m))
                       (.getClojureExtraData (core/main-with-files m t))
                       #_(try (.getClojureExtraData (core/main-with-files m t))
                            (catch Exception e
                              (println "[WARNING] Error in case" (strip-extension m))
                              (println (.toString e))
                              {:error true})))
                     groundtruth (groundtruth/load-json (slurp g))
                     cmp (if error {:task {:accuracy "NA"}}
                           (groundtruth/compare-extended-groundtruths
                            (config/config :enable-anchors)
                            model
                            extended-match-struct
                            groundtruth))
                     fmt #(try-or (format "%.2f" (double %)) %)]
                 cmp))
             (map vector model-files text-files groundtruth-files))))
         __ (def --case-results (map vector (map strip-extension model-files) case-results))]
     (merge-case-results case-results))))

(comment
  --case-results

  END)


(comment
  BEGIN REPL

  (do
    (require '[edu.upc.modelvsdocument.repl.comparison-queries])
    (edu.upc.modelvsdocument.repl.comparison-queries/init-default))

  
  (def --weights-without-anchors
    {:has-lemma 0.28581, :has-parent-synset 0.37634, :in-agent 0.10592, :agent-head 0.03468, :has-form 0.9829, :patient-head 0.25405, :in-patient 0.30367, :lemma-conditional-pred 0.99334, :has-discourse-marker 0.39743, :has-synset 0.38369, :has-action 0.92101, :lemma-conditional-follow 0.06977})


  (def --weights-for-anchors
    {:has-lemma 0.33388, :has-parent-synset 0.11518, :in-agent 0.14035, :agent-head 0.08782, :has-form 0.82331, :patient-head 0.70631, :in-patient 0.00724, :lemma-conditional-pred 0.71281, :has-discourse-marker 0.94884, :has-synset 0.6623, :has-action 0.73167, :lemma-conditional-follow 0.30683})

  (def --old-new-weights
    {:has-lemma (* 0.5 0.11651) :has-parent-synset 0.19204 :in-agent (* 1.75 0.0783) :agent-head (* 1.75 0.125) :in-patient (* 1.75 0.0783) :patient-head (* 1.75 0.125) :has-form 0.48355 :lemma-conditional-pred 0.55536 :has-synset 0.42809 :has-action 0.69586 :lemma-conditional-follow 0.82183})
  
  (def --config
    {:enable-cache true, :use-gurobi true, :gurobi-tmp-path "BPMN/tmp/" :return-trivial-alignment false :similarity-function "weighted-fake-jaccard"})


  (do 
    (def --result
      (with-config (merge --config {:enable-cutoff false
                                    :threshold-missing 0.4
                                    :threshold-misplaced 0.4})
        (run-benchmark "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/auto-benchmark"
                       "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth"
                       [{:use-old-label-parser true :enable-anchors true :feature-weight-overrides --old-new-weights
                         :similarity-function "weighted-fake-jaccard"}
                        {:use-old-label-parser true :enable-anchors false :feature-weight-overrides --old-new-weights
                         :similarity-function "weighted-fake-jaccard"}
                        {:use-old-label-parser false :enable-anchors true :feature-weight-overrides --weights-for-anchors
                         :similarity-function "weighted-fake-jaccard"}
                        {:use-old-label-parser false :enable-anchors false :feature-weight-overrides --weights-for-anchors
                         :similarity-function "weighted-fake-jaccard"}])))
    (export-to-csv --result "/home/josep/test.csv")
    (clojure.java.shell/sh "notify-send" "done!"))


  ;;WIL: Accuracy is lower?

  ;; resuĺtats-predictors
  (let [bar (mk-progress-bar)
        progress (atom 0)]
    (doseq [threshold [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]]
      (def --result
        (with-config (merge --config {:enable-cutoff true
                                      :threshold-missing threshold
                                      :threshold-misplaced threshold})
          (run-benchmark "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/auto-benchmark"
                         "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth"
                         [{:use-old-label-parser true :enable-anchors true :feature-weight-overrides --old-new-weights
                           :similarity-function "weighted-fake-jaccard"}
                          {:use-old-label-parser true :enable-anchors false :feature-weight-overrides --old-new-weights
                           :similarity-function "weighted-fake-jaccard"}
                          {:use-old-label-parser false :enable-anchors true :feature-weight-overrides --weights-for-anchors}
                          {:use-old-label-parser false :enable-anchors false :feature-weight-overrides --weights-for-anchors
                           :similarity-function "weighted-fake-jaccard"}])))
      (export-to-csv --result (str "/home/josep/resultats-predictors/threshold-" threshold ".csv"))
      (swap! progress #(+ 10 %))
      (set-progress-bar bar @progress)))

  ;; resultats-predictors-2 (zoom in range 0-0.3)
  (let [bar (mk-progress-bar)
        progress (atom 0)]
    (doseq [threshold (range 0 0.3 0.03)]
      (def --result
        (with-config (merge --config {:enable-cutoff true
                                      :threshold-missing threshold
                                      :threshold-misplaced threshold})
          (run-benchmark "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/auto-benchmark"
                         "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth"
                         [{:use-old-label-parser true :enable-anchors true :feature-weight-overrides --old-new-weights
                           :similarity-function "weighted-fake-jaccard"}
                          {:use-old-label-parser true :enable-anchors false :feature-weight-overrides --old-new-weights
                           :similarity-function "weighted-fake-jaccard"}
                          {:use-old-label-parser false :enable-anchors true :feature-weight-overrides --weights-for-anchors}
                          {:use-old-label-parser false :enable-anchors false :feature-weight-overrides --weights-for-anchors
                           :similarity-function "weighted-fake-jaccard"}])))
      (export-to-csv --result (str "/home/josep/resultats-predictors-2/threshold-" threshold ".csv"))
      (swap! progress #(+ 10 %))
      (set-progress-bar bar @progress)))

  (def --old-weights
    {:has-lemma 0.11651, :has-parent-synset 0.19204, :in-agent 0.9843,
     :agent-head 0.2408, :has-form 0.48355, :patient-head 0.59129,
     :in-patient 0.04503, :lemma-conditional-pred 0.55536, :has-synset 0.42809,
     :has-action 0.69586, :lemma-conditional-follow 0.82183})


  ;; WIL: Why is the synset for token 27 of zoo not extracted?
  (def --text (edu.upc.modelvsdocument.text/analyze :text (slurp "/home/josep/ModelsBpmn/Zoo.txt")))
  (def --token (nth (select [:paragraphs ALL :sentences ALL :tokens ALL] --text) 27))



  (defn compare-for-p [p-neighbour p-list]
    (config/with-weights --new-weights
      (binding [edu.upc.modelvsdocument.core/p-neighbour p-neighbour
                edu.upc.modelvsdocument.core/p-list p-list]
        (with-config --config
          (run-benchmark "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/auto-benchmark"
                         "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth"
                         [{:enable-anchors true}
                          {:enable-anchors false}])))))

  (defn safe-div
    "Instead of exception, return infinity when dividing by zero."
    [a b]
    (if (zero? b) java.lang.Float/POSITIVE_INFINITY
        (/ a b)))

  (defn format-comp
    [a b]
    (str (float a) "//" (float b)))

  (defn comparison-column [result]
    (mapv #(vector %1 %2)
          (select [:comparisons FIRST ALL :accuracy] result)
          (select [:comparisons LAST ALL :accuracy] result)))

  (def --case-names
    ["BicycleManufacturing" "ClaimsCreation" "Dispatch-of-goods" "Hospital" "Hotel" "HotelService" "Model1-2" "Model1-4" "Model10-1" "Model10-10" "Model10-11" "Model10-12" "Model10-13" "Model10-14" "Model10-3" "Model10-4" "Model10-5" "Model10-6" "Model10-7" "Model10-8" "Model10-9" "Model2-1" "Model2-2" "Model3-1" "Model3-2" "Model3-3" "Model3-4" "Model3-5" "Model3-6" "Model4-1" "Model5-1" "Model5-2" "Model5-3" "Model6-2" "Model6-3" "Model6-4" "Model7-1" "Model8-1" "Model8-2" "Model8-3" "Model9-1" "Model9-3" "Model9-4" "Model9-5" "Model9-6" "Self-service-restaurant" "Underwriter" "Zoo"])


  (defn column-average [columns]
    (apply
     mapv
     (fn [& rows]
       (vector
        (float (avg (mapv first rows)))
        (float (avg (mapv second rows)))))
     columns))

  (def --cols-result
    (let [progress (mk-progress-bar)
          current-cases (atom 0)
          total-cases (* 4 1 3)]
      (do (set-random-seed! 123123)
          (doall (for [p-list (reverse (range 0.0 1.0 0.3))
                       p-neighbour [1/3]]
                   (column-average
                    (remove nil?
                            (for [rep (range 3)]
                              (do (swap! current-cases inc)
                                  (set-progress-bar progress (int (* 100 (/ @current-cases total-cases))))
                                  (try (let [result (compare-for-p p-neighbour p-list)]
                                         (comparison-column result))
                                       (catch Exception e nil)))))))))))

  (spit "/home/josep/results.csv"
        (clojure.string/join
         "\n"
         (apply map
                (fn [& rest] (clojure.string/join
                              ","
                              (flatten rest)))
                (conj --cols-result --case-names))))



  END REPL)


(comment
  "1. the textual process description
   2. the process model
   (Of course also fine to provide these in separate files)

   Per activity of the process model:
   3. the gold standard correspondence (sentence + sentence ID)
   4. the correspondence generated by our approaches
   5. a flag when there is a difference between either your approach and the gold standard or between your approach and our approach

   Also, if possible:
   6. a matrix of similarity scores for all the activities to the sentences "


  (defn comparison->flag [comparison]
    (apply str
           (replace
            {1 "A", 2 "B", 3 "C"}
            (first
             (reduce
              (fn [[string-so-far seen max-code] next]
                (if (contains? seen next)
                  [(conj string-so-far (seen next)) seen max-code]
                  (let [code-of-next (inc max-code)]
                    [(conj string-so-far code-of-next)
                     (assoc seen next code-of-next)
                     code-of-next])))
              [[] {} 0]
              comparison)))))

  (defn similarities->matrix [similarities]
    (let [sorted-tasks (sort-by a/id (filter b/flow-element? (keys similarities)))
          sorted-sentences (sort-by (comp #(Integer/parseInt %) a/id) (filter t/sentence? (keys similarities)))]
      (as-> [(map a/label sorted-tasks)] $$
        (concat $$
                (for [sentence sorted-sentences]
                  (map #(get-in similarities [sentence %]) sorted-tasks)))
        (map conj $$ (concat [""] (map a/label sorted-sentences)))
        (transform (walker string?) #(subs % 0 (min 15 (count %))) $$))))

  (def --done ["Recourse"])

  (defn han-benchmark
    [benchmark-path groundtruths-path out-path]
    (let [model-files (filter #(not ((set --done) (strip-extension %)))
                              (sort (get-files-with-extension "bpmn" benchmark-path)))
          text-files (filter #(not ((set --done) (strip-extension %)))
                             (sort (get-files-with-extension "txt" benchmark-path)))
          groundtruth-files (filter #(not ((set --done) (strip-extension %)))
                                    (sort (get-files-with-extension "json" groundtruths-path)))

          __ (assert (every? #(apply = %) (apply map vector (map #(map strip-extension %) [model-files text-files groundtruth-files])))
                     "There must be the same set of text, bpmn and json files in the benchmark folders.")

          case-names   (map strip-extension model-files)
          outfiles (map #(io/file out-path (str % "-comparison.csv")) case-names)
          sim-outfiles (map #(io/file out-path (str % "-similarities.csv")) case-names)

          groundtruths (->> groundtruth-files (map slurp) (map groundtruth/load-json))

          results-upc  (for [[model-file text-file] (map vector model-files text-files)]
                         (with-config {:enable-anchors false, :use-vu-similarity false}
                           (core/main-with-files model-file text-file)))
          results-vu   (for [[model-file text-file] (map vector model-files text-files)]
                         (with-config {:enable-anchors false, :use-vu-similarity true}
                           (core/main-with-files model-file text-file)))

          matches-upc  (map #(-> % .getClojureExtraData :new-match-struct) results-upc)
          matches-vu   (map #(-> % .getClojureExtraData :new-match-struct) results-vu)

          sims-upc (map #(-> % .getClojureExtraData :similarities) results-upc)
          sims-vu  (map #(-> % .getClojureExtraData :similarities) results-vu)

          bpmns (map #(-> % .getClojureExtraData :model) results-upc)
          texts (map #(-> % .getClojureExtraData :text) results-vu)]

      ;; Generate CSVs of the form:
      ;; Task Id, Task Label, flag, Match GT id, Match GT label, Match UPC id, Match UPC label, Match VU id, Match VU label
      (doseq [[case-name out sim-out gt upc sim-upc vu sim-vu bpmn text] (map vector case-names outfiles sim-outfiles groundtruths matches-upc sims-upc matches-vu sims-vu bpmns texts)
              :let [tasks (:gt/tasks gt)]]
        (with-open [out-writer (io/writer out)
                    sim-out-writer (io/writer sim-out)]
          (csv/write-csv out-writer
                         (concat
                          [["Task ID" "Task Label", "Diff", "Groundtruth Sentence ID", "Groundtruth Sentence Label",
                            "UPC Sentence ID", "UPC Sentence Label", "VU Sentence ID", "VU Sentence Label"]]
                          (for [task tasks
                                :let [task-label (bpmn/task-name-of-id bpmn task)
                                      match-gt (get (:gt/alignment gt) task)
                                      match-gt (if (keyword? match-gt) -2 match-gt)
                                      match-gt-label (a/label (a/get-element text (str (inc match-gt))))
                                      match-upc (get (:gt/alignment upc) task)
                                      match-upc (if (keyword? match-upc) -2 match-upc)
                                      match-upc-label (a/label (a/get-element text (str (inc match-upc))))
                                      match-vu (get (:gt/alignment vu) task)
                                      match-vu (if (keyword? match-vu) -2 match-vu)
                                      match-vu-label (a/label (a/get-element text (str (inc match-vu))))
                                      flag (comparison->flag [match-gt match-upc match-vu])]]
                            [task task-label flag match-gt match-gt-label match-upc match-upc-label match-vu match-vu-label]))
                         :separator \;)
          (csv/write-csv sim-out-writer
                         (concat [["Similarities UPC:"]]
                                 (similarities->matrix sim-upc)
                                 [[""] ["Similarities VU:"]]
                                 (similarities->matrix sim-vu))
                         :separator \;)))))




  (with-config
    {:enable-cache true
     :use-gurobi true
     :return-trivial-alignment true
     :gurobi-tmp-path "BPMN/tmp"}
    (han-benchmark "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/auto-benchmark"
                   "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth"
                   "/home/josep/output-results")))

