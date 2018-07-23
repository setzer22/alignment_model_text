(ns edu.upc.modelvsdocument.repl.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer :all]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.core :refer :all]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.modelvsdocument.wordnet :as wordnet]
            [edu.upc.modelvsdocument.verification.groundtruth :as groundtruth]))

(comment
         ; CACHE INITIALIZATION
         (cache/initialize (str (System/getProperty "user.home") "/.textserver-cache.clj"))

         ; READ WORDNET DICTIONARIES
         (if (and (.exists (clojure.java.io/as-file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/wn30.src"))
                  (.exists (clojure.java.io/as-file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/senses30.src")))
           (wordnet/read-wordnet-dictionaries "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/")
           (throw (Exception. "Wordnet dictionaries not found")))

)

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

(comment
  (print *e)

  (execute-algorithm "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/The_Big_Model/"
                     "big.bpmn"
                     "big.txt")

  (identity big-result)

  (spit "/home/josep/tmp/good-result.clj"
        (apply list (com.rpl.specter/transform
          (com.rpl.specter/walker int?)
          str
          (sort-by first core/choco-matches)))
        )

  (get-matrix core/sim-matrix 33 38)

  (print-log "Credit-scoring.bpmn" "Credit-scoring.txt")
  (print-log "Recourse.bpmn" "Recourse.txt")
  (print-log "Dispatch-of-goods.bpmn" "Dispatch-of-goods.txt")
  (print-log "Self-service-restaurant.bpmn" "Self-service-restaurant.txt")
  (print-log "Bicycle_Manufacturer.bpmn" "Bicycle_Manufacturer.txt")
  (print-log "Computer_Repair.bpmn" "Computer_Repair.txt")
  (print-log "Hotel.bpmn" "Hotel.txt")
  (print-log "Underwriter.bpmn" "Underwriter.txt")
  (print-log "Hospital.bpmn" "Hospital.txt")
  (print-log "Zoo.bpmn" "Zoo.txt")
  (time (print-log "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/hva/"
                   "model-6-1.bpmn" "model-6-1.txt")))

(comment
  "The H. van der Aa. benchmark"
  (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"
        benchmark-files (file-seq (io/file benchmark-path))
        text-files (sort-by #(.getName %) (filter #(= "txt" (extension %)) benchmark-files))
        bpmn-files (sort-by #(.getName %) (filter #(= "bpmn" (extension %)) benchmark-files))]
    (spit "/home/josep/execution-log" "")
    (map (fn [f1 f2]
            (try (do
                   (println (str (.getName f1) " vs " (.getName f2)))
                   (core/main-with-files f1 f2)
                   (spit "/home/josep/execution-log" (str (.getName f1) " vs " (.getName f2) " [OK]" "\n") :append true))
                 (catch Exception e
                   (sh "notify-send" "Error!")
                   (spit "/home/josep/execution-log" (str (.getName f1) " vs " (.getName f2) " [EXECUTION ERROR: " (.getMessage e) "]" "\n") :append true)))
            ; Let the data race begin
            (str  (.getName f1) (.getName f2)))
          bpmn-files 
          text-files))

  (print *e)
  (println 
    (groundtruth/explain-comparison
      (groundtruth/compare-to-groundtruth
        (:match-struct 
          (.getClojureExtraData 
            (execute-algorithm 
              "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"
              "HotelService.bpmn"
              "HotelService.txt")))
        (groundtruth/parse-groundtruth 
          (slurp "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/hotelservice.csv")))))


  (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"
        benchmark-files (file-seq (io/file benchmark-path))
        text-files (sort-by #(.getName %) (filter #(= "txt" (extension %)) benchmark-files))
        bpmn-files (sort-by #(.getName %) (filter #(= "bpmn" (extension %)) benchmark-files))]
    (mapify 
      (for [[model text] (zip bpmn-files text-files)]
        (do 
          (println (.getName model) " vs " (.getName text))
          [(strip-extension model) (:match-struct (.getClojureExtraData (core/main-with-files model text)))]))))

  (def result *1)
  (io/make-parents "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/ours.clj")
  (spit "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/ours.clj" result)

  (clojure.pprint/pprint benchmark->groundtruth)

  (println *e)
  (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"
        benchmark-files (file-seq (io/file benchmark-path))
        __ (spit "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/ours.csv" "")
        results-table (io/file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/ours.csv")
        text-files (sort-by #(.getName %) (filter #(= "txt" (extension %)) benchmark-files))
        bpmn-files (sort-by #(.getName %) (filter #(= "bpmn" (extension %)) benchmark-files))]
    (doseq [[model text] (zip bpmn-files text-files)
            :let [groundtruth (benchmark->groundtruth model)
                  __ (assert (not (nil? groundtruth)))]]
      (println (.getName model) " vs " (.getName text))
      (println 
        (let [start-time (System/currentTimeMillis)
              comparison (groundtruth/compare-to-groundtruth 
                           (:match-struct (.getClojureExtraData (core/main-with-files model text)))
                           (groundtruth/parse-groundtruth (slurp groundtruth)))
              end-time (System/currentTimeMillis)]
          (spit results-table 
                (str (strip-extension model) ", " 
                     (::groundtruth/ok comparison) "/" (::groundtruth/total comparison) ", " 
                     (- end-time start-time) "\n")
                :append true)))))
  (clojure.pprint/pprint benchmark->groundtruth)

  (clojure.pprint/pprint (get (read-string (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/ours.clj")) "Model9-6"))

  (defn name->groundtruth [case-name]
    (let [groundtruth-files (file-seq (io/file "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/"))
          files-with-names (remove #(nil? (first %)) (pair-with strip-extension groundtruth-files))]
      (second (apply min-key #(levenshtein (first %) case-name) files-with-names))))

  (print *e)
  (identity comparisons)
  (def comparisons
    (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"
          models-to-run ["Model6-3" ]
          bpmn-files (mapv #(io/file (str benchmark-path % ".bpmn")) models-to-run)
          text-files (mapv #(io/file (str benchmark-path % ".txt")) models-to-run)
          results (mapv core/main-with-files bpmn-files text-files)
          groundtruths (mapv #(-> % name->groundtruth slurp groundtruth/parse-groundtruth) models-to-run)
          match-structs (mapv #(-> % .getClojureExtraData :match-struct) results)
          ]
      (mapv groundtruth/compare-to-groundtruth match-structs groundtruths)))

  (comment
    "TF-IDF integration test"
    (println (.getLog (let [text "Dog dog dog cat.\nCat dog Cat cat.\nParrot dog cat."
                    model-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/Bicycle_Manufacturer.bpmn"]
                (main model-path text))))

    (pprint text-features)

    (pprint model-features)

    (* 1.5 (* 1/3 (Math/log 3)))

    )

  (use '(incanter core charts))


  (let [models  [{:case-name "model_002", :time 771, :error nil}
                 {:case-name "model_005", :time 756, :error nil}
                 {:case-name "model_006", :time 1182, :error nil}
                 {:case-name "model_007", :time 833, :error nil}
                 {:case-name "model_008", :time 589, :error nil}
                 {:case-name "model_009", :time 1088, :error nil}
                 {:case-name "model_010", :time 1166, :error nil}
                 {:case-name "model_011", :time 836, :error nil}
                 {:case-name "model_012", :time 15860, :error nil}
                 {:case-name "model_013", :time 10911, :error nil}
                 {:case-name "model_014", :time 19037, :error nil}
                 {:case-name "model_015", :time 16480, :error nil}
                 {:case-name "model_016", :time 13825, :error nil}
                 {:case-name "model_017", :time 14617, :error nil}
                 {:case-name "model_018", :time 15467, :error nil}
                 {:case-name "model_019", :time 24231, :error nil}
                 {:case-name "model_020", :time 20022, :error nil}
                 {:case-name "model_021", :time 28619, :error nil}
                 {:case-name "model_022", :time 35065, :error nil}
                 {:case-name "model_023", :time 29277, :error nil}
                 {:case-name "model_024", :time 19843, :error nil}
                 {:case-name "model_025", :time 39958, :error nil}
                 {:case-name "model_026", :time 37809, :error nil}
                 {:case-name "model_027", :time 46946, :error nil}
                 {:case-name "model_028", :time 25292, :error nil}
                 {:case-name "model_029", :time 26740, :error nil}
                 {:case-name "model_030", :time 52162, :error nil}
                 {:case-name "model_031", :time 88363, :error nil}
                 {:case-name "model_032", :time 47947, :error nil}
                 {:case-name "model_033", :time 49642, :error nil}
                 {:case-name "model_034", :time 34602, :error nil}
                 {:case-name "model_035", :time 48050, :error nil}
                 {:case-name "model_036", :time 47309, :error nil}
                 {:case-name "model_037", :time 80269, :error nil}
                 {:case-name "model_038", :time 46690, :error nil}
                 {:case-name "model_039", :time 68802, :error nil}
                 {:case-name "model_040", :time 57182, :error nil}
                 {:case-name "model_041", :time 145581, :error nil}
                 {:case-name "model_042", :time 150639, :error nil}
                 {:case-name "model_044", :time 82734, :error nil}
                 {:case-name "model_045", :time 145922, :error nil}
                 {:case-name "model_046", :time 175549, :error nil}
                 {:case-name "model_048", :time 115941, :error nil}
                 {:case-name "model_050", :time 109133, :error nil}
                 {:case-name "model_052", :time 86227, :error nil}
                 {:case-name "model_054", :time 96932, :error nil}
                 {:case-name "model_056", :time 196175, :error nil}
                 {:case-name "model_059", :time 157449, :error nil}
                 {:case-name "model_061", :time 155918, :error nil}
                 {:case-name "model_064", :time 214617, :error nil}
                 {:case-name "model_065", :time 151886, :error nil}
                 {:case-name "model_066", :time 219313, :error nil}
                 {:case-name "model_067", :time 166599, :error nil}
                 {:case-name "model_068", :time 150979, :error nil}
                 {:case-name "model_070", :time 229713, :error nil}
                 {:case-name "model_071", :time 317970, :error nil}
                 {:case-name "model_074", :time 197618, :error nil}
                 {:case-name "model_075", :time 255224, :error nil}
                 {:case-name "model_076", :time 198535, :error nil}
                 {:case-name "model_077", :time 204463, :error nil}
                 {:case-name "model_079", :time 643558, :error nil}
                 {:case-name "model_088", :time 352671, :error nil}
                 {:case-name "model_088", :time 352671, :error nil}
                 {:case-name "model_088", :time 352671, :error nil}
                 {:case-name "model_088", :time 352671, :error nil}
                 {:case-name "model_095", :time 296482, :error nil}
                 {:case-name "model_096", :time 813278, :error nil}
                 {:case-name "model_097", :time 481735, :error nil}
                 {:case-name "model_101", :time 265196, :error nil}
                 {:case-name "model_107", :time 1032076, :error nil}]
        sizes (map #(Integer/parseInt (second (re-matches #"model_(\d+)" (:case-name %)))) models)
        times (map #(/ (:time %) 1000) models)]
    (def plot (scatter-plot sizes times
                            :x-label "Input Size"
                            :y-label "Time (s)")))

  (def their-time-benchmark-data
    (read-string (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/Comparison/inconsistenciesmodeltext-output.clj")))


  (let [sizes (map #(Integer/parseInt (second (re-matches #"model_(\d+)" (:case-name %))))
                   their-time-benchmark-data)
        times (map #(/ (:total-time %) 1000) their-time-benchmark-data)]
    (add-points plot sizes times))

  (set-axis plot :y (log-axis :label "Log Time (s)" :base 10))

  (view plot)

  (groundtruth/compare-to-groundtruth
    (:match-struct (.getClojureExtraData
        (time (let [benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/big-benchmark/"]
          (core/main-with-files (io/file (str benchmark-path "Model9-6.bpmn"))
                                (io/file (str benchmark-path "Model9-6.txt")))))))
    (groundtruth/parse-groundtruth (slurp "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/Model9-6.csv"))))
