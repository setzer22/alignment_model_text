(ns edu.upc.modelvsdocument.threshold-experiments
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.java.shell :refer [sh]]
            [clojure.core.matrix :as m]
            [clojure.set :as set]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.matchings :as matchings]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.threshold :refer :all]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [com.rpl.specter :refer [transform select ALL] :as specter]))

;(defn run-experiment [function]
  ;(future (let [result (function)]
            ;(sh "notify-send" "-i" 
                ;"/usr/share/icons/evolvere/scalable/status/dialog-information-symbolic.svg" 
                ;"-u" "critical" "Experiment execution complete")
            ;result)))
;
;(defn get-similarity-matrices []
  ;(let [benchmark-folder (io/file "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/")
        ;models (sort (filter #(= "bpmn" (extension %)) (.listFiles benchmark-folder)))
        ;texts (sort (filter #(= "txt" (extension %)) (.listFiles benchmark-folder)))
        ;results (doall (into {} (map (fn [m t] [(strip-extension m) (.getClojureExtraData (core/main-with-files m t))]) models texts)))]
    ;results))
;
;(defn experiment-single-round [t1 t2 similarity-matrices]
  ;"Optimise the thresholds"
  ;(for [[case-name case-data] similarity-matrices
        ;:when (::matchings/model case-data)
        ;:let [__ case-name
              ;sorted-tasks (bpmn/all-task-ids (::matchings/model case-data))
              ;task-index (fn [task-id] (.indexOf sorted-tasks task-id))
              ;unmatched-sentences* ,,, 
              ;(map #(-> % first dec)  We subtract 1 because sentences are 1-based but columns 0-based
                   ;(filter (fn [[_ matches]] (= matches ::matchings/no-task)) 
                           ;(::matchings/sentence->task case-data)))
              ;unmatched-tasks* ,,, 
              ;(map #(-> % first task-index) 
                   ;(filter (fn [[_ matches]] (= matches ::matchings/no-sentence))
                           ;(::matchings/task->sentence case-data)))
;
              ;{unmatched-sentences :row-indices, unmatched-tasks :column-indices} ,,,
              ;(unmatched-elements-to-filter (:similarity-matrix case-data) t1 t2)
;
              ;]]
    ;{:case-name case-name
     ;:unmatched-sentences* (set unmatched-sentences*)
     ;:unmatched-tasks* (set unmatched-tasks*)
     ;:unmatched-sentences (set unmatched-sentences) 
     ;:unmatched-tasks (set unmatched-tasks)}))
;
;(comment 
  ;(defn jaccard [s1 s2]
    ;(if (every? empty? [s1 s2]) 
      ;1
      ;(/ (count (set/intersection s1 s2))
         ;(count (set/union s1 s2)))))
;
  ;(defn similarity-of-values [{:keys [unmatched-sentences* unmatched-sentences 
                                      ;unmatched-tasks* unmatched-tasks]}]
    ;(+ (jaccard unmatched-sentences* unmatched-sentences)
       ;(jaccard unmatched-tasks* unmatched-tasks)))
;
  ;(defn score-of-round [results]
    ;(double (reduce + (map similarity-of-values results))))
;
  ;(defn experiment-step [t1 t2]
    ;(score-of-round (experiment-single-round t1 t2)))
;
  ;(defn all-threshold-pairs []
    ;(for [t1 (range 0.1 11 0.1), t2 (range 0.1 11 0.1)] [t1 t2]))
;
   ;Optimal values found were 7.2 and 11.0
  ;(defn optimize-thresholds []
    ;(let [t-pairs (all-threshold-pairs)
          ;partitions (partition-all 8 t-pairs)]
      ;(reduce (partial max-key first) 
              ;(pmap (fn [t-pairs] (reduce (partial max-key first)
                                          ;(pair-with #(apply experiment-step %) t-pairs)))
                    ;partitions)))))
;
;(comment 
  ;"Get the matrices"
  ;(def similarity-matrices-future (run-experiment get-similarity-matrices))
  ;(def similarity-matrices (merge-with merge 
                                       ;@similarity-matrices-future
                                       ;matchings/tagged-cases))
;
  ;(spit "/home/josep/matrices-log.txt" 
        ;(time (print (apply 
          ;str 
          ;(for [[case-name case-data] similarity-matrices
                ;:when (::matchings/model case-data)
                ;:let [__ case-name 
                      ;sorted-tasks (bpmn/all-task-ids (::matchings/model case-data))
                      ;task-index (fn [task-id] (.indexOf sorted-tasks task-id))
                      ;unmatched-sentences* ,,, 
                      ;(map #(-> % first dec)  We subtract 1 because sentences are 1-based but columns 0-based
                           ;(filter (fn [[_ matches]] (= matches ::matchings/no-task)) 
                                   ;(::matchings/sentence->task case-data)))
;
                      ;unmatched-tasks* ,,, 
                      ;(map #(-> % first task-index) 
                           ;(filter (fn [[_ matches]] (= matches ::matchings/no-sentence))
                                   ;(::matchings/task->sentence case-data)))
;
                      ;unmatched-rows* unmatched-sentences*
                      ;unmatched-columns* unmatched-tasks*
;
                      ;m (m/matrix (:similarity-matrix case-data))
;
                      ;{unmatched-sentences :row-indices, unmatched-tasks :column-indices}
                      ;(rows-and-cols-to-discard m 8.0 4.0)
                      ;
;
                      ;max-per-column (map-columns #(apply max %) m)
                      ;max-per-row (map-rows #(apply max %) m)
;
                      ;separate (fn [values, indices-to-separate] 
                                 ;(reduce-kv (fn [[A B], index, value]
                                              ;(if (in? indices-to-separate index)
                                                ;[(conj A value) B]
                                                ;[A (conj B value)]))
                                            ;[[], []]
                                            ;values))
;
                      ;separated-max-per-column (separate max-per-column unmatched-columns*)
                      ;separated-max-per-row (separate max-per-row unmatched-rows*)
;
                      ;debug-log (with-out-str 
                                  ;(core/section case-name
                                                ;(core/subsection "Matrix"
                                                                 ;(binding [*print-right-margin* 200] 
                                                                   ;(pprint 
                                                                     ;(->> case-data
                                                                          ;:similarity-matrix
                                                                          ;(transform (specter/walker number?) #(format "%.4f" %)))))
;
                                                                 ;)
                                                ;(core/subsection "Unmatched sentences"
                                                                 ;(println "Reference: " unmatched-sentences*)
                                                                 ;(println "Computed: " unmatched-sentences))
;
;
;
                                                ;(core/subsection "Unmatched tasks"
                                                                 ;(println "Reference: " unmatched-tasks*)
                                                                 ;(println "Computed: " unmatched-tasks))
;
                                                ;))]]
;
            ;debug-log))))))
;
;(defn replace-indices [vect indices replacement]
    ;(if-not (empty? indices) 
      ;(let [v (if (vector? vect) vect (into [] vect))]
        ;(apply assoc v
               ;(flatten (map (fn [idx] [idx replacement]) indices))))
      ;vect))
;
;(comment
  ;"Second pass log"
  ;(spit "/home/josep/entropies.txt" 
        ;(with-out-str 
          ;(doseq [[case-name case-data] similarity-matrices 
                  ;:let [m (:similarity-matrix case-data) 
                        ;filtered (unmatched-elements-to-filter-1 m 8.0)
                        ;entropy-per-col (replace-indices 
                                          ;(map-columns #(entropy (normalize %)) m)
                                          ;(filtered :column-indices)
                                          ;"DISCRD")
                        ;entropy-per-row (replace-indices 
                                          ;(map-rows #(entropy (normalize %)) m)
                                          ;(filtered :row-indices)
                                          ;"DISCRD")
                        ;table (conj (mapv #(into [] (concat [%2] %1)) m entropy-per-row) (into [] (concat ["------"] entropy-per-col)))
                        ;]]
            ;(binding [*print-right-margin* 200] 
              ;(println case-name)
              ;(doseq [row (format-numbers table)]
                ;(doseq [n row] (print n ""))
                ;(print "\n"))
              ;(print "\n\n")
              ;)))))
;
