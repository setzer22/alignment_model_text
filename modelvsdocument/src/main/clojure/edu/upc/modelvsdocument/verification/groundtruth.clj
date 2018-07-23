(ns edu.upc.modelvsdocument.verification.groundtruth
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clojure.pprint :as pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.inspector :as inspector]
            [com.rpl.specter :as specter :refer [transform select ALL FIRST]]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.verification.robust-groundtruth :refer [groundtruth-patches]]))


(spec/def ::int-or-str #(or (int? %) (string? %)))
(spec/def ::sentences (spec/map-of int? string?))
(spec/def ::tasks (spec/map-of ::int-or-str string?))
(spec/def ::error-code (spec/or :missing #(= "m" %) :wrong-order #(= "w" %) :ambiguous #(= "a" %)))
(spec/def ::task->sentence (spec/map-of ::int-or-str (spec/or :missing ::error-code
                                                      :sentence-id int?)))
(spec/def ::groundtruth (spec/keys :req [::sentences ::tasks ::task->sentence]))

(spec-fn parse-groundtruth string? -> ::groundtruth)
(defn parse-groundtruth [csv-string]
  (let [csv                  (csv/read-csv csv-string :separator \;)
        [_ & relevant-lines] (take-while #(not (every? empty? %)) csv)
        parse-first-int      (fn [list-of-pairs] (transform [ALL FIRST] #(Integer/parseInt %) list-of-pairs))
        empty-str?           #(= "" %)
        remove-empty-pairs   (fn [list-of-pairs] (remove #(every? empty-str? %) list-of-pairs))
        tasks                (-> (take-while #(-> % first empty? not)
                                             (map #(keep % [0 1]) relevant-lines)),
                                 remove-empty-pairs, parse-first-int, mapify-ordered)
        sentence-id-pairs    (-> (take-while #(-> % first empty? not)
                                             (map #(keep % [5 6]) relevant-lines))
                                 remove-empty-pairs parse-first-int)
        original-sentences   (take (count sentence-id-pairs)
                                   (rest (drop-while #(not= % "Original text") (map #(nth % 0) csv))))
        sentences            (mapify-ordered
                              (map (fn [[id _] sentence] [id sentence])
                                   sentence-id-pairs
                                   original-sentences))
        unmatched-tasks      (parse-first-int
                              (remove #(-> % second empty?)
                                      (map vector
                                           (map #(nth % 0) relevant-lines)
                                           (map #(nth % 3) relevant-lines))))
        parse-int            (fn [x not-int-val]
                               (try (Integer/parseInt x)
                                    (catch NumberFormatException e not-int-val)))
        matching             (as-> relevant-lines $$
                               (mapify
                                (mapv vector
                                      (take (count tasks) (map #(Integer/parseInt (nth % 0)) $$))
                                      (take (count tasks) (map #(parse-int (nth % 2) :gt/no-match) $$)))))
        ]
    {::sentences      sentences
     ::tasks          tasks
     ::task->sentence matching
     ::task->error    (mapify unmatched-tasks)}))

(spec-fn mk-groundtruth ::sentences ::tasks ::task->sentence)
(defn mk-groundtruth [sentences-by-id tasks-by-id task->sentence]
  {::sentences sentences-by-id
   ::tasks tasks-by-id
   ::task->sentence task->sentence})

(spec-fn print-assignment ::groundtruth)
(defn print-assignment [assignment]
  (let [matching (::task->sentence assignment)]
    (doseq [[t s] matching
            :let [task (get (::tasks assignment) t)
                  sentence (get (::sentences assignment) s (str "[UNMATCHED, error=" s "]"))]]
      (println task " <-> " sentence))))

(spec-fn string-difference string? string?)
(defn string-difference [s1 s2]
  (let [f1 (frequencies (string/lower-case s1))
        f2 (frequencies (string/lower-case s2))]
    (+ (reduce + (for [c (keys f1)] (Math/abs (- (get f1 c) (get f2 c 0)))))
       (reduce + (for [c (keys f2)] (Math/abs (- (get f2 c) (get f1 c 0))))))))

(spec-fn best-candidate string? (spec/or :tasks ::tasks, :sentences ::sentences))
(defn best-candidate [s candidates]
  (first
    (apply min-key (fn [[candidate-id candidate-str]]
                     (string-difference s candidate-str))
           candidates)))

(spec-fn get-sentence-id string? ::groundtruth)
(defn get-sentence-id [s groundtruth]
  (best-candidate s (::sentences groundtruth)))

(def ^:dynamic current-model
  "This variable is set by the caller with the name of the
   model being evaluated. By using this, we can see if the
   model is one of the ones which needs patching because of
   repeated task names."
  nil)

;(spec-fn get-task-id string? ::groundtruth)
(defn get-task-id
  ([t task-id groundtruth] ;PATCH
   (if (and (contains? groundtruth-patches current-model)
            (contains? (:ids (groundtruth-patches current-model)) task-id))
     ((:ids (groundtruth-patches current-model)) task-id)
     (get-task-id t groundtruth)))
  ([t groundtruth]
   (best-candidate t (::tasks groundtruth))))

(spec-fn matching-translator ::groundtruth ::groundtruth)
(defn matching-translator [matching1 matching2]
  {::task->task
   (mapify (for [[t-id t-str] (::tasks matching1)]
             [t-id (get-task-id t-str t-id matching2)])) ;PATCH
   ::sentence->sentence
   (mapify (for [[s-id s-str] (::sentences matching1)]
             [s-id (get-sentence-id s-str matching2)]))})

(spec/def ::ok int?)
(spec/def ::not-ok int?)
(spec/def ::total int?)
(spec/def ::diff (spec/* (spec/spec (spec/cat
                                      :result (spec/spec (spec/cat :t1 string? :s1 string?))
                                      :groundtruth (spec/spec (spec/cat :t2 string? :s2 string?))))))
(spec/def ::summary (spec/keys :req [::ok ::not-ok ::total ::diff]))

(spec-fn compare-to-groundtruth ::groundtruth ::groundtruth -> ::summary)
(defn compare-to-groundtruth [result groundtruth]
  "Compares the given result to the groundtruth, returning the ratio of correct matchings"
  ;(assert (= (count (keys (::tasks result))) (count (keys (::tasks groundtruth)))))
  (assert (<= (count (keys (::tasks result))) (count (keys (::tasks groundtruth)))))
  (assert (= (count (keys (::sentences result))) (count (keys (::sentences groundtruth)))))
  (let [{:keys [::task->task ::sentence->sentence]} (matching-translator result groundtruth)
        correct-tasks (for [[t-id, _] (::tasks result)
                            :let [s-id1 (get (::task->sentence result) t-id)
                                  s-str1 (get (::sentences result) s-id1)
                                  s-id2 (get (::task->sentence groundtruth) (task->task t-id))]]
                        ; Either both tasks are unassigned or assigned to the same sentence
                        (or (and (nil? s-str1) (nil? (get (::sentences groundtruth) s-id2)))
                            (when (not (nil? s-str1)) (= (get-sentence-id s-str1 groundtruth) s-id2))))
        total (count (::tasks result))
        ok-count (count (filter identity correct-tasks))
        no-count (- total ok-count)

        ; Diff
        incorrect-indices (indices-such-that not correct-tasks)
        incorrect-tasks (map #(nth (seq (::tasks result)) %) incorrect-indices)
        incorrect-st-pairs (for [[id text :as task] incorrect-tasks
                                 :let [sentence-id (get (::task->sentence result) id)
                                       sentence-text (get (::sentences result) sentence-id)]]
                             [task [sentence-id sentence-text]])
        gt-task-ids (map (fn [[_ task-text]] (get-task-id task-text groundtruth)) incorrect-tasks)
        gt-st-pairs (for [task-id gt-task-ids
                          :let [task-text (get (::tasks groundtruth) task-id)
                                sentence-id (get (::task->sentence groundtruth) task-id)
                                sentence-text (get (::sentences groundtruth) sentence-id)]]
                      [[task-id task-text] [sentence-id sentence-text]])
        diff (mapv vector incorrect-st-pairs gt-st-pairs)]
    {::total total
     ::ok ok-count
     ::not-ok no-count
     ::diff diff}))

(spec-fn print-comparison ::summary -> string?)
(defn explain-comparison [{:keys [::ok ::not-ok ::diff ::total]}]
  (str "Summary:\n\n -> " ok "/" total " correct assignments\n -> "
       not-ok "/" total " wrong assignments\n -> "
       "performance: " (format "%.2f" (double (/ ok total))) "\n"
       "\nDifferences:\n\n"
       (apply str (for [diff-case diff]
           (str "  (*) result: " (-> diff-case first first second) " -> " (-> diff-case first second second) "\n"
                "      expected: " (-> diff-case second first second) " -> " (-> diff-case second second second) "\n\n")))))


(defn print-task-correlation [matching1 matching2]
  (doseq [[t-id t-str] (::tasks matching1)]
    (println t-str " -> " (get (::tasks matching2) (get-task-id t-str matching2)))))

(defn print-sentence-correlation [matching1 matching2]
  (doseq [[t-id t-str] (::sentences matching1)]
    (println t-str " -> " (get (::sentences matching2) (get-sentence-id t-str matching2)))))

(comment
  "Test code"
  (inspector/inspect-table (csv/read-csv (slurp (io/file "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/Model1-4.csv")) :separator \;))
  (print-assignment (parse-groundtruth (slurp "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/Model8-2.csv")))
  (compare-to-groundtruth edu.upc.modelvsdocument.core/-gt
                          (parse-groundtruth (slurp "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/bicyclemanufacturing.csv")))
  (dofolder [f "/home/josep/Repositories/inconsistenciesmodeltext/input/groundtruths/"]
            (when (= (extension f) "csv")
              (println "--------- " (.getName f) " ---------")
              (print-assignment (parse-groundtruth (slurp f))))))
