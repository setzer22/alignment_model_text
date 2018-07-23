(ns edu.upc.modelvsdocument.core-students
  (:require [com.rpl.specter :as specter :refer :all]
            [edu.upc.modelvsdocument.utils :as utils :refer :all]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.config :as config]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.bpmn-alignable :as bpmn-al]
            [edu.upc.modelvsdocument.text :as text]
            [edu.upc.modelvsdocument.similarity :as similarity]
            [edu.upc.modelvsdocument.predictors :as pred]
            [edu.upc.modelvsdocument.back-edges :as back-edges]
            [edu.upc.modelvsdocument.sorter.model-sorter :as model-sorter]
            [clojure.pprint :refer [pprint]]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.annotation-mockup :as ann]
            [edu.upc.modelvsdocument.atds :as atds]
            [clj-pipeline.core :as pipeline :refer [defpipe-input defpipe-output defpipe-step
                                                    run-pipeline env]]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]))

#_"In this file we implement a slightly modified core pipeline to perform the
   annotated text vs model pipeline for student evaluation."

(declare config) ;; Defined later...

#_"We will actually need to interject into the pipeline at three steps:
  - During text analysis, the pre-analyzed text is loaded instead.
  - At text ordering, the new order matrix must be used.
  - At the solver step, changing the actual optimization problem. "

(defpipe-step get-case-info [case-name]
  (if (atds/annotation-exists? case-name)
    (let [{:keys [text roles text-an optional-task-ids order-matrix]} (atds/get-case-info case-name)]
      {:text-res (text/mk-text text text-an)
       :order-matrix order-matrix
       :optional-task-ids optional-task-ids
       :roles roles})
    #_ "else"
    {:text-res @(ann/text-an case-name)
     :order-matrix (ann/order-matrix case-name)
     :optional-task-ids (ann/optional-task-ids case-name)
     :roles []}))

(defpipe-step analyze-students-text [text-res]
  (let [;;text-res @(ann/text-an case-name) ;; NOTE: Path validation done here
        text-alignables (a/get-all-alignables text-res)
        S (count text-alignables)]
    (env text-alignables, S)))

(defpipe-step compute-students-text-order [order-matrix text-alignables case-name]
  (let [[dummy & text-alignables] (sort-by #(Integer/parseInt (a/id %)) text-alignables)
        text-alignables (vec text-alignables)
        ;;order-matrix (ann/order-matrix case-name)
        sentence-order {}
        sentence-order (reduce
                        (fn [sentence-order [ai aj rel]]
                          (assoc-in sentence-order [ai aj] rel))
                        sentence-order
                        (for [i (range (count text-alignables))
                              j (range (count text-alignables))
                              :let [ai (text-alignables i)
                                    aj (text-alignables j)]]
                          [ai aj (get-in order-matrix [i j])]))
        ;; NOTE: We assume anchor points are active, thus the dummy sentence
        sentence-order (reduce
                        (fn [sentence-order alignable]
                          (-> sentence-order
                              (assoc-in [alignable dummy] :||)
                              (assoc-in [dummy alignable] :||)))
                        sentence-order
                        text-alignables)]
    (env sentence-order)))

#_"We also need a way to mark text sentences as optional, so they don't count for missing tasks"
#_(defpipe-step optional-tasks [case-name]
    {:optional-task-ids (ann/optional-task-ids case-name)})

#_"Additionally, we will need to adapt to the new output. We need to return a list of errors, namely:
   1. Are gateways being reused? [WARNING]
   2. Are there any implicit gateways? [ERROR]
   3. Are there any tasks missing? [ERROR]
   4. Are there any superfluous tasks? [ERROR]
   5. Are there any issues with the order? [WARNING]

   We won't treat order issues as an error since we can't detect it with reasonable accuracy yet. "

#_"1. Gateway reuse means a gateway has >=2 incoming and >=2 outgoing arcs."

(defn is-gateway-reused? [gateway]
  (and (>= (count (.getIncomingFlows gateway)) 2)
       (>= (count (.getOutgoingFlows gateway)) 2)))

(defpipe-step detect-gateway-reuse [model]
  {:reused-gateway-ids (->> (bpmn/all-gateways model)
                            (filter is-gateway-reused?)
                            (mapv #(.getId %)))})

#_"2. Implicit gateway means a task has >=2 incoming or >= outgoing arcs."

(defn is-implicit-gateway? [gateway]
  (or (>= (count (.getIncomingFlows gateway)) 2)
      (>= (count (.getOutgoingFlows gateway)) 2)))

(defpipe-step detect-implicit-gateway [model]
  {:implicit-gateway-ids (->> (bpmn/all-tasks model)
                              (filter is-implicit-gateway?)
                              (mapv #(.getId %)))})

#_"3. We know that a task is missing because, in the trivial alignment, there is a non-optional
      sentence s and no task t such that s~t."
;;TODO: Optional activities
(defpipe-step detect-missing-tasks [trivial-alignment text-alignables optional-task-ids]
  {:missing-tasks (as-> trivial-alignment $$
                    (map first $$)
                    (set $$)
                    (set/difference (set (remove text/dummy? text-alignables)) $$)
                    (remove (fn [sentence] (optional-task-ids (:id sentence))) $$)
                    (sort-by #(Integer/parseInt (a/id %)) $$))})

#_"4. We know that a task is superfluous if the predictor for that task identifies it as missing.
      Since that means there is no good sentence match for that task."
(defpipe-step detect-superfluous-tasks [trivial-alignment similarities]
  (let [predictions (pred/compute-predictor :students-missing similarities trivial-alignment)]
    {:superfluous-tasks (->> predictions
                             (filter #(bpmn/task? (first %)))
                             (filter #(< (second %) (config :missing-threshold)))
                             (map first))}))

#_"5. We know there are issues with the order if the max constrained predictor is below a given
      threshold."
#_"NOTE: This was unreliable so it is temporarily disabled"
(defpipe-step detect-order-issues [max-constrained]
  {:order-issues false #_(> max-constrained (config :order-threshold))})

#_"Finally, we define the input and output of the new pipeline. Also a config structure to control
   output-related things. This structure is not under the config module because we want to reload it
   at runtime"

(def ^:dynamic config
  {:verbose true
   :missing-threshold 0.15
   :order-threshold 0.50
   :show-quantities true})

(defpipe-step detect-roles [model roles case-name]
  #_"TODO: Improve role detection. But how?"
  (let [labels (:analyzed-labels model)
        words-of-lanes
        (as-> [] $$
          (apply conj $$ (bpmn/all-pool-ids model))
          (apply conj $$ (bpmn/all-lane-ids model))
          (mapify (pair-with-r #(select [:tokens ALL :lemma] (labels %)) $$)))

        jaccard (fn [s1 s2]
                  (if (empty? (set/intersection s1 s2))
                    0
                    (/ (count (set/intersection s1 s2))
                       (count (set/union s1 s2)))))

        role-labels
        (map #(str/join " " %) roles)

        default-answer
        (mapify
         (map vector
              role-labels
              (repeat nil)))

        roles-found
        (when (seq roles)
          (merge
           default-answer
           (mapify
            (map
             (fn [[id lane-words]]
               (let [max-sim (apply max (map #(jaccard (set lane-words) %) roles))]
                 (if (>= max-sim 0.5)
                   [(str/join " " (apply max-key #(jaccard (set lane-words) %) roles)) id])))
             words-of-lanes))))
        ]
    {:roles-found (or roles-found default-answer)}))

#_(detect-roles
   {:model (bpmn-al/analyze "/home/josep/ModelsBpmn/Zoo.bpmn" "")
    :case-name "Zoo"
    :roles edu.upc.atdlib.atd2roles/--roles})


(defpipe-step detect-double-actions [model]
  (let [labels (:analyzed-labels model)
        double-action-ids
        (->> labels
             (filter #(= (-> % second :constituents first :label) "double-action"))
             (map first))
        double-action-labels
        (map #(a/label (a/get-element model %)) double-action-ids)]
    (env double-action-labels)))

(defpipe-output collect-errors [similarities, trivial-alignment, reused-gateway-ids, implicit-gateway-ids,
                                missing-tasks, superfluous-tasks order-issues, model-is-connected, case-name,
                                start-or-end-event-missing? model-contains-non-natural-loops roles-found
                                double-action-labels]
  (let [OK 0, WARNING 1, ERROR 2
        err-msg (fn err-msg [condition, err-code, mode, ok-msg, err-msg]+
                  (if condition
                    {"error" ok-msg
                     "code" OK
                     "mode" mode}
                    {"error" err-msg
                     "code" err-code
                     "mode" mode}))
        not-empty? #(not (seq %))

        missing-roles (keys-such-that nil? roles-found)

        ;; Static Messages
        errors [;; Implicit Gateways
                (err-msg (not-empty? implicit-gateway-ids) ERROR "validate"
                         "There are no implicit gateways in your model"
                         "There exist implicit gateways in your model.")
                ;; Gateway Reuse
                (err-msg (not-empty? reused-gateway-ids) ERROR "validate"
                         "You are not reusing any gateways in your model."
                         "You are reusing gateways in your model.")
                ;; Missing Tasks
                (if (config :show-quantities)
                  (err-msg (not-empty? missing-tasks) ERROR "validate"
                           "There are no tasks missing from your model"
                           (format "You have %d tasks which are still missing"
                                   (count missing-tasks)))
                  (err-msg (not-empty? missing-tasks) ERROR "validate"
                           "There are no tasks missing from your model"
                           "You have some tasks which are still missing"))
                ;; Unnecesary Tasks
                (if (config :show-quantities)
                  (err-msg (not-empty? superfluous-tasks) ERROR "validate"
                           "All tasks in your model are relevant."
                           (format "You have %d unnecesary tasks in your model"
                                   (count superfluous-tasks)))
                  (err-msg (not-empty? superfluous-tasks) ERROR "validate"
                           "All tasks in your model are relevant."
                           "You have some unnecesary tasks in your model"))]]
    ;; Dynamic Messages
    (as-> errors $$

      ;; Roles 
      (conj $$ (if (= (count missing-roles) 0)
                 {"error" "Your model contains all the relevant actor(s) of the process"
                  "code" OK
                  "mode" "validate"}
                 (if (config :show-quantities)
                   {"error" (format "Your model is missing %d actors of the process."
                                    (count missing-roles))
                    "code" ERROR
                    "mode" "validate"}
                   {"error" "Your model is missing some of the relevant actors of the process."
                    "code" ERROR
                    "mode" "validate"})))

      ;; Natural Loops
      (if model-contains-non-natural-loops
        (conj $$ {"error" "Your model contains some non-natural loops."
                  "code" WARNING
                  "mode" "validate"})
        $$)

      ;; Missing start/end event
      (if start-or-end-event-missing?
        (conj $$ {"error" "Your model is missing a start or end event. Control flow checks are disabled"
                  "code" WARNING
                  "mode" "validate"})
        $$)

      ;; Unconnected graph
      (if (not model-is-connected)
        (conj $$ {"error" "Your model is not connected. Control flow checks are disabled."
                  "code" WARNING
                  "mode" "validate"})
        $$)

      ;; Contains double action
      (if (pos? (count double-action-labels))
        (conj $$ {"error" "Some tasks in your model represent more than one action."
                  "code" WARNING
                  "mode" "validate"})
        $$)

      ;; Order Issues
      (if order-issues
        (conj $$ {"error" "There are issues with the control flow of your model."
                  "code" WARNING
                  "mode" "validate"})
        $$)

      ;; TODO: Since we put the extra feedback in the validation, we don't need (config :verbose) anymore.

      ;; Missing Tasks (Only for Done)
      (if (config :verbose)
        (reduce
         (fn [messages sentence]
           (conj messages {"error" (format "You are missing a task about \"%s\" in your model" (a/label sentence))
                           "code" ERROR
                           "mode" "done"}))
         $$
         missing-tasks)
        $$)

      ;; Unnecesary Tasks (Only for Done)
      (if (config :verbose)
        (reduce
         (fn [messages sentence]
           (conj messages {"error" (format "Your task named \"%s\" is not necessary" (a/label sentence))
                           "code" WARNING
                           "mode" "done"}))
         $$
         superfluous-tasks)
        $$)

      ;; Missing Roles (Only for Done)
      (if (config :verbose)
        (reduce
         (fn [messages role]
           (conj messages {"error" (format "Your are missing the \"%s\" role" role)
                           "code" ERROR
                           "mode" "done"}))
         $$
         missing-roles)
        $$)

      ;; Double Actions (Only for Done)
      (if (config :verbose)
        (reduce
         (fn [messages role]
           (conj messages {"error" (format "Your task \"%s\" contains more than one action" role)
                           "code" WARNING
                           "mode" "done"}))
         $$
         double-action-labels)
        $$)
      )))




(defpipe-step debug-defs
  [similarities
   max-constrained
   text-features
   model-features
   alignment
   trivial-alignment
   task-order
   start-or-end-event-missing?
   model
   model-alignables
   roles-found]
  (def --model model)
  (def --similarities similarities)
  (def --max-constrained max-constrained)
  (def --text-features text-features)
  (def --model-features model-features)
  (def --alignment alignment)
  (def --trivial-alignment trivial-alignment)
  (def --task-order task-order)
  (def --roles-found roles-found)
  (def --start-or-end-event-missing? start-or-end-event-missing?)
  (def --model-alignables model-alignables)
  {})

(defpipe-input student-input [model-path case-name]
  (env model-path case-name))

#_"There is one remaining addition: Students usually will try to validate processes which may not be well-formed,
   connected or lack start-end events. We should check all this information and avoid some high level checks otherwise."

(defpipe-step analyze-model [model-path]
  (let [model (bpmn/read-model model-path)
        type-found? (fn [class]
                      (->> (.getProcesses model)
                           (map (fn [process]
                                  (filter #(instance? class %) (.getFlowElements process))))
                           (every? seq)))
        start-event-missing? (not (type-found? org.activiti.bpmn.model.StartEvent))
        end-event-missing? (not (type-found? org.activiti.bpmn.model.EndEvent))
        model-an (config/with-config {:model-read-strict false #_(not (or start-event-missing? end-event-missing?))}
                   (bpmn-al/analyze model ""))
        alignables (a/get-all-alignables model-an)]
    {:model model-an
     :model-alignables alignables
     :start-or-end-event-missing? (or start-event-missing? end-event-missing?)
     :T (count alignables)}))

(defn connected-graph? [G]
  (let [visited? (atom #{})
        connected? (fn connected? [v]
                     (do (swap! visited? conj v)
                         (doseq [n (G v)
                                 :when (not (@visited? n))]
                           (connected? n))))
        __ (connected? (first (keys G)))]
    (= @visited? (set (keys G)))))

(defmacro dbg [x]
  `(do (println ~x)
       ~x))

(defpipe-step check-connected [model]
  {:model-is-connected
   (every?
    connected-graph?
    (map #(let [G (:graph %)]
            (merge-with set/union G (bpmn/reverse-graph G))) (:processes model)))})

(defn acyclic-graph? [G]
  (->> G
       model-sorter/topological-sort
       (apply concat)
       set
       (= (set (keys G)))))

(defpipe-step check-natural-loops [model]
  {:model-contains-non-natural-loops
   (not (every? (fn [process]
                  (let [G (transform [MAP-VALS] set (:graph process))]
                    (acyclic-graph?
                     (back-edges/remove-edges
                      G
                      (back-edges/back-edges G (:start-event process))))))
                (:processes model)))})

(defn if-pipe [cond-key fn-if fn-else]
  #(do (if (get % cond-key)
         (fn-if %)
         (fn-else %))))

(defn when-pipe [cond-key fn-if]
  #(do (if (get % cond-key)
         (fn-if %)
         %)))

(defn when-not-pipe [cond-key fn-if]
  #(if (get % cond-key)
     %
     (fn-if %)))

(defpipe-step nop-step []
  {})

(defn student-pipeline []
  [student-input

   get-case-info

   #_optional-tasks
   analyze-students-text
   analyze-model
   check-connected
   core/extract-features
   core/compute-similarities
   compute-students-text-order
   (when-pipe :model-is-connected
     core/remove-back-edges)
   (when-not-pipe :start-or-end-event-missing?
     (when-pipe :model-is-connected
       check-natural-loops))
   (when-not-pipe :start-or-end-event-missing?
     core/compute-model-order)
   (when-not-pipe :start-or-end-event-missing?
     core/compute-alignment)
   core/compute-trivial-alignment
   (when-not-pipe :start-or-end-event-missing?
     core/compute-max-constrained)

   detect-gateway-reuse
   detect-implicit-gateway
   detect-missing-tasks
   detect-superfluous-tasks
   (when-not-pipe :start-or-end-event-missing?
     detect-order-issues)
   detect-double-actions
   detect-roles

   debug-defs

   collect-errors])

(def --hospital-weights
  {:has-lemma (* 2 0.11651)
   :has-parent-synset (* 0.05 0.19204)
   :has-synset (* 0.25 0.42809)

   :in-agent (* 0 0.0783)
   :agent-head (* 0 0.125)

   :in-patient (* 10 0.0783)
   :patient-head (* 10 0.125)

   :has-form (* 0.5 0.48355)
   :lemma-conditional-pred (* 2 0.55536)
   :has-action (* 11 0.125)
   :lemma-conditional-follow (* 2 0.82183)})

(defn evaluate-students [model-path case-name]
  (config/with-config {:feature-weight-overrides --hospital-weights
                       :remove-back-edges? true
                       :tf-idf-strict false}
    (apply run-pipeline
           [model-path case-name]
           (student-pipeline))))


(comment
  BEGIN

  
  (transform
   (multi-path
    [MAP-KEYS]
    [MAP-VALS ALL FIRST])
   a/label
   --similarities)

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/Emergency_Hospital.bpmn" "CourtHearing"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/ModelsBpmn/Model3-2.bpmn" "CourtHearing"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/ModelsBpmn/Hotel.bpmn" "Hotel"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/DSS_BENCHMARK/Models/1561879068_rev3.bpmn" "CreditRating"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/DSS_BENCHMARK/Models/1511399591_rev3.bpmn" "WebFrontend"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/ModelsBpmn/Dispatch-of-goods.bpmn" "Dispatch_of_Goods"))



  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/ModelsBpmn/Credit-scoring.bpmn" "Credit_scoring"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/Hospital_My.bpmn" "Hospital"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/ModelsBpmn/Zoo.bpmn" "Zoo"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/test/bug_josep.bpmn" "Zoo"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/test/bug_lluis.bpmn" "Hospital"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/test/bug_lluis2.bpmn" "Hospital"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/test/feedback_lluis_model_1.bpmn" "Zoo"))

  (binding [config (assoc config
                          :verbose true)]
    (evaluate-students "/home/josep/test/feedback_lluis_model_2.bpmn" "Zoo"))

  --model-features



  (->> --task-order
       (transform [ALL FIRST] a/label)
       (transform [ALL LAST ALL FIRST] a/label))

  --similarities

  --alignment

  (select [MAP-VALS] (filter #(= "Task_18in3wi" (try-or (.getId (first %)) nil)) --model-features))

  (select [MAP-VALS MAP-VALS] (filter #(= "Task_18in3wi" (try-or (.getId (first %)) nil)) --similarities))

  (select [MAP-VALS MAP-VALS] (filter #(= "Task_1kh5snw" (try-or (.getId (first %)) nil)) --similarities))

  --similarities

  (filter #(= "Task11" (try-or (.getId (first %)) nil)) --model-features)

  --similarities

  --text-features

  --model-features


  --max-constrained

  (spit "/tmp/trivial-alignment.txt"
        (with-out-str
          (pprint (->> --trivial-alignment (transform [ALL ALL] a/label)))))

  (for [[[s t] [s' t']] (zip --alignment --trivial-alignment)
        :when (bpmn/task? t)]
    (do (assert (= t t'))
        (print (a/label t))
        (print (get-in --similarities [s t]))
        (print " vs ")
        (println (get-in --similarities [s' t']))))

  (spit "/tmp/alignment.txt"
        (with-out-str
          (pprint (->> --alignment (transform [ALL ALL] a/label)))))

  END)
