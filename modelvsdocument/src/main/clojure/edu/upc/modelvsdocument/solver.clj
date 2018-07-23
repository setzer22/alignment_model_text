(ns edu.upc.modelvsdocument.solver
  (:require [clojure.spec :as spec]
            [com.rpl.specter :as specter :refer [select transform ALL walker]]
            [clojure.spec.test :as stest]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.java.io :as io]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.schemas :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.config :as config :refer [config]]
            [clojure.pprint :refer :all]
            [edu.upc.modelvsdocument.text :as text]
            [clojure.java.shell :as shell :refer [sh]])
  (:import [lpsolve LpSolve]))

(spec-fn matrix-float-to-int ::t/cost-matrix int? -> ::t/cost-matrix-int)
(defn matrix-float-to-int 
  "Matrix is a [0..1] matrix, returns a matrix of integer elements
   where the average value in matrix has a value of resolution"
  [matrix resolution]
  (let [avg (/ (reduce + (flatten matrix)) 
               (* (count matrix) 
                  (count (get matrix 0))))
        coef (* resolution (/ 1 avg))]
    (transform [ALL ALL] #(int (* coef %)) matrix)))

#_(spec-fn solve-with-lpsolve ::t/cost-matrix ::t/order-matrix ::t/order-matrix)
#_(defn solve-with-lpsolve [sim-matrix sentence-order task-order]
    (try
      (let [sim-matrix (matrix-float-to-int sim-matrix 1000)
            T          (count task-order)     ; number of tasks
            S          (count sentence-order) ; number of sentences
            ST         (* S T)

            all-variables (for [s (range S), t (range T)] [s t])
            assigned      (fn [s t] (str "a_"s "_"t))

            get-task-order     (fn [t1 t2]
                                 (get-matrix task-order t1 t2))
            get-sentence-order (fn [s1 s2]
                                 (get-matrix sentence-order s1 s2))

            ;; Initial solver with T variables.
            solver (LpSolve/makeLp 0 (* S T))

            ;; An improvised local-API to make constraints:
            ;; The Left-Hand-Side (LHS) of a constraints is a sum of terms, represented as an array
            ;; The Right-Hand-Side (RHS) is a constant value
            ;; A constraint is always of the form LHS cmp RHS with comp in {<=, >=, =, !=}
            assigned       (fn [s t] [s t])
            make-lhs       (fn [] (vec (repeat ST 0)))
            add-term       (fn [lhs s t coef] (assoc lhs (+ (* s T) t) coef))
            +vars-coefs    (fn [variables coefficients]
                             (let [row (make-lhs)]
                               (reduce (fn [row [[s t] coef]] (add-term row s t coef))
                                       row
                                       (zip variables coefficients))))
            +vars          (fn [& variables] (+vars-coefs variables (repeat 1)))
            lhs->str       (fn [constr] (clojure.string/join " " constr))
            cmp->lpsolve   {:>= LpSolve/GE, :<= LpSolve/LE, := LpSolve/EQ}
            add-constraint (fn [lhs cmp value]
                             (.strAddConstraint solver (lhs->str lhs) (cmp->lpsolve cmp) value))]
        ;; All variables are binary
        (doseq [t (range ST)]
          ;; We set variables from 1 to ST because column 0 is the objective function
          (.setBinary solver (+ t 1) true))

        ;; Order constraints
        (doseq [s     (range S), t  (range T)
                s'    (range S), t' (range T)
                :when (and (= :-> (get-task-order t t'))
                           (> s s'))]
          (add-constraint
           (+vars (assigned s t) (assigned s' t')) :<= 1))

        ;; Only one sentence per task
        (doseq [t (range T)]
          (add-constraint
           (apply +vars (for [s (range S)] (assigned s t))) := 1))

        ;; Objective function
        (.setMaxim solver)
        (.strSetObjFn solver (lhs->str (+vars-coefs all-variables (flatten sim-matrix))))

        ;;TODO: Add SOS constraints?
        (.solve solver)

        (let [result   (into [] (.getPtrVariables solver))
              st-pairs (for [s     (range S), t (range T)
                             :when (= 1.0 (result (+ (* s T) t)))]
                         [s t])]
          (.deleteLp solver)
          st-pairs))
      (catch java.lang.UnsatisfiedLinkError e
        (throw (Exception. "The libraries needed to run lp_solve were not found in any of the java.library.path folders.")))))

;; TODO: (spec-fn generate-cplex-lp-model ::t/cost-matrix ::t/order-matrix ::t/order-matrix)
(defn generate-cplex-lp-model [{:keys [flows sentences similarities flow-order sentence-order
                                       gateways close-gateways dummy-value] :as problem}]
  (let [T (count flows)     ; number of tasks
        S (count sentences) ; number of sentences

        flows     (if (vector? flows) flows (vec flows))
        sentences (if (vector? sentences) sentences (vec sentences))

        ilp-variables (for [t (range T), s (range S)] (str "a_" s "_" t))
        ilp-weights   (for [t (range T), s (range S)] (get-in similarities [(flows t) (sentences s)]))

        assigned (fn [s t] (str "a_"s "_"t))

        is-gateway? (set (indices-such-that (set gateways) flows))
        is-dummy?   #{(first (indices-such-that #(= dummy-value %) sentences))}

        get-flow-order     (fn [t1 t2]
                             (get-in flow-order [(flows t1) (flows t2)]))
        get-close-flow-order (fn [t-cl t2]
                               (let [close (close-gateways (flows t-cl))]
                                 (if close
                                   (get-in flow-order [close (flows t2)])
                                   ::no-order)))
        get-sentence-order (fn [s1 s2]
                             (get-in sentence-order [(sentences s1) (sentences s2)]))

        ;;ad-hoc ILP DSL
        restrictions (fn [strings] (println (clojure.string/join "\n" strings)))
        sum          (fn [& vars] (clojure.string/join " + " vars))

        r-id (fn [letter & args] (apply str letter "_" (clojure.string/join "_" args)))
        leq  (fn [a b] (str a " <= " b))
        geq  (fn [a b] (str a " >= " b))
        eq   (fn [a b] (str a " = " b))

        restriction (fn [restr-id body] (str restr-id ": " body))]

    (with-out-str
      (println "Maximize")
      (println (apply sum (map #(str %1 " " %2) ilp-weights ilp-variables)))
      (println "Subject To")

      ;; Task ordering restrictions
      (when-not (config :disable-restrictions)
        (restrictions
         (for [s     (range S), t  (range T)
               s'    (range S), t' (range T)
               :when (and (= :-> (get-flow-order t t'))
                          (not (is-gateway? t'))
                          (not (is-gateway? t))
                          (= :<- (get-sentence-order s s')))]
           (restriction (r-id "o" s s' t t') (leq (sum (assigned s t) (assigned s' t')) 1)))))

      ;; Gateway Order restrictions
      (when-not (config :disable-restrictions)
        (restrictions
         (for [s     (range S), g  (range T)
               s'    (range S), t' (range T)
               :when (and (is-gateway? g)
                          (= :<- (get-flow-order g t'))
                          (= :-> (get-sentence-order s s')))]
           (restriction (r-id "g" s s' g t') (leq (sum (assigned s g) (assigned s' t')) 1)))))

      ;;Closing gateway order restrictions
      (when-not (config :disable-restrictions)
        (restrictions
         (for [s     (range S), g  (range T)
               s'    (range S), t' (range T)
               :when (and (is-gateway? g)
                          ;; NOTE: get-close-flow-order compares order with respect to the
                          ;;       close-gateway pair of g
                          (= :-> (get-close-flow-order g t'))
                          (= :<- (get-sentence-order s s')))]
           (restriction (r-id "g" s s' g t') (leq (sum (assigned s g) (assigned s' t')) 1)))))

      ;; Tasks cannot map to the dummy sentence
      (restrictions
       (for [s     (range S), t (range T)
             :when (and (not (is-gateway? t))
                        (is-dummy? s))]
         (restriction (r-id "d" s t) (eq (assigned s t) 0))))

      ;; Every anchor point is pointed to by at most one gateway
      (restrictions
       (for [s (range S) :when (not (is-dummy? s))]
         (restriction
          (r-id "a" s)
          (leq (apply sum (for [t (range T) :when (is-gateway? t)] (assigned s t))) 1))))

      ;; All flow elements must be mapped to exactly one sentence
      (restrictions
       (for [t (range T)]
         (restriction (r-id "x" t) (eq (apply sum (for [s (range S)] (assigned s t))) 1))))

      ;; All sentences (except dummy) must correspond to at most task
      (comment
        (restrictions
         (for [s (range S)
               :when (not (is-dummy? s))]
           (restriction (r-id "k" s) (leq (apply sum (for [t (range T)] (assigned s t))) 1)))))

      (println "Bounds")
      (restrictions (map #(str "0 <= " % " <= 1") ilp-variables))
      (println "Binary")
      (println (clojure.string/join " " ilp-variables))
      (println "SOS")
      (restrictions
       (for [t (range T)]
         (str "sos"t": S1 :: "
              (clojure.string/join " " (for [s (range S)] (str (assigned s t) " : " s))))))
      (println "End"))))

(defn parse-gurobi-solution [solution-path]
  (let [solution (as-> (line-seq (clojure.java.io/reader solution-path)) $$
                   (rest $$)
                   (map (fn [val] (let [[var val] (clojure.string/split val #" ")
                                        [_, s, t] (re-matches #"a_(\d+)_(\d+)" var)]
                                    [[s t] val]))
                        $$)
                   (transform (walker string?) #(int (Math/round (Double/parseDouble %))) $$)
                   (filter #(-> % second pos?) $$)
                   (map first $$))]
    solution))

;;(spec-fn solve-with-gurobi ::t/cost-matrix ::t/order-matrix ::t/order-matrix (spec/* int?) (spec/* int?) (spec/* int?))
(defn solve-with-gurobi [model text similarities model-order text-order]
  (let [execution-id   (Math/abs (hash [similarities model-order text-order]))
        model-filename (.getAbsolutePath (io/file (home-relative (config :gurobi-tmp-path)) (str "model" execution-id ".lp")))
        sol-filename   (.getAbsolutePath (io/file (home-relative (config :gurobi-tmp-path)) (str "model" execution-id ".sol")))

        model-alignables (a/get-all-alignables model)
        text-alignables  (a/get-all-alignables text)

        gateways       (filter #(instance? org.activiti.bpmn.model.Gateway %) model-alignables)
        close-gateways (let [all-open-close-pairs (select [:processes ALL :open-close-pairs ALL] model)
                             find-pair (fn [g]
                                         (reduce
                                          (fn [_ [g1 g2]]
                                            (cond
                                              (= g g1) (reduced g2)
                                              (= g g2) (reduced g1)
                                              :else nil))
                                          nil
                                          all-open-close-pairs))]
                         (mapify
                          (map
                           (fn [g] [g (a/get-element model (find-pair (a/id g)))])
                           gateways)))

        problem {:flows          model-alignables
                 :sentences      text-alignables
                 :gateways       gateways
                 :dummy-value    (first (filter text/dummy? text-alignables))
                 :similarities   similarities
                 :flow-order     model-order
                 :sentence-order text-order
                 :close-gateways close-gateways}

        ;; Write the model file to /tmp
        __ (spit model-filename (generate-cplex-lp-model problem))

        ;;TODO: Set a way to control the number of cores in gurobi
        ;;TODO: Kill gurobi if this thread dies
        ;; Invoke gurobi as a shell process and solve the problem
        parsed-sol (let [{:keys [exit out] :as result} (sh "gurobi_cl" (str "ResultFile=" sol-filename) model-filename)]
                     (if (= exit 0)
                       (parse-gurobi-solution sol-filename)
                       (do (throw (Exception. (str "Error during gurobi execution. Are the environment variables"
                                                   "properly deifned?\nGurobi Output:\n\n"
                                                   result))))))

        __ (io/delete-file (io/file model-filename))
        __ (io/delete-file (io/file sol-filename))]
    (as-> parsed-sol $$
      (transform [ALL FIRST] #(nth text-alignables %) $$)
      (transform [ALL LAST] #(nth model-alignables %) $$))))
