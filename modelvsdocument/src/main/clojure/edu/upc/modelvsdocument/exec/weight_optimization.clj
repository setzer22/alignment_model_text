(ns edu.upc.modelvsdocument.exec.weight-optimization
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [random-seed.core :refer :all]
            [edu.upc.modelvsdocument.config :as config :refer [config]]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.core :refer :all]
            [edu.upc.modelvsdocument.verification.new-groundtruth :as groundtruth]
            [edu.upc.modelvsdocument.Main :as main]
            [edu.upc.modelvsdocument.verification.benchmark :as benchmark]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class
   :name edu.upc.modelvsdocument.WeightOptimizer
   :main true))

"We're going to run an optimisation meta-heuristic to find the best set of weights.
First of ull let us define the set of results for the current weight set."

"Through all this file we'll be referring to individuals as the sets of weights. Each
individual is uniquely mapped to an actual result, which is represented as an execution
of the benchmark. Individuals are evaluated using the micro and macro averages (higher is better)"

"The folder contains all model-text pairs cases we want to include in our benchmark"
(def ^:dynamic benchmark-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/optimization-benchmark")

"The folder contains all the corresponding groundtruth files for the above pairs"
(def ^:dynamic groundtruths-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/json-groundtruth-opt")




(do "Let's define a helper function that will load the benchmark data"
    (defn get-benchmark-files [models-path texts-path groundtruths-path]
      (let [srt (fn srt [coll] (sort-by #(.getName %) coll))
            model-files (srt (get-files-with-extension "bpmn" models-path))
            text-files (srt (get-files-with-extension "txt" texts-path))
            groundtruth-files (srt (get-files-with-extension "json" groundtruths-path))
            common  (set/intersection (into #{} (map strip-extension) model-files)
                                      (into #{} (map strip-extension) text-files)
                                      (into #{} (map strip-extension) groundtruth-files))
            model-files (filter #(common (strip-extension %)) model-files)
            text-files (filter #(common (strip-extension %)) text-files)
            groundtruth-files (filter #(common (strip-extension %)) groundtruth-files)]
        {:model-files model-files
         :text-files text-files
         :groundtruth-files groundtruth-files}))

    (def model-files nil)
    (def text-files nil)
    (def groundtruth-files nil)

    (defn load-benchmark [models-path texts-path groundtruths-path]
      (let [{:keys [model-files text-files groundtruth-files]} (get-benchmark-files
                                                                models-path
                                                                texts-path
                                                                groundtruths-path)]
        (def model-files model-files)
        (def text-files text-files)
        (def groundtruth-files groundtruth-files))))

"We introduce now a way to evaluate the fitness of an individual (set of weights). We run it
against the benchmark and compute accuracy. As an optimization, the fitness function is memoized."
(do
  (defn fitness-raw [individual]
    (let [{accuracy :all}
          (benchmark/run-extended-benchmark
           model-files text-files groundtruth-files
           {:use-vu-similarity false
            :enable-anchors true
            :feature-weight-overrides individual
            :enable-cutoff true
            :enable-wrong-order-cutoff true
            :remove-back-edges? true}
           8)]
      accuracy))
  (def fitness (memoize fitness-raw)))

"Now we need a way to generate new individuals, that is, a random weight generator.

Note that weights are not bounded, but the only thing that matters is the relative weights
 between features. Because of that, we
only consider weights between 0 and 1"

"To avoid precision errors with gurobi, we restrict all values to 5 decimal places."

""
(do

  ;;TODO: New features
  (def genes
    [:has-form :has-lemma :has-action :agent-head :patient-head :in-agent :in-patient
     :has-synset :has-parent-synset :has-discourse-marker
     #_:lemma-conditional-pred #_:lemma-conditional-follow])

  (defn rand-val [] (/ (rand-int 100000) 100000.0))

  (defn random-individual []
    (mapify (map vector genes (repeatedly rand-val)))))

"First, we should ensure there is, in fact, some variance when playing with the weights."

(comment


  (time (fitness-raw {:has-lemma 0.12071, :has-parent-synset 0.05025, :in-agent 0.1392, :agent-head 0.16927, :has-form 0.8387, :patient-head 0.40324, :in-patient 0.33904, :has-discourse-marker 0.72142, :has-synset 0.97612, :has-action 0.22651}))

  (def --preliminary-test (time (doall (map fitness (repeatedly 100 random-individual)))))

  (float (apply max --preliminary-test)) ;; 0.65

  (float (apply min --preliminary-test))) ;; 0.58

"To optimise we'll be using a genetic algorithm. Tipically genetic algorithms must also support mutation
 and crossover of individuals. We implement this operations"
(do

  (def number-mutated-features 2)

  (defn random-subset [seq len]
    (take len (shuffle seq)))

  (defn mutate [individual]
    (let [mutating-attributes (random-subset genes number-mutated-features)]
      (reduce (fn [individual feature]
                (assoc individual feature (rand-val)))
              individual
              mutating-attributes)))

  (defn crossover [individual1 individual2]
    (let [num-genes (count genes)
          cut-point (rand-int (count genes))
          father-attributes (take cut-point genes)
          mother-attributes (drop cut-point genes)]
      (merge
       (reduce #(assoc %1 %2 (get individual1 %2)) {} father-attributes)
       (reduce #(assoc %1 %2 (get individual2 %2)) {} mother-attributes)))))

"The genetic algorithm doesn't care about the best individual, but we do. That's why we'll build
an ad-hoc ranking data structure. A ranking is a data structure allowing the insertion of elements.
Elements must have some kind of score. A ranking of size N will store the best N elements."
(do
  (defn mk-ranking [size f]
    {:size size
     :elements []
     :score-fn f})

  (defn ranking-insert
    [{:keys [size elements score-fn] :as ranking} new-element]
    (assoc ranking :elements (as-> elements $$
                               (conj $$ [(score-fn new-element) new-element])
                               (sort-by (comp - first) $$)
                               (take size $$)
                               (into [] $$)))))

"This function allows us to select individuals from a population distributed
according to their fitness."
(defn roulette-select
  ([population] (roulette-select population (map fitness population)))
  ([population fitnesses]
   (if (empty? population)
     (random-individual) ; Avoids crashing when all the population is the same individual
     (let [fitness-sum (reduce + fitnesses)
           accum-fitness (reductions + fitnesses)
           random-value (rand fitness-sum)]
       (nth population
            (min (dec (count population))
                 (count (take-while #(> random-value %) accum-fitness))))))))

"With this, we have all the ingredients we need to run a genetic algorithm.
Let's implement it"
(do

  (def crossover-factor 0.7)
  (def mutation-factor 0.1)

  "The main part of the genetic algorithm is the function which takes the current
   population and generates the next population. A historical ranking of the best
   individuals seen so far is also preserved. Additionally, a path to a log file
   should be supplied and statistics will be written on it at each generation."
  ;; WIL: HERE: TODO: XXX: 
  (defn next-generation [population best-so-far]
    (loop [new-population []
           father (roulette-select population)
           mother (roulette-select (remove #(= father %) population))]
      (if (= (count population)
             (count new-population))
        [new-population (apply max-key fitness best-so-far new-population)]
        (let [new-individual (if (< (rand) crossover-factor)
                               (crossover father mother)
                               (max-key fitness father mother))
              new-individual (if (< (rand) mutation-factor)
                               (mutate new-individual)
                               new-individual)]
          (recur (conj new-population new-individual)
                 (roulette-select population)
                 (roulette-select population))))))

  (defn initial-population [size]
    (repeatedly size random-individual))

  (defn genetic-optimise [population-size number-of-generations]
    (let [initial-number-generations number-of-generations
          __ (println "\tGeneration 0")]
      (loop [population (initial-population population-size)
             best (apply max-key fitness population)
             number-of-generations number-of-generations]
        (if (<= number-of-generations 0)
          [population best]
          (let [gen (inc (- initial-number-generations number-of-generations))
                __ (println "\tGeneration" gen)
                [new-population new-best] (next-generation population best)
                __ (println "Best of generation " gen " (fitness: " (float (fitness new-best)) ")\n------\n"new-best "\n------")
                __ (println "Population fitness: " (map fitness new-population))]
            (recur new-population new-best (dec number-of-generations))))))))

"Finally we build the main executable to run on the cluster"
(defn main-optimize [config-path]
  (let [{:keys [number-of-generations, population-size,
                num-partitions, models-path, texts-path
                groundtruths-path output-path module-config-path
                freeling-dir-path cache-path credentials-path] :as config}
        (read-string (slurp config-path))]
    (main/-loadConfig module-config-path credentials-path)
    (edu.upc.nlp4bpm-commons.config/-setConfig "freeling-dir-path"
                                               freeling-dir-path)
    (edu.upc.nlp4bpm-commons.cache/initialize cache-path)
    (edu.upc.nlp4bpm-commons.freeling-api/set-mode "local")
    (load-benchmark models-path texts-path groundtruths-path)
    (spit output-path
          (into [] (genetic-optimise population-size
                                     number-of-generations)))))

(defn -main [config-path & _]
  (main-optimize config-path)
  (System/exit 0))

(comment

  (def --cluster-result (read-string (slurp "/home/josep/genetic-results.txt")))

  (apply max (map float (map fitness (first --cluster-result))))

  (float (fitness {:has-lemma 0.33388, :has-parent-synset 0.11518, :in-agent 0.14035, :agent-head 0.08782, :has-form 0.82331, :patient-head 0.70631, :in-patient 0.00724, :lemma-conditional-pred 0.71281, :has-discourse-marker 0.94884, :has-synset 0.6623, :has-action 0.73167, :lemma-conditional-follow 0.30683}))

  (def --the-result
    (main-optimize "/home/josep/optimizer-config.clj"))

  (fitness
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

  --the-result

  (load-benchmark "/home/josep/DSS_BENCHMARK/GeneticDataset/Models"
                  "/home/josep/DSS_BENCHMARK/GeneticDataset/Texts"
                  "/home/josep/DSS_BENCHMARK/GeneticDataset/Groundtruths")


  (-> "/home/josep/genetic-results.txt" slurp read-string second fitness float)

  )
