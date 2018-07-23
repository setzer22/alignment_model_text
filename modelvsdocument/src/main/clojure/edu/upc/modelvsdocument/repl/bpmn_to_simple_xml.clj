(ns edu.upc.modelvsdocument.repl.bpmn-to-simple-xml
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all ]
            [edu.upc.modelvsdocument.bpmn :as bpmn :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [clojure.data.xml :as xml]
            [clojure.pprint :as pprint :refer [pprint]]
            [clojure.inspector :as inspector :refer :all]
            [clojure.set :as set]
            [com.rpl.specter :as s]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell :refer [sh]]))

(defmacro section
  {:style/indent 1}
  [& body]
  `(do ~@body))

(section "Global definitions"
  (def benchmarks-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/")

  (def xml-path "/home/josep/Repositories/inconsistenciesmodeltext/input/xmlfiles.bak/")

)

(section "Instrument the whole namespace for testing"
  (stest/instrument (stest/enumerate-namespace 'edu.upc.modelvsdocument.repl.bpmn-to-simple-xml)))

(section "First of all let's load a couple bpmn models and also an example xml."
  (def bpmn1 (-> (str benchmarks-path "mini-benchmark/Bicycle_Manufacturer.bpmn") read-model build-model))

  (def bpmn2 (-> (str benchmarks-path "mini-benchmark/Dispatch-of-goods.bpmn") read-model build-model))

  (def bpmn3 (-> (str benchmarks-path "mini-benchmark/Zoo.bpmn") read-model build-model))

  (def model6-4 (-> (str benchmarks-path "big-benchmark/Model6-4.bpmn") read-model build-model))

  (def xml-example (-> (str xml-path "bicyclemanufacturing.xml") slurp xml/parse-str))
)

(section "We want to convert the BPMN models into the XML file. As we can see the XML file is a
          simplified version of the XML one containing only the tasks and a simplified version of
          the graph. Let's write the basix XML building blocks"

  (spec/def ::id (spec/and string? #(re-matches #"\d+" %)))
  (spec/def ::original-id string?)
  (spec/def ::label string?)
  (spec/def ::activity (spec/keys :req-un [::id ::label]))

  (spec-fn xml-activity ::activity)
  (defn xml-activity [{:keys [id label original-id]}]
    (xml/element "activity" {:id id, :originalId original-id}
                 (xml/element "label" {} label)))

  (xml/indent-str (xml-activity {:id "4"
                                 :original-id "task4"
                                 :label "Inform storehouse and engineering department"}))

  (spec-fn xml-pair ::id ::id)
  (defn xml-pair [pred suc]
    (xml/element "pair" {}
                 (xml/element "pred" {} pred)
                 (xml/element "succ" {} suc)))

  (xml/indent-str (xml-pair "3" "2"))

  (spec-fn xml-follows (spec/* (spec/tuple ::id ::id)))
  (defn xml-follows [arcs]
    (apply xml/element "follows" {}
           (map #(apply xml-pair %) arcs)))

  (xml/indent-str (xml-follows
                   [["3" "2"]
                    ["1" "2"]
                    ["2" "1"]]))

  (spec-fn xml-activities (spec/* ::activity))
  (defn xml-activities [activities]
    (apply xml/element "activities" {}
           (map #(xml-activity %) activities)))

  (println (xml/indent-str (xml-activities
                            [{:id "1" :label "foo" :original-id "task1"}
                             {:id "2" :label "bar" :original-id "task2"}
                             {:id "3" :label "baz" :original-id "task2"}])))

  (spec-fn xml-text string?)
  (defn xml-text [text]
    (xml/element "text" {} text))

  (spec-fn xml-process (spec/* ::activity) (spec/* (spec/tuple ::id ::id)) string?)
  (defn xml-process [activities arcs text]
    (xml/element "process" {}
                 (xml-activities activities)
                 (xml-follows arcs)
                 (xml-text text)))

  (println
   (xml/indent-str
    (xml-process [{:id "1" :label "foo" :original-id "task1"} {:id "2" :label "bar" :original-id "task2"} {:id "3" :label "baz" :original-id "task2"}]
                                [["3" "2"] ["1" "2"] ["2" "1"]]
                                "First foo must be done. Then the employee performs bar. In the end baz is executed.")))

)

(section "Since our IDs are strings and they only use increasing integers, we must create a mapping from
          our ids to the integers. It's very important to notice that in their format, ID number 0 bears a
          different meaning. The (unique) start event. Thus our ID mapping starts at 1. For convenience
          ID 0 is mapped to itself so we don't have to treat it differently"

  (spec/def ::id-mapping (spec/map-of string? ::id))

  (spec-fn create-id-mapping ::t/bpmn)
  (defn create-id-mapping [bpmn]
    (assert (not (in? (bpmn/all-task-ids bpmn) "0")) "ERROR: ID number 0 is reserved.")
    (mapify (conj ["0" "0"]
                  (map #(vector %1 (str %2)) (bpmn/all-task-ids bpmn) (rest (range))))))

  (create-id-mapping bpmn1)

  )

(section "Once we have the main building blocks, we need to convert our BPMN model representation
          to something that fits theirs. For this we need a way to compute the set of tasks (easy)
          and the set of arcs (not so easy). Note that for the set of arcs we must apply the following
          rule: T_a -> T_b <=> There's a path from T_a to T_b using sequenceflows, gateways and
          messageflow arcs."

         (def id-mapping (create-id-mapping bpmn1))

         "To the list of activities extracted, we add a special activity called start which serves as
   the start event."
         (spec-fn get-activities ::t/bpmn ::id-mapping)
         (defn get-activities [bpmn, id-mapping]
           (conj
            (map
             (fn [task] {:id (id-mapping (.getId task)) :label (.getName task) :original-id (.getId task)})
             (bpmn/all-tasks bpmn))
            {:id "0" :label "start" :original-id "nil"}))

         (get-activities bpmn1 id-mapping)

         (spec/def ::graph (spec/map-of any? (spec/and (spec/coll-of any?) set?)))

         (spec/fdef merge-graphs :args (spec/* ::graph))
         (defn merge-graphs [& graphs]
           (reduce #(merge-with set/union %1 %2) graphs))

         (spec-fn add-arcs ::graph (spec/* (spec/tuple string? string?)))
         (defn add-arcs [G arcs]
           (reduce (fn [G [src dst]]
                     (update G src conj dst)) G arcs))

         (spec-fn all-tasks-in-reach ::graph string? fn?)
         (defn all-tasks-in-reach [G node task?]
           ((fn all-tasks-in-reach-rec
              [G node task? visited]
                (let [visited (conj visited node)]
                (distinct
                 (mapcat
                  (fn [neighbour]
                    (cond
                      (task? neighbour) [neighbour]
                      (visited neighbour) []
                      :else (all-tasks-in-reach-rec G neighbour task? (conj visited neighbour))))
                  (G node)))))
            G, node, task?, #{}))

         (all-tasks-in-reach {:a #{"a"} "a" #{:a}} :a keyword?) ;l-OOPS
         (all-tasks-in-reach {:a #{"a" "b" :c} "a" #{:d "a"} "b" #{"c" "d"} "c" #{:e} "d" #{}} :a keyword?)

         (defn graph-to-arcs [graph]
           (mapcat (fn [[src dests]] (pair-with (constantly src) dests)) graph))

         (graph-to-arcs
          {:a #{:b :c :d}
           :b #{:k :l :m}})

         "An arc in the XML task graph can be due to 3 reasons:
     -> There's an arc in the real task graph
     -> There's an arc that goes from task A to task B and all other nodes in the path are gateways.
        edges in this path can be also be messageFlows
     -> There's an arc that goes from a start event to task A."
         (spec-fn get-arcs ::t/bpmn ::id-mapping)
         (defn get-arcs [bpmn, id-mapping]
           (let [bpmn-graph   (add-arcs
                               (apply merge-graphs (s/select [:processes s/ALL :graph] bpmn))
                               (:message-flows bpmn))
                 __ "Note: There's no way to know what should we consider the real start id if
              two tasks have no relationship whatsoever and both are start events. In this
              case we always select the first."
                 start-id     (first (set/difference (set (keys bpmn-graph))
                                                     (apply set/union (vals bpmn-graph))))
                 task-ids     (bpmn/all-task-ids bpmn)
                 task-ids-set (set (bpmn/all-task-ids bpmn))
                 task?        #(contains? task-ids-set %)
                 task-graph   (reduce #(assoc %1 %2 #{}) {} task-ids)
                 task-graph   (reduce (fn [G n] (assoc G n (set (all-tasks-in-reach bpmn-graph n task?))))
                                      task-graph
                                      task-ids)
                 task-graph   (assoc task-graph "0" (set (all-tasks-in-reach bpmn-graph start-id task?))) ; PATCH: Add the start task
                 task-graph   (s/transform (s/walker string?) id-mapping task-graph)]
             (graph-to-arcs task-graph)))

         (get-arcs model6-4 (create-id-mapping model6-4)))

(section "Finally we can use the above functions to build the final XML"
  (defn bpmn->simplified-xml [bpmn text]
    (let [id-mapping #spy/d (create-id-mapping bpmn)
          activitites #spy/d (get-activities bpmn id-mapping)
          arcs (get-arcs bpmn id-mapping)]
      (xml-process activitites arcs text)))

  (xml-activities (get-activities bpmn3 (create-id-mapping bpmn3)))


  )

(section "We can now export some models in the new format"

  (def export-folder (str benchmarks-path "/exported-xml-models/"))
  (io/make-parents (str export-folder "/foo.txt"))

  (defn export-model [model-path text-path output-path]
    (let [bpmn-model (-> model-path bpmn/read-model bpmn/build-model)
         text (-> text-path slurp)]
      (spit output-path
            (xml/indent-str (bpmn->simplified-xml bpmn-model text)))))

  (export-model (str benchmarks-path "/The_Big_Model/big.bpmn")
                (str benchmarks-path "/The_Big_Model/big.txt")
                "big.xml")

  )

(section "And finally we can load their tool with the models we want to execute"

  (def xml-files-path "/home/josep/Repositories/inconsistenciesmodeltext/input/xmlfiles/")
  (def text-files-path "/home/josep/Repositories/inconsistenciesmodeltext/input/texts/")

  (defn delete-files-in-folder [folder-path extension]
    (mapv io/delete-file (get-files-with-extension extension folder-path)))

  (defn clean-executions []
    (delete-files-in-folder xml-files-path "xml")
    (delete-files-in-folder text-files-path "txt"))


  (defn load-benchmark [benchmark-path model-names]
    (clean-executions)
    (for [name model-names
          :let [bpmn-path (str benchmark-path name ".bpmn")
                text-path (str benchmark-path name ".txt")]]
      (try
        (do (export-model bpmn-path text-path (str xml-files-path "/" name ".xml"))
            (spit (str text-files-path "/" name ".txt") (slurp text-path)))
        (catch Throwable e (println "Model " name " couldn't be exported. Reason: " (.getMessage e))))))

  (def big-model-path (str benchmarks-path "/The_Big_Model/"))


  )
(defn all-case-names-in [path]
  (sort (distinct
        (remove nil?
                (map strip-extension
                     (filter
                      #(= (extension %) "bpmn")
                      (file-seq
                       (clojure.java.io/file path))))))))

(comment "REPL"
         (clean-executions)

         (let [path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/New-Zoo/"]
           (load-benchmark path (all-case-names-in path)))

         )


