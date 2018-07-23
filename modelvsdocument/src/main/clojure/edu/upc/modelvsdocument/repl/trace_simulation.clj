(ns edu.upc.modelvsdocument.repl.trace-simulation
  (:require [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.repl.bpmn-to-simple-xml :refer [add-arcs merge-graphs]]
            [edu.upc.modelvsdocument.repl.xes-testing :as xes]
            [com.rpl.specter :as s]
            [clojure.set :as set]
            [clojure.java.io :as io]))



(defn simplify-graph [bpmn]
  (add-arcs
   (apply merge-graphs (s/select [:processes s/ALL :graph] bpmn))
   (:message-flows bpmn)))

(defn choose-one [list]
  (nth list (rand-int (count list))))

(defn interleave-randomly [& seqs]
  (let [ord (map first (shuffle seqs))
        remaining-seqs (map rest (filter seq seqs))]
    (if (seq remaining-seqs)
      (concat ord (apply interleave-randomly remaining-seqs))
      [])))

(defn get-start [simple-graph]
  (let [starter-set (set/difference (set (keys simple-graph))
                                    (apply set/union (vals simple-graph)))]
    (when (> (count starter-set) 1)
      (println "[WARNING]: Graph has more than one start event. Taking one randomly"))
    (first starter-set)))

(defn generate-trace-ids [bpmn]
  (let [G          (simplify-graph bpmn)
        end-event? (set (flatten (map :end-events (:processes bpmn))))
        adjacent   (fn [node] (seq (G node)))
        trace ,,,
        (fn trace [node] (cond
                     (bpmn/exclusive-gateway? bpmn node) (trace (choose-one (adjacent node)))
                     (bpmn/parallel-gateway? bpmn node)  (apply interleave-randomly (map trace (adjacent node)))
                     (end-event? node)                   []
                     :else                               (cons node (trace (choose-one (adjacent node))))))
        ]
    (remove nil? ;TODO: There shouldn't be nils in this trace...
            (trace (-> G get-start adjacent first)))))

(defn id->event [bpmn id]
  (let [lane (bpmn/lane-of-task id bpmn)
        pool (bpmn/pool-of-task id bpmn)]
    (xes/event (cond-> {"concept:name" (bpmn/task-name-of-id bpmn id)}
                       (and lane (.getName lane)) (assoc "Role" (.getName lane))
                       (and pool (.getName pool)) (assoc "org:group" (.getName pool))))))

(defn generate-trace [bpmn trace-id]
  (xes/mk-trace (map (partial id->event bpmn)
                     (generate-trace-ids bpmn))
                {"concept:name" trace-id
                 "creator" "jsanchezf"}))

(defn gen-counter []
  (let [counter (atom 0)]
    (fn [] (swap! counter inc)
      @counter)))

(defn generate-log [bpmn num-traces]
  (let [id-generator (gen-counter)
        traces (repeatedly num-traces #(generate-trace bpmn (str "trace-"(id-generator))))]
    (xes/mk-log traces)))

(comment
  "REPL"

  (def test-model (-> "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/New-Zoo/ZooLoop.bpmn"
                      bpmn/read-model
                      bpmn/build-model))

  (xes/save-to-xml (generate-log test-model 20) (io/file "/home/josep/test.xes"))

  (.getName (bpmn/lane-of-task "Task_1qrcrh7" test-model))
  (.getAttributes (id->event test-model "Task_1qrcrh7"))

  ;; Simplified graph:
  {"Task_0ssja9k" #{"EndEvent_0ueqhec"},
   "Task_0j4npun" #{"ParallelGateway_0jj7fu7"},
   "Task_1qrcrh7" #{"ParallelGateway_1oafzb2"},
   "Task_0w3czw9" #{"IntermediateCatchEvent_1pyp6o5"},
   "StartEvent_0vpq959" #{"Task_0420rk1"},
   "ParallelGateway_0g0ww2s" #{"Task_1qrcrh7" "Task_1hgxf0p"},
   "Task_17x20z3" #{"ExclusiveGateway_0ajb73w"},
   "Task_0420rk1" #{"Task_0oz8hjr"},
   "Task_02qz4bj"
   #{"ParallelGateway_0jj7fu7" "IntermediateCatchEvent_1pyp6o5"},
   "ParallelGateway_0jj7fu7" #{"EndEvent_1pqhyp2"},
   "IntermediateCatchEvent_1pyp6o5" #{"Task_0ssja9k"},
   "StartEvent_1yl25iv" #{"Task_17x20z3"},
   "EndEvent_0fwg27l" #{},
   "ParallelGateway_1oafzb2" #{"IntermediateCatchEvent_1dgnju9"},
   "Task_0oz8hjr" #{"EndEvent_0fwg27l" "IntermediateCatchEvent_1dgnju9"},
   "ExclusiveGateway_06m442q" #{"Task_0wrm9ya"},
   "Task_1gst9f9" #{"Task_0j4npun"},
   "ExclusiveGateway_0ajb73w" #{"Task_14zetua" "Task_064spq4"},
   "Task_0qrzi4y" #{"ParallelGateway_0tw525m"},
   "ParallelGateway_0tw525m" #{"Task_1gst9f9" "Task_106edn4"},
   "Task_0oih0sg" #{"StartEvent_0vpq959" "ParallelGateway_1oafzb2"},
   "Task_1hgxf0p" #{"Task_0oih0sg"},
   "Task_0wrm9ya" #{"Task_0w3czw9" "StartEvent_13penc1"},
   "EndEvent_0ueqhec" #{},
   "Task_14zetua" #{"ExclusiveGateway_06m442q"},
   "Task_106edn4" #{"ParallelGateway_0g0ww2s"},
   "IntermediateCatchEvent_1dgnju9" #{"Task_02qz4bj"},
   "StartEvent_13penc1" #{"Task_0qrzi4y"},
   "Task_064spq4" #{"ExclusiveGateway_06m442q"},
   "EndEvent_1pqhyp2" #{}}
  )


