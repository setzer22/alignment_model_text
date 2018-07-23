(ns edu.upc.modelvsdocument.bpmn-alignable-test
  (:require [clojure.test :as test :refer :all]
            [clojure.spec :as spec]
            [edu.upc.modelvsdocument.bpmn-alignable :as bpmn-al]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.schemas :as t]))


(deftest spec-model
  (is (spec/valid? ::t/bpmn
                    (bpmn-al/get-debug-model "Zoo"))))


;;(spit "src/test/clojure/edu/upc/modelvsdocument/model_output1.clj"
      ;;(with-out-str (clojure.pprint/pprint (bpmn-al/get-debug-model "Zoo"))))


