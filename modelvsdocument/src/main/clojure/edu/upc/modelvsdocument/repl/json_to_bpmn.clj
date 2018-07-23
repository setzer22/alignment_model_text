(ns edu.upc.modelvsdocument.json-to-bpmn
  (:import [org.activiti.bpmn.model BpmnModel Task Gateway Event ManualTask]
           [org.activiti.bpmn.model.Process]
           [org.activiti.bpmn.converter BpmnXMLConverter]))


(comment
  #_"Test 1: Create an activiti model and export it"

  

  (let [a1 (doto (ManualTask.)
             (.setId "a1"))
        a2 (doto (ManualTask.)
             (.setId "a2"))
        process (doto (org.activiti.bpmn.model.Process.)
                  (.addFlowElement a1)
                  (.addFlowElement a2))
        model (doto (edu.upc.modelvsdocument.bpmn/read-model "/home/josep/base.bpmn")
                (.addProcess process))
        converter (BpmnXMLConverter.)]
    (String. (.convertToXML converter model)
             "UTF-8"))

  END)
