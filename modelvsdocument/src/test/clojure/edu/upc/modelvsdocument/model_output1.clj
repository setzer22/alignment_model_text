{:model
 #object[org.activiti.bpmn.model.BpmnModel 0x3a9cd1f7 "org.activiti.bpmn.model.BpmnModel@3a9cd1f7"],
 :processes
 [{:graph
   {"Task_0j4npun" #{"ParallelGateway_0jj7fu7"},
    "Task_1qrcrh7" #{"ParallelGateway_1oafzb2"},
    "ParallelGateway_0g0ww2s" #{"Task_1qrcrh7" "Task_0oih0sg"},
    "Task_02qz4bj" #{"ParallelGateway_0jj7fu7"},
    "ParallelGateway_0jj7fu7" #{"EndEvent_1pqhyp2"},
    "ParallelGateway_1oafzb2" #{"IntermediateCatchEvent_1dgnju9"},
    "Task_1gst9f9" #{"Task_0j4npun"},
    "Task_0qrzi4y" #{"ParallelGateway_0tw525m"},
    "ParallelGateway_0tw525m" #{"Task_1gst9f9" "Task_106edn4"},
    "Task_0oih0sg" #{"ParallelGateway_1oafzb2"},
    "Task_106edn4" #{"ParallelGateway_0g0ww2s"},
    "IntermediateCatchEvent_1dgnju9" #{"Task_02qz4bj"},
    "StartEvent_13penc1" #{"Task_0qrzi4y"},
    "EndEvent_1pqhyp2" #{}},
   :pool
   #object[org.activiti.bpmn.model.Pool 0x46eb2ef8 "org.activiti.bpmn.model.Pool@46eb2ef8"],
   :lanes
   [#object[org.activiti.bpmn.model.Lane 0x2d119712 "org.activiti.bpmn.model.Lane@2d119712"]
    #object[org.activiti.bpmn.model.Lane 0x58f984ff "org.activiti.bpmn.model.Lane@58f984ff"]
    #object[org.activiti.bpmn.model.Lane 0x15033717 "org.activiti.bpmn.model.Lane@15033717"]],
   :task-to-lane
   {"Task_0j4npun" "Lane_0a6rbai",
    "Task_1qrcrh7" "Lane_0nocd2c",
    "ParallelGateway_0g0ww2s" "Lane_0nocd2c",
    "Task_02qz4bj" "Lane_0nocd2c",
    "ParallelGateway_0jj7fu7" "Lane_0nocd2c",
    "ParallelGateway_1oafzb2" "Lane_0nocd2c",
    "Task_1gst9f9" "Lane_0nocd2c",
    "Task_0qrzi4y" "Lane_0nocd2c",
    "ParallelGateway_0tw525m" "Lane_0nocd2c",
    "Task_0oih0sg" "Lane_0zpnyy9",
    "Task_106edn4" "Lane_0nocd2c",
    "IntermediateCatchEvent_1dgnju9" "Lane_0nocd2c",
    "StartEvent_13penc1" "Lane_0nocd2c",
    "EndEvent_1pqhyp2" "Lane_0nocd2c"},
   :process-id "Process_1",
   :start-event "StartEvent_13penc1",
   :end-events ("EndEvent_1pqhyp2"),
   :process
   #object[org.activiti.bpmn.model.Process 0x76a74b47 "org.activiti.bpmn.model.Process@76a74b47"]}
  {:graph
   {"StartEvent_0vpq959" #{"Task_0420rk1"},
    "Task_0420rk1" #{"Task_0oz8hjr"},
    "Task_0oz8hjr" #{"EndEvent_0fwg27l"},
    "EndEvent_0fwg27l" #{}},
   :pool
   #object[org.activiti.bpmn.model.Pool 0x75e22ef8 "org.activiti.bpmn.model.Pool@75e22ef8"],
   :lanes [],
   :task-to-lane nil,
   :process-id "Process_05qliyd",
   :start-event "StartEvent_0vpq959",
   :end-events ("EndEvent_0fwg27l"),
   :process
   #object[org.activiti.bpmn.model.Process 0x356457c0 "org.activiti.bpmn.model.Process@356457c0"]}
  {:graph
   {"Task_0ssja9k" #{"EndEvent_0ueqhec"},
    "Task_0w3czw9" #{"IntermediateCatchEvent_1pyp6o5"},
    "Task_17x20z3" #{"ExclusiveGateway_0ajb73w"},
    "IntermediateCatchEvent_1pyp6o5" #{"Task_0ssja9k"},
    "StartEvent_1yl25iv" #{"Task_17x20z3"},
    "ExclusiveGateway_06m442q" #{"Task_0wrm9ya"},
    "ExclusiveGateway_0ajb73w" #{"Task_14zetua" "Task_064spq4"},
    "Task_0wrm9ya" #{"Task_0w3czw9"},
    "EndEvent_0ueqhec" #{},
    "Task_14zetua" #{"ExclusiveGateway_06m442q"},
    "Task_064spq4" #{"ExclusiveGateway_06m442q"}},
   :pool
   #object[org.activiti.bpmn.model.Pool 0x4f6824ca "org.activiti.bpmn.model.Pool@4f6824ca"],
   :lanes [],
   :task-to-lane nil,
   :process-id "Process_0m9k2qh",
   :start-event "StartEvent_1yl25iv",
   :end-events ("EndEvent_0ueqhec"),
   :process
   #object[org.activiti.bpmn.model.Process 0x320f8154 "org.activiti.bpmn.model.Process@320f8154"]}],
 :message-flows
 [["Task_02qz4bj" "IntermediateCatchEvent_1pyp6o5"]
  ["Task_0oih0sg" "StartEvent_0vpq959"]
  ["Task_0oz8hjr" "IntermediateCatchEvent_1dgnju9"]
  ["Task_0wrm9ya" "StartEvent_13penc1"]]}
