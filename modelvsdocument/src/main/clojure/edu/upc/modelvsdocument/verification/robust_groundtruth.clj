(ns edu.upc.modelvsdocument.verification.robust-groundtruth)

"We have a problem in our current groundtruth comparison approach. Right
 now to find out which task in the real model belongs to the task in the
 groundtruth we compare by using string similarity. The string from the
 actual model that resembles the most the string in the groundtruth file
 is assuming to be the candidate.

 This fails whenever we have repeated tasks.

 Possible solution:

 Patch the current code by adding an additional structure which maps, for
 each model, the model ID to the groundtruth ID. If an ID is not present
 in this structure, the old method will be used. Thus, we only need to sp-
 ecify the repeated tasks for the models which do have repeated tasks.

 The structure will look like this: "

{:model-name "Model9-1"
 ; ID in model   ->  ID in groundtruth
 :ids {"Task_1wzib5c" "1"
       "Task_0d0s2c9" "3"}}

"Whenever Model9-1 is being evaluated, and the evaluation corresponding
 to Task_1wzib5c is performed, the matcher should directly resolve that
 this task corresponds to activity 1 in the groundtruth, without comp-
 uting the similarity outside."

"To do this, we'll have to modify the groundtruth pipeline so it knows at
 all times the name of the model being evaluated and the original task ids
 when coming from the model. This makes the comparison much more asymmetrical."

"Under further inspection we see that Model9-6 and Model10-4 don't need any
 fixing, since both repeated tasks are mapped to the same sentence."

"We now include the rest of the structures"

{:model-name "Model2-1"
 :ids {"Task_15" "16"
       "Task_24" "25"}}

{:model-name "Model10-14"
 :ids {"Task_0" "4" ;Confirm bill
       "Task_3" "1"
       "Task_5" "7"

       "Task_1" "5" ; Reject bill
       "Task_4" "2"
       "Task_6" "8"

       "Task_2" "3" ; Examine
       "Task_7" "6"}}

"We finally construct the final structure aggregating all the data."

(def groundtruth-patches
  {"Model9-1"
   {:model-name "Model9-1"
    :ids {"Task_1wzib5c" 1
          "Task_0d0s2c9" 3}}

   "Model2-1"
   {:model-name "Model2-1"
    :ids {"Task_15" 16
          "Task_24" 25}}

   "Model10-14"
   {:model-name "Model10-14"
    :ids {"Task_0" 4 ;Confirm bill
          "Task_3" 1
          "Task_5" 7

          "Task_1" 5 ; Reject bill
          "Task_4" 2
          "Task_6" 8

          "Task_2" 3 ; Examine
          "Task_7" 6}}})




