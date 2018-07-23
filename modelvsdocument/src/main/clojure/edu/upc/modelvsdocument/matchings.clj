(ns edu.upc.modelvsdocument.matchings
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as spec-test]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clojure.set :as set]
            [com.rpl.specter :as specter]
            [edu.upc.modelvsdocument.bpmn :as bpmn]))

(def task-id? string?)
(def sentence-id? integer?)
(spec/def ::no-task #(= % ::no-task))
(spec/def ::sentence->task (spec/map-of sentence-id?
                                        (spec/or
                                         :no-match ::no-task
                                         :match (spec/coll-of task-id?))))
(spec/def ::no-sentence #(= % ::no-sentence))
(spec/def ::task->sentence (spec/map-of task-id?
                                        (spec/or :match sentence-id?
                                                 :no-match ::no-sentence)))
(spec/def ::tagged-case (spec/keys :req [::sentence->task ::task->sentence ::model]))
(spec/def ::tagged-cases (spec/map-of string? ::tagged-case))
(def tagged-cases
    {"Dispatch-of-goods"
     {::sentence->task
      {1 ["Task_0vaxgaa"]
       2 ::no-task
       3 ["Task_0e6hvnj" "Task_0s79ile"]
       4 ::no-task
       5 ["Task_0jsoxba" "Task_12j0pib"]
       6 ["Task_05ftug5"]
       7 ["Task_0sl26uo"]}
      ::task->sentence
      {"Task_0vaxgaa" 1
       "Task_0e6hvnj" 3
       "Task_0s79ile" 3
       "Task_0jsoxba" 5
       "Task_12j0pib" 5
       "Task_05ftug5" 6
       "Task_0sl26uo" 7}
      }

     "Computer_Repair"
     {::sentence->task
      {1 ::no-task
       2 ["Task_0utq0n4" "Task_189p4oe"]
       3 ["Task_1ff8ap0" "Task_1nsxwm3"]
       4 ::no-task
       5 ["Task_1dwkm6f" "Task_19igqv8"]
       6 ["Task_0gckyo9"]
       7 ::no-task}
      ::task->sentence
      {"Task_0utq0n4" 2
       "Task_189p4oe" 2
       "Task_1ff8ap0" 3
       "Task_1nsxwm3" 3
       "Task_1dwkm6f" 4
       "Task_19igqv8" 5
       "Task_0gckyo9" 6
       "Task_1ixc0ik" ::no-sentence}
      }

     "Bicycle_Manufacturer"
     {::sentence->task
      {1 ::no-task
       2 ["Task_1ii2rs5"]
       3 ::no-task
       4 ::no-task
       5 ["Task_1evwa99"]
       6 ["Task_1utuu2x"]
       7 ["Task_03h44gw" "Task_0hsiz61"]
       8 ::no-task
       9 ["Task_0k4jzru"]
       10 ["Task_0qhgw0l"]
       11 ["Task_1xckyo7"]}
      ::task->sentence
      {"Task_1ceh3k4" ::no-sentence
       "Task_1ii2rs5" 2
       "Task_1evwa99" 5
       "Task_1utuu2x" 6
       "Task_03h44gw" 7
       "Task_0hsiz61" 7
       "Task_0k4jzru" 9
       "Task_0qhgw0l" 10
       "Task_1xckyo7" 11}
      }

     "Hospital"
     {::sentence->task
      {1 ::no-task
       2 ["Task1"]
       3 ["Task4"]
       4 ["Task2"]
       5 ::no-task
       6 ["Task3"]
       7 ["Task5"]
       8 ["Task7"]
       9 ["Task6"] ;; Order inversion! sentences 8 and 9
       10 ["Task7"]
       11 ["Task8" "Task9"]
       12 ["Task10"]
       13 ["Task11" "Task12"]
       14 ["Task13" "Task14"]} 
      ::task->sentence 
      {"Task1" 2
       "Task2" 4
       "Task3" 6
       "Task4" 3
       "Task5" 7
       "Task6" 9
       "Task7" 8 ;; Order inversion! sentences 8 and 9
       "Task8" 11
       "Task9" 11
       "Task10" 12
       "Task11" 13
       "Task12" 13
       "Task13" 14
       "Task14" 14}
      }

     "Hotel"
     {::sentence->task
      {1 ::no-task
       2 ["Task_19oznyv"]
       3 ["Task_16m28ah" "Task_0mymppr"]
       4 ["Task_1yfp1gq" "Task_0c685de" "Task_1y44uft"]
       5 ::no-task
       6 ["Task_1esbsdw"]
       7 ["Task_0z390gs"]
       8 ["Task_05jr8ik"]
       9 ["Task_12rew8t"]
       10 ["Task_0h8ljxx" "Task_0vkxpbk"]
       11 ::no-task}
      ::task->sentence
      {"Task_19oznyv" 2
       "Task_16m28ah" 3
       "Task_0mymppr" 3
       "Task_1yfp1gq" 4
       "Task_0c685de" 4
       "Task_1y44uft" 4
       "Task_1esbsdw" 6
       "Task_0z390gs" 7
       "Task_05jr8ik" 8
       "Task_12rew8t" 9
       "Task_0h8ljxx" 10
       "Task_0vkxpbk" 10}
      }

"Recourse"
{::sentence->task
 {1 ::no-task
  2 ["Task_0iirfhd" "Task_02fdytg" "Task_12lthpj"]
  3 ["Task_04aofbe"]
  4 ["Task_1bwmf45" "Task_0yan60f"]
  5 ["Task_0eti3m2"]
  6 ["Task_1qlbv5i"]
  7 ["Task_1w7bb1w"]
  8 ::no-task ;;This is actually "Task_1w8bb1w, but that would break one of our assumptions"
  }
 ::task->sentence
 {"Task_0iirfhd" 2
  "Task_02fdytg" 2
  "Task_12lthpj" 2
  "Task_04aofbe" 3
  "Task_1bwmf45" 4
  "Task_0yan60f" 4
  "Task_0eti3m2" 5
  "Task_1qlbv5i" 6
  "Task_1w7bb1w" 7 ;; see above
  }
 }

"Self-service-restaurant"
{::sentence->task 
 {1 [ "Task_1kt8dzo"]
  2 [ "Task_1udyby3"]
  3 [ "Task_0a2rm9v"]
  4 [ "Task_1y7mm27" "Task_1ng51gy" "Task_12h2fs9"]
  5 [ "Task_03wsmkw" "Task_0ttgn0d" "Task_1jlgbwe"]
  6 [ "Task_1wgretj"] 
  7 [ "Task_0av6xl6" "Task_1tdsk5o"]
  8 [ "Task_0ot5dif"]
  9 [ "Task_12zp7cy"]
  10 ::no-task
  11 [ "Task_0o0pue9" "Task_07amhtq"] 
  12 [ "Task_1a48xz1"]
  13 [ "Task_0rpvccw"]}
 ::task->sentence 
 {"Task_1kt8dzo" 1
  "Task_1udyby3" 2
  "Task_0a2rm9v" 3
  "Task_1y7mm27" 4
  "Task_1ng51gy" 4
  "Task_12h2fs9" 4
  "Task_03wsmkw" 5
  "Task_0ttgn0d" 5
  "Task_1jlgbwe" 5
  "Task_1wgretj" 6
  "Task_0av6xl6" 7
  "Task_1tdsk5o" 7
  "Task_0ot5dif" 8
  "Task_12zp7cy" 9
  "Task_0o0pue9" 11
  "Task_07amhtq" 11 
  "Task_1a48xz1" 12
  "Task_0rpvccw" 13}
 }

"Underwriter"
{::sentence->task 
 {1 ::no-task
  2 ::no-task
  3 ::no-task
  4 ["Task_08320qz"]
  5 ::no-task
  6 ["Task_0z891iy"]
  7 ["Task_0plj3yi"]
  8 ::no-task
  9 ["Task_1usz0ic"]
  10 ["Task_0a6dqnu"]
  11 ["Task_0xpzfes" "Task_0wjw0ga"]}
 ::task->sentence
 {"Task_08320qz" 4
  "Task_0z891iy" 6
  "Task_0plj3yi" 7
  "Task_1usz0ic" 9
  "Task_0a6dqnu" 10
  "Task_0xpzfes" 11
  "Task_0wjw0ga" 11}
 }

"Zoo"
{::sentence->task 
 {1 ::no-task
  2 ["Task_17x20z3"]
  3 ["Task_064spq4"]
  4 ["Task_14zetua"]
  5 ["Task_0wrm9ya"]
  6 ["Task_0qrzi4y" "Task_106edn4"]
  7 ["Task_1gst9f9"]
  8 ["Task_0oih0sg"]
  9 ["Task_0420rk1"]
  10 ["Task_0oz8hjr" "Task_02qz4bj"]
  11 ["Task_0j4npun"]
  12 ["Task_0ssja9k"]}
 ::task->sentence
 {"Task_17x20z3" 2
  "Task_064spq4" 3
  "Task_14zetua" 4
  "Task_0wrm9ya" 5
  "Task_0qrzi4y" 6
  "Task_106edn4" 6
  "Task_1gst9f9" 7
  "Task_0oih0sg" 8 
  "Task_0420rk1" 9
  "Task_0oz8hjr" 10
  "Task_02qz4bj" 10
  "Task_0j4npun" 11
  "Task_0ssja9k" 12
  "Task_0w3czw9" ::no-sentence
  "Task_1qrcrh7" ::no-sentence}
 }})
