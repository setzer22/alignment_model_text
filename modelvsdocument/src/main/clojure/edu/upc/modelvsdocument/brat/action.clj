(ns edu.upc.modelvsdocument.brat.action
  (:require [clj-pipeline.core :as pipeline]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.brat.utils :refer :all]
            [edu.upc.modelvsdocument.brat.specs :as brt]
            [clojure.set :as set]
            [clojure.string :as str]))

(defrecord Action [id, position, words, object, role, optional?, end?])

(pipeline/defpipe-step compute-actions [analyzed-text enriched-anns relation-graph attrs id->Role]
  (let [id->tag (zipmap (map :id (::brt/tag enriched-anns))
                        (::brt/tag enriched-anns))
        actions (map
                 (fn [brat-action]
                   (let [[role-tag-id] (adj-with-type "HasRole" relation-graph (:id brat-action))
                         [object-tag-id] (adj-with-type "HasObject" relation-graph (:id brat-action))]
                     (->Action
                      (:id brat-action)
                      (:start brat-action)
                      (get-tokens-in-ann-range analyzed-text brat-action)
                      (get id->Role object-tag-id nil)
                      (get id->Role role-tag-id nil)
                      (= "Yes" (get-in attrs ["OptionalAction" (:id brat-action)]))
                      (= "Yes" (get-in attrs ["IsEndEvent" (:id brat-action)])))))
                 (filter #(= (:type %) "Action") (::brt/tag enriched-anns)))]
    {:actions actions}))

(defn action-str [{:keys [id words object role]}]
  (with-out-str
    (print "Action(")
    (print (str "Id:" id ", "))
    (print (str "Agent:" (try-or (str/join " " (map #(-> % :form) (:main-mention-words role))) "") ", "))
    (print (str "Verb:" (try-or (str/join " " (map #(-> % :form) words)) "") ", "))
    (print (str "Object:" (try-or (str/join " " (map #(-> % :form) (:main-mention-words object))) "") ")"))))
