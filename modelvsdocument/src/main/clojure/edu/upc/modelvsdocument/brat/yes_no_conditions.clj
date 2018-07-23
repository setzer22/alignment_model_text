(ns edu.upc.modelvsdocument.brat.yes-no-conditions
  (:require [edu.upc.modelvsdocument.brat.specs :as brt]))

(defrecord YesNoCondition [id condition-words yes-words no-words])

;;WIL: The order of YesBranch and NoBranch is swapped
;;WIL: What does it mean that a condition has multiple branches of the same type?
;;WIL: The graph should not be directed for symmetric relations

#_(pipeline/defpipe-step compute-yes-no-conditions [enriched-anns analyzed-text relation-graph]
    {:yes-no-conditions
     (map
      (fn [yes-no-cd]
        (let [yes-branch (adj-with-type "YesBranch" relation-graph yes-no-cd)
              no-branch (adj-with-type "NoBranch" relation-graph yes-no-cd)]
          (->YesNoCondition (get-tokens-in-ann-range analyzed-text yes-no-cd)
                            (when yes-branch
                              (get-tokens-in-ann-range analyzed-text yes-branch))
                            (when no-branch
                              (get-tokens-in-ann-range analyzed-text no-branch)))))
      (filter #(= (:type %) "YesNoCondition") (::brt/tag enriched-anns)))})
