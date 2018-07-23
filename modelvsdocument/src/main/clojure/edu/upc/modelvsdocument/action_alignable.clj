(ns edu.upc.modelvsdocument.action-alignable
  (:require [edu.upc.modelvsdocument.utils :refer :all]
            [com.rpl.specter :as specter :refer :all]
            [edu.upc.modelvsdocument.alignable :as alignable]
            [edu.upc.modelvsdocument.textserver :as freeling]
            [edu.upc.modelvsdocument.text-preprocess :as preproc]
            [edu.upc.modelvsdocument.brat.brat :as brat]
            [edu.upc.modelvsdocument.config :as globals]))

(defrecord AnnotatedText [alignable-map annotations relation-graph paragraphs semantic_graph text]
  alignable/Context
  (get-element [this id] (get alignable-map id))
  (get-all-alignables [this] (vals (:alignable-map this)))
  (get-output-alignables [this] (alignable/get-all-alignables this)))


;;WIL: This should be really handled in the brat.clj namespace. The key idea is to abstract away from brat's format, so what we will get out of a brat file is a list of "Predicate" objects, each with an "Action" and optionally "Role" and "Object". We will also need to extract other information such as "Roles" (as a separate entity, not tied to an action), "Conditions", ...



(comment

  brat/--parsed-brat-file

  (brat/adj-with-type "HasRole" (:relation-graph brat/--parsed-brat-file)
                      (first (::brat/tag (:enriched-anns brat/--parsed-brat-file))))

  (def --node (first (::brat/tag (:enriched-anns brat/--parsed-brat-file))))

  (get (:relation-graph brat/--parsed-brat-file) "T1")



  (map alignable/label
       (mk-anntext (slurp "/home/josep/Hospital.txt")
                   brat/--an
                   (:enriched-anns brat/--parsed-brat-file)
                   (:relation-graph brat/--parsed-brat-file)))

  )
