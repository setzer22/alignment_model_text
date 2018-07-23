(ns edu.upc.modelvsdocument.brat.relations
  (:require [edu.upc.modelvsdocument.textserver :as freeling]
            [clj-pipeline.core :as pipeline]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.brat.utils :refer :all]
            [edu.upc.modelvsdocument.brat.specs :as brt]
            [clojure.set :as set]))

(def symmetric?
  "The set of symmetric relations."
  #{"CoreferencedWith"})

(pipeline/defpipe-step build-relation-graph [classified-anns]
  {:relation-graph
   (reduce
    (fn [graphs {:keys [type src-id dst-id]}]
      (cond-> graphs
        true (update-in [type src-id] conj dst-id)
        (symmetric? type) (update-in [type dst-id] conj src-id)))
    {}
    (::brt/relation classified-anns))})

