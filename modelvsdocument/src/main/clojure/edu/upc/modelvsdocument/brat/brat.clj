(ns edu.upc.modelvsdocument.brat.brat
  (:require [edu.upc.modelvsdocument.textserver :as freeling]
            [clj-pipeline.core :as pipeline]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.brat.utils :refer :all]
            [edu.upc.modelvsdocument.brat.parser :refer :all]
            [edu.upc.modelvsdocument.brat.relations :refer :all]
            [edu.upc.modelvsdocument.brat.action :refer :all]
            [edu.upc.modelvsdocument.brat.role :refer :all]
            [edu.upc.modelvsdocument.brat.yes-no-conditions :refer :all]
            [edu.upc.modelvsdocument.brat.control-flow :refer :all]
            [edu.upc.modelvsdocument.brat.specs :as brt]
            [clojure.set :as set])
  (:import [clojure.lang PersistentVector]))

(pipeline/defpipe-input read-input [ann-path-or-file text-path-or-file]
  {:data (line-seq (io/reader ann-path-or-file))
   :original-text (slurp text-path-or-file)})

(pipeline/defpipe-step analyze-text [original-text]
  {:analyzed-text (freeling/textserver->json
                   (freeling/analyze-cached :text original-text :level "tagged"))})

(pipeline/defpipe-step classify-anns [anns]
  {:classified-anns (reduce
                     (fn [classified ann]
                       (update classified (type-key ann) conj ann))
                     (zipmap brat-types (repeat []))
                     anns)})

(pipeline/defpipe-step classify-attrs [classified-anns]
  {:attrs (->> (::brt/attribute classified-anns)
               (group-by :type)
               (transform MAP-VALS #(group-by :referenced-tag %))
               (transform [MAP-VALS MAP-VALS] (comp :value first)))})

(pipeline/defpipe-step enrich-tags [classified-anns analyzed-text original-text]
  {:enriched-anns (transform
                   [::brt/tag ALL]
                   (fn [tag]
                     (enrich-Tag original-text analyzed-text tag))
                   classified-anns)})

(pipeline/defpipe-output output [enriched-anns relation-graph actions #_yes-no-conditions roles id->Role attrs flows]
  (pipeline/env enriched-anns relation-graph actions flows))

(defn parse-brat-file [ann-file-or-path text-file-or-path]
  (pipeline/run-pipeline
   [ann-file-or-path text-file-or-path]
   read-input
   parse-lines
   classify-anns
   classify-attrs
   analyze-text
   enrich-tags
   build-relation-graph
   compute-roles
   compute-actions
   #_compute-yes-no-conditions
   compute-flows
   output))


(comment
  (def --text (slurp "/home/josep/Hospital.txt"))

  (def --an (freeling/textserver->json (freeling/analyze-cached :text --text
                                                                :level "tagged")))

  (select [:paragraphs ALL :sentences ALL :tokens ALL #(= (:form %) "asked")] --an)

  (def --parsed-brat-file (parse-brat-file "/home/josep/Hospital.ann"
                                           "/home/josep/Hospital.txt"))

  --parsed-brat-file
  

  (let [rels (:flows --parsed-brat-file)
        #_(apply merge-with
                 (fn [m1 m2]
                   (merge-with #(if (= %1 %2) %1 :??) m1 m2))
                 (:flows --parsed-brat-file))]
    (copy-to-clipboard!
     (with-out-str
       (doseq [a1 (map :id (:actions --parsed-brat-file))]
         (doseq [a2 (map :id (:actions --parsed-brat-file))]
           (print (str (get-in rels [a1 a2] :--) " ")))
         (println "")))))


  (doseq [action (:actions --parsed-brat-file)]
    (println (:id action) "; " (action-str action))))

  

