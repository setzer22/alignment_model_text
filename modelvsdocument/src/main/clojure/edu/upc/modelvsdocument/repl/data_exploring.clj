(ns edu.upc.modelvsdocument.data-exploring
  (:require [defun :refer [defun]]
            [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.utils :refer :all]
            [clojure.inspector :as inspector]
            [clojure.java.io :as io]
            [incanter.core :refer :all]
            [incanter.io :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.datasets :refer :all]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [com.rpl.specter :as s :refer [transform select ALL]]
            [clojure.pprint :as pprint :refer [pprint]]))

(defmacro section [& body]
  `(comment ~@body))

(def text "At the beginning the initial mortgage request of the customer is entered in the bank's system. Afterwards, the bank checks whether they have all the information necessary to process the mortgage request. If not, the bank contacts the customer to ask for the missing information. This step is repeated until all information is complete.

Once the information has been obtained, the bank calculates the available funds for the client, her annual income as well as the required funds in order to buy the property. These computations are all independent of each other and can be done in parallel.

Once all computations are completed the bank queries a central database for additional mortgages the customer might have. If the customer has more than one active mortgage, a rejection letter is sent and the application is closed. This ends the mortgage process. Otherwise, the mortgage application is registered locally. If the customer already has a single active mortgage, the headquarters need to be informed afterwards in addition to registering the mortgage application locally.

In the next stage, the bank performs the following three checks/calculations independently of each other. The bank assesses the mortgage's value compared to the property's value, the applicant's current employment status as well as the applicant's payment history.

After all checks are completed, the mortgage is inspected in detail as described subsequently. If the mortgage is below €1.000.000, a single employee is sufficient making a decision about the mortgage application. For mortgages equal or larger than €1.000.000, a second employee is required for decision making. In the latter case two employees evaluate the mortgage request individually. This is done in parallel. Afterwards they meet to make a decision. If they cannot agree on approving the mortgage, a rejection letter is sent and the application is closed. This ends the mortgage process.

If the mortgage request is approved the bank prepares a mortgage offer for the customer. Then, the bank sends the offer to the customer. Afterwards, the bank evaluates the response forms returned by the customer. If the customer accepts the conditions presented to him in the mortgage offer, the money is made available through a deposit and the mortgage application is closed. This ends the mortgage process. If the customer does not accept the conditions, the bank contacts the customer to inquire for the reasons for not accepting the offer. Afterwards, the response is evaluated by the bank. If the bank decides to offer the customer different conditions for her mortgage request the mortgage request is updated. Then, the mortgage request needs to be evaluated and approved by the bank, as done with the previous request. Otherwise, a letter is sent to the customer and the mortgage application is closed. This ends the process.")

(section "Generate the results"
  (def models-to-run (read-string (slurp "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/nlp-data-eindhoven/models-to-run")))

  (def models-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/nlp-data-eindhoven/bpmn-files/")

  (def results
    (doall (for [process-name models-to-run
                 :let       [model-path (str models-path "/" process-name ".bpmn")
                             __ (println process-name)]]
             (let [result (core/main model-path text)]
               result))))

  (def our-scoring
    (mapify-ordered
     (sort-by second
              (map (fn [name result]
                     [name (.getTotalScore result)])
                   models-to-run results))))

  (spit "/home/josep/our-scoring.spit.clj" our-scoring)
  )




(section "Define the scoring data"
  (def our-scoring (read-string (slurp "/home/josep/our-scoring.spit.clj")))

  (def their-scoring
    (let [scoring-csv-path "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/nlp-data-eindhoven/scores.csv"
          scoring-csv      (mapify-ordered
                            (sort-by
                             second
                             (transform [s/ALL s/LAST]
                                        #(Double/parseDouble %)
                                        (remove #(-> % second empty?)
                                                (rest (csv/read-csv (slurp scoring-csv-path) :separator \;))))))]
      scoring-csv))

  (def top-ten-scoring (take-last 10 their-scoring))

  (def worse-ten-scoring (take 10 their-scoring))

  (pprint our-scoring)
  (pprint their-scoring))

(section "Plot the differences"
  (let [order (map first top-ten-scoring)
        scores (map second top-ten-scoring)]
    (view (bar-chart order scores :x-label "Case" :y-label "Score")))

  (let [order (map first top-ten-scoring)]
    (view (bar-chart order (map #(our-scoring %) order) :x-label "Case" :y-label "Score")))

  (insert-image
   (create-image "/home/josep/tmp/their-scoring.png"))

  (insert-image
   (create-image "/home/josep/tmp/our-scoring.png"))
  )

(section "Averaging the values"
         [[" " "TOP 10" "WORSE 10"]
          ["them" ()]]
         (count their-scoring)

         (def our-ordered-scoring
           (mapify-ordered
            (filter second (map (fn [name] [name (our-scoring name)])
                  (map first their-scoring)))))

         (map mean (partition 20 (map second their-scoring)))

         (map mean (partition 20 (map second our-ordered-scoring)))


         (mean (map second top-ten-scoring))

         (mean (map second worse-ten-scoring))


         )
