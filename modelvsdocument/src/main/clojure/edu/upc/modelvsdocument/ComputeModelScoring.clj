(ns edu.upc.modelvsdocument.ComputeModelScoring
  (:require [edu.upc.modelvsdocument.core :as core]
            [edu.upc.modelvsdocument.config :as config :refer [config with-config]]
            [edu.upc.modelvsdocument.wordnet :as wordnet]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.solver :as solver]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.nlp4bpm-commons.freeling-api :as freeling]
            [edu.upc.modelvsdocument.utils :as utils :refer :all])
  (:gen-class
   :main true))

(def text "At the beginning the initial mortgage request of the customer is entered in the bank's system. Afterwards, the bank checks whether they have all the information necessary to process the mortgage request. If not, the bank contacts the customer to ask for the missing information. This step is repeated until all information is complete.

Once the information has been obtained, the bank calculates the available funds for the client, her annual income as well as the required funds in order to buy the property. These computations are all independent of each other and can be done in parallel.

Once all computations are completed the bank queries a central database for additional mortgages the customer might have. If the customer has more than one active mortgage, a rejection letter is sent and the application is closed. This ends the mortgage process. Otherwise, the mortgage application is registered locally. If the customer already has a single active mortgage, the headquarters need to be informed afterwards in addition to registering the mortgage application locally.

In the next stage, the bank performs the following three checks/calculations independently of each other. The bank assesses the mortgage's value compared to the property's value, the applicant's current employment status as well as the applicant's payment history.

After all checks are completed, the mortgage is inspected in detail as described subsequently. If the mortgage is below €1.000.000, a single employee is sufficient making a decision about the mortgage application. For mortgages equal or larger than €1.000.000, a second employee is required for decision making. In the latter case two employees evaluate the mortgage request individually. This is done in parallel. Afterwards they meet to make a decision. If they cannot agree on approving the mortgage, a rejection letter is sent and the application is closed. This ends the mortgage process.

If the mortgage request is approved the bank prepares a mortgage offer for the customer. Then, the bank sends the offer to the customer. Afterwards, the bank evaluates the response forms returned by the customer. If the customer accepts the conditions presented to him in the mortgage offer, the money is made available through a deposit and the mortgage application is closed. This ends the mortgage process. If the customer does not accept the conditions, the bank contacts the customer to inquire for the reasons for not accepting the offer. Afterwards, the response is evaluated by the bank. If the bank decides to offer the customer different conditions for her mortgage request the mortgage request is updated. Then, the mortgage request needs to be evaluated and approved by the bank, as done with the previous request. Otherwise, a letter is sent to the customer and the mortgage application is closed. This ends the process.")

(defn file-in-path-exists? [path]
  (and (string? path)
       (.exists (clojure.java.io/file path))))

;;@CopyPasted
(defn -main [config-path credentials-path & _]
  (try
    (freeling/set-credentials-path! credentials-path)
    (config/override-config-from-file config-path)
    (when (:enable-cache config)
      (let [cache-path (:cache-path config)
            cache-file (clojure.java.io/file cache-path)]
        (if (.exists (.getParentFile cache-file))
          (cache/initialize cache-path)
          (println "WARNING: Cache path" cache-path " doesn't exist. Proceeding without cache."))))
    (cond (file-in-path-exists? (:wordnet-path config))
          ,,(wordnet/read-wordnet-dictionaries (:wordnet-path config))
          (file-in-path-exists? (System/getenv "NLP4BPM_WORDNET_PATH"))
          ,,(wordnet/read-wordnet-dictionaries (System/getenv "NLP4BPM_WORDNET_PATH"))
          :else
          ,,(throw (Exception. "Wordnet path not specified in the config file or NLP4BPM_WORDNET_PATH variable. Please set this path pointing to the wordnet folder in the installation directory.")))
    ;;Main Logic
    (let [models-path (:models-path config)
          models-to-run (read-string (slurp (:models-to-run-path config)))
          sentence-order (read-string (slurp (:sentence-order-path config)))
          output-file (:output-file config)
          gurobi-models-folder (:gurobi-models-folder config)]
      (with-config {:gurobi-tmp-path gurobi-models-folder}
        (doall (for [process-name models-to-run
                     :let       [model-path (str models-path "/" process-name ".bpmn")
                                 model (bpmn/build-model (bpmn/read-model model-path))
                                 __ (println "Start processing" process-name)]]
                 (let [result (try (.getTotalScore (core/main model-path text sentence-order))
                                   (catch Throwable e -0.0314))]
                   (spit output-file [model-path result] :append true))))))
    ;;----------
    (if (config :enable-cache) (cache/saveCache))
    (catch Throwable e
      (println "There was an error in the execution of the program. Please contact the authors. INFO:" (.getMessage e)))))

