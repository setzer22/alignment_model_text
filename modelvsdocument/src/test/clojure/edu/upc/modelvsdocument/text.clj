(ns edu.upc.modelvsodument.text-test
  (:require [edu.upc.modelvsdocument.text :refer [mk-text map->Text]]
            [edu.upc.modelvsdocument.textserver :as freeling]
            [edu.upc.modelvsdocument.environment :as environment]
            [edu.upc.modelvsdocument.schemas :as t]
            [clojure.test :refer :all]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]))

;; TEST 1: Ensure that freeling output doesn't change. Note that this test may fail if
;;         freeling version is bumped. In that case, the example output should be updated.
(deftest test-mk-text
  ;; NOTE: Use equals for record-map equality
  (environment/enable-test-environment!)
  (is (.equals (mk-text test-text (freeling/textserver->json (freeling/analyze-cached :text test-text)))
               (map->Text (read-string (slurp "src/test/clojure/edu/upc/modelvsdocument/text_output1.clj"))))))

;; TEST 2: Ensure the output from freeling conforms to the text spec.
(deftest spec-test
  (environment/enable-test-environment!)
  (is (spec/valid?
       ::t/text
       (mk-text test-text (freeling/textserver->json (freeling/analyze-cached :text test-text))))))


;; -------------------------------------------

;; NOTE: Generate test output file

;;(def test-text
  ;;" When a visitor wants to become a member of Barcelona's ZooClub, the following steps must be taken. First of all, the customer must decide whether he wants an individual or family membership. If he wants an individual membership, he must prepare his personal information. If he wants a family membership instead, he should prepare the information for its spouse and spawn as well. The customer must then give this information to the ZooClub department. The ZooClub enters the visitor's personal data into the system and takes the payment request to the Billing department. The ZooClub department also forwards the visitor's information to the marketing department. On receiving the request, the billing department also enters the visitor's personal data into their local database. After that, the billing department sends the payment request to the bank. The bank processes the payment information and, if everything is correct, charges the payment into user's account. Once the payment is confirmed, the ZooClub department can print the card and deliver it to the visitor. In the meantime, the Marketing department makes a request to mail the Zoo Club's magazine to the visitor's home. Once the visitor receives the card, he can go home.")

;;(spit "/tmp/out.clj"
      ;;(with-out-str (clojure.pprint/pprint (mk-text test-text (freeling/textserver->json (freeling/analyze-cached :text test-text))))))
