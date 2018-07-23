(ns edu.upc.modelvsdocument.bpmn-analysis-old
  (:require [com.rpl.specter :refer :all]
            [clojure.pprint :refer :all]
            [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [clojure.string :as string]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.utils :as utils]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.bpmn :as bpmn]))

(spec-fn capitalize string?)
(defn capitalize
  [sentence]
  (let [characters
        (loop [characters (seq sentence)
               new-characters []
               first true]
          (if characters
            (let [[acronym [x & xs]] (split-while #(Character/isUpperCase %) characters)
                  acronym (if (or first (> (count acronym) 1)) acronym (map string/lower-case acronym))
                  new-characters (apply conj new-characters acronym)
                  new-characters (if x (conj new-characters (string/lower-case x))
                                     new-characters)]
              (recur xs new-characters false))
            new-characters))]
    (apply str characters)))

(spec-fn analyze-labels (spec/* string?), string?)
(defn analyze-labels
  [labels context-text]
  (let [text (string/join " .\n\n" labels)
        full-text (str text " .\n\n" context-text)
        positions (concat (reductions + (reduce #(conj %1 (+ 4 (count %2))) [] (butlast labels))) [(count text)])
        parsed (textserver/textserver->json
                (textserver/analyze-cached :text full-text :lang "en" :output "json" :level "dep"))
        sentences (select [:paragraphs ALL :sentences ALL] parsed)
        get-begin (fn [sentence] (Integer/parseInt (select-one [:tokens FIRST :begin] sentence)))
        text-info (loop [sentences sentences,
                         [end* & p-rest] positions,
                         text-info []]
                    (if end*
                      (let [this-label-sentences (into [] (take-while #(< (get-begin %) end*)) sentences)
                            remaining-sentences (drop-while #(< (get-begin %) end*) sentences)]
                        (recur remaining-sentences
                               p-rest
                               (conj text-info {:sentences this-label-sentences})))
                      text-info))
        text-info (apply assoc0 text-info (mapcat vector (indices-such-that empty? labels) (repeat nil)))]
    text-info))

(spec-fn analyze-model-text ::t/bpmn string?)
(defn analyze-model-text [model, context-text]
  (let [element-ids    (bpmn/all-element-ids model)
        element-labels (map #(capitalize (bpmn/get-label model %)) element-ids)

        labels (concat element-labels (bpmn/all-pool-labels model) (bpmn/all-lane-labels model))
        ids    (concat element-ids (bpmn/all-pool-ids model) (bpmn/all-lane-ids model))

        text-info       (analyze-labels labels context-text)
        analyzed-labels (mapify (zip ids text-info))]
    (assoc model :analyzed-labels analyzed-labels)))

(comment 
  "OLD VERSION"


  (spec-fn missing-lane-text string?)
  (defn missing-lane-text [lane-id]
    (.toString (hash lane-id)))

  (spec-fn partition-count seq? (spec/* int?))
  (defn partition-count [lst & counts]
    (second (reduce
             (fn [[count-so-far sublists-so-far] next-count]
               [(+ count-so-far next-count)
                (conj sublists-so-far (subvec lst count-so-far (+ count-so-far next-count)))])
             [0, []]
             counts)))

  (spec-fn analyze-model-text ::t/bpmn string?)
  (defn analyze-model-text [model-struct, text]
   (let [tasks          (bpmn/all-tasks model-struct)
         named-gateways (remove #(empty? (.getName %)) (bpmn/all-gateways model-struct))
         lanes          (bpmn/all-lanes model-struct)
         pools          (bpmn/all-pools model-struct)

                                        ; All tasks, names and pools should have a valid name. We fail otherwise.
                                        ; NOTE: We could tolerate this the same way we do with gateways, but that
                                        ;       would mean restructuring the extraction code since we can't assume
                                        ;       this anymore
                                        ;__ (when-not (every? #(not (empty? (.getName %))) pools)
                                        ;(throw (Exception. "ERROR: BPMN Model is malformed, all pools should have a name.")))
                                        ;__ (when-not (every? #(not (empty? (.getName %))) lanes)
                                        ;(throw (Exception. "ERROR: BPMN Model is malformed, all lanes should have a name.")))
         __ (when-not (every? #(not (empty? (.getName %))) tasks)
              (throw (Exception. "ERROR: BPMN Model is malformed, all tasks should have a name.")))

                                        ;TODO: Temporary patch so experiment 1 works. We will replace ALL missing pool names by some string.
                                        ;      Note the string will be generated deterministically so the result can be cached.
         pools (map #(if (empty? (.getName %))
                       (do (.setName % (str "pool" (missing-lane-text (.getId %)))) %)
                       %) pools)
         lanes (map #(if (empty? (.getName %))
                       (do (.setName % (str "lane" (missing-lane-text (.getId %)))) %)
                       %) lanes)

         [T G L P]      [(count tasks) (count named-gateways) (count lanes) (count pools)]

                                        ; NOTE: The " .\n\n " Sepparator should make the sentences in tasks always become separated.
         model-txt      (string/join " .\n\n " (concat
                                                (map #(capitalize (.getName %)) tasks)
                                                (map #(capitalize (.getName %)) named-gateways)
                                                (map #(capitalize (.getName %)) lanes)
                                                (map #(capitalize (.getName %)) pools)))
         full-text      (str text ".\n " model-txt ".")
         textserver-response
         (textserver/analyze-cached :text full-text, :lang "en", :level "tagged")
         model-res      (textserver/textserver->json textserver-response)
         dummy-count    (+ T G L P)

                                        ; Remove the text sentences from the analyzed model text
         dummy-ids      (mapv :id (take-last dummy-count (select [:paragraphs ALL :sentences ALL] model-res)))

         partition-count (fn partition-count [lst & counts]
                           (second (reduce
                                    (fn [[count-so-far sublists-so-far] next-count]
                                      [(+ count-so-far next-count)
                                       (conj sublists-so-far (subvec lst count-so-far (+ count-so-far next-count)))])
                                    [0, []]
                                    counts)))

                                        ; Get the ids from the analyzed text
                                        ; We create a new paragraph with only the sentences from the model
         all-sentences  (select [:paragraphs ALL :sentences ALL] model-res)
                                        ;__ (map #(map :lemma %) (select [ALL :tokens] all-sentences))
                                        ;__ (count all-sentences)

         [text-sentences, task-sentences, gateway-sentences, lane-sentences, pool-sentences]
         (partition-count all-sentences (- (count all-sentences) (+ T G L P)) T G L P)

                                        ;__ (count text-sentences)
                                        ;__ (count task-sentences)
                                        ;__ (count gateway-sentences)
                                        ;__ (count lane-sentences)
                                        ;__ (count pool-sentences)

         good-sentences (concat
                         (map #(assoc %1 :task-id (.getId %2)) task-sentences tasks)
                         (map #(assoc %1 :gateway-id (.getId %2)) gateway-sentences named-gateways)
                         (map #(assoc %1 :lane-id (.getId %2)) lane-sentences lanes)
                         (map #(assoc %1 :pool-id (.getId %2)) pool-sentences pools))

         new-paragraph  {:sentences good-sentences}
         model-an       (assoc model-res :paragraphs [new-paragraph])]

     model-an)))
