(ns edu.upc.modelvsdocument.text
  (:require [edu.upc.modelvsdocument.utils :refer :all]
            [com.rpl.specter :as specter :refer :all]
            [edu.upc.modelvsdocument.alignable :as alignable]
            [edu.upc.modelvsdocument.textserver :as freeling]
            [edu.upc.modelvsdocument.text-preprocess :as preproc]
            [edu.upc.modelvsdocument.config :as globals]))

(defrecord Text [alignable-map paragraphs semantic_graph text dummy]
  alignable/Context
  (get-element [this id] (if-not (= id "-1")
                           (get (:alignable-map this) id)
                           (:dummy this)))
  (get-all-alignables [this] (if (globals/config :enable-anchors)
                               (conj (vals (:alignable-map this)) (:dummy this))
                               (vals (:alignable-map this))))
  (get-output-alignables [this] (alignable/get-all-alignables this)))

(defrecord Sentence [id label]
  alignable/Alignable
  (id [this] (:id this))
  (label [this] (:label this)))

(defrecord DummySentence []
  alignable/Alignable
  (id [this] "-1")
  (label [this] "DUMMY"))

(defn mk-dummy [] (->DummySentence))

(defn sentence? [x]
  (instance? Sentence x))

(defn read-from-file [path]
  (as-> (map->Text (read-string (slurp path))) $$
    (transform [:alignable-map MAP-VALS]
               #(map->Sentence %) $$)
    (assoc $$ :dummy (mk-dummy))))

(defn dummy? [x]
  (instance? DummySentence x))

(defn mk-text [original-text json]
  (let [sentences (select [:paragraphs ALL :sentences ALL] json)
        ids (map :id sentences)
        labels (map (fn [sentence] (subs original-text
                                         (Integer/parseInt (select-one [:tokens FIRST :begin] sentence))
                                         (Integer/parseInt (select-one [:tokens LAST :end] sentence))))
                    sentences)
        sentences (map (fn [id label] (->Sentence id label)) ids labels)
        alignable-map (apply sorted-map-by
                             ;; Custom comparator used so ids are sorted intuitively.
                             ;; This is not needed but is very useful for debugging
                             #(< (Integer/parseInt %1)
                                 (Integer/parseInt %2) )
                             (mapcat vector ids sentences))]
    (map->Text {:text original-text
                :paragraphs (:paragraphs json)
                :semantic_graph (:semantic_graph json)
                :alignable-map alignable-map
                :dummy (mk-dummy)})))

(defn analyze [& {:keys [text lang resolve-anaphoras]
                  :or {lang "en", resolve-anaphoras true}}]
  (let [json (freeling/textserver->json (freeling/analyze-cached :text text))
        json (if resolve-anaphoras (preproc/resolve-anaphoras json) json)]
    (mk-text text json)))

(comment
  (analyze :text (slurp "/home/josep/ModelsBpmn/Zoo.txt") :lang "en")

  ((freeling/analyze-cached :text (slurp "/home/josep/ModelsBpmn/Zoo.txt")))

  (edu.upc.nlp4bpm-commons.cache/saveCache )

  (def --text *1)

  (alignable/get-all-alignables --text )

                                        ;(def --text edu.upc.modelvsdocument.repl.predicates/text1)

  (alter-var-root #'edu.upc.modelvsdocument.config/config assoc :enable-cache true)

  (alter-var-root #'edu.upc.modelvsdocument.config/config assoc :use-gurobi true)

  (edu.upc.nlp4bpm-commons.cache/initialize "/home/josep/.textserver-cache.clj.old")
  (edu.upc.nlp4bpm-commons.freeling-api/set-mode "local")

  (map alignable/human-readable
       (alignable/get-all-alignables (mk-text (slurp "/home/josep/ModelsBpmn/Zoo.txt") --text)))

  )
