(ns edu.upc.modelvsdocument.repl.predicates
  (:require [edu.upc.nlp4bpm-commons.Freeling :as freeling]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [com.rpl.specter :as specter :refer :all]
            [clojure.data.csv :as csv]))

(defonce --auto-1309184124-test-1p044340124 ; A symbol we won't re-use
  (do (clojure.lang.RT/loadLibrary "freeling_javaAPI")
      (edu.upc.freeling.Util/initLocale "default")))

(defonce --semdb (edu.upc.freeling.SemanticDB. "/usr/local/share/freeling/en/semdb.dat"))

(def text1 (textserver/textserver->json
            (textserver/analyze-with-textserver :text (slurp "/home/josep/ModelsBpmn/Zoo.txt"))))

(def ALL-PREDICATES [:paragraphs ALL :sentences ALL :predicates ALL])

(def ALL-TOKENS [:paragraphs ALL :sentences ALL :tokens ALL])

(defn StringList->seq [s]
  "Converts a StringList object to a regular seq. The original object is destroyed"
  (if (pos? (.size s))
    (lazy-seq
     (cons (.getFirst s)
           (do (.clearFirst s)
               (StringList->seq s))))
    nil))

(def semfile->description
  {"00"    "adj.all (all adjective clusters)"
   "01"    "adj.pert (relational adjectives (pertainyms))"
   "02"    "adv.all (all adverbs)"
   "03"    "noun.Tops (unique beginner for nouns)"
   "04"    "noun.act (nouns denoting acts or actions)"
   "05"    "noun.animal (nouns denoting animals)"
   "06"    "noun.artifact (nouns denoting man-made objects)"
   "07"    "noun.attribute (nouns denoting attributes of people and objects)"
   "08"    "noun.body (nouns denoting body parts)"
   "09"    "noun.cognition (nouns denoting cognitive processes and contents)"
   "10"    "noun.communication (nouns denoting communicative processes and contents)"
   "11"    "noun.event (nouns denoting natural events)"
   "12"    "noun.feeling (nouns denoting feelings and emotions)"
   "13"    "noun.food (nouns denoting foods and drinks)"
   "14"    "noun.group (nouns denoting groupings of people or objects)"
   "15"    "noun.location (nouns denoting spatial position)"
   "16"    "noun.motive (nouns denoting goals)"
   "17"    "noun.object (nouns denoting natural objects (not man-made))"
   "18"    "noun.person (nouns denoting people)"
   "19"    "noun.phenomenon (nouns denoting natural phenomena)"
   "20"    "noun.plant (nouns denoting plants)"
   "21"    "noun.possession (nouns denoting possession and transfer of possession)"
   "22"    "noun.process (nouns denoting natural processes)"
   "23"    "noun.quantity (nouns denoting quantities and units of measure)"
   "24"    "noun.relation (nouns denoting relations between people or things or ideas)"
   "25"    "noun.shape (nouns denoting two and three dimensional shapes)"
   "26"    "noun.state (nouns denoting stable states of affairs)"
   "27"    "noun.substance (nouns denoting substances)"
   "28"    "noun.time (nouns denoting time and temporal relations)"
   "29"    "verb.body (verbs of grooming, dressing and bodily care)"
   "30"    "verb.change (verbs of size, temperature change, intensifying, etc.)"
   "31"    "verb.cognition (verbs of thinking, judging, analyzing, doubting)"
   "32"    "verb.communication (verbs of telling, asking, ordering, singing)"
   "33"    "verb.competition (verbs of fighting, athletic activities)"
   "34"    "verb.consumption (verbs of eating and drinking)"
   "35"    "verb.contact (verbs of touching, hitting, tying, digging)"
   "36"    "verb.creation (verbs of sewing, baking, painting, performing)"
   "37"    "verb.emotion (verbs of feeling)"
   "38"    "verb.motion (verbs of walking, flying, swimming)"
   "39"    "verb.perception (verbs of seeing, hearing, feeling)"
   "40"    "verb.possession (verbs of buying, selling, owning)"
   "41"    "verb.social (verbs of political and social activities and events)"
   "42"    "verb.stative (verbs of being, having, spatial relations)"
   "43"    "verb.weather (verbs of raining, snowing, thawing, thundering)"
   "44"    "adj.ppl (participial adjectives)"})

(defn sense-info [wn]
  (let [sense-info (.getSenseInfo --semdb wn)]
    {:wn wn
     :parents (StringList->seq (.getParents sense-info))
     :semfile (semfile->description (.getSemfile sense-info))
     :words (StringList->seq (.getWords sense-info))
     :tonto (StringList->seq (.getTonto sense-info))
     :sumo (.getSumo sense-info)
     :cyc (.getCyc sense-info)}))

(def --result
  (as-> text1 $$
    (select [ALL-PREDICATES :head_token] $$)
    (map (fn [id] (select-one [ALL-TOKENS #(= (:id %) id) (collect-one :form) :wn] text1))
         $$)
    (mapcat (fn [[form wn]] (let [info (sense-info wn)
                                  tonto (:tonto info)]
                              (map #(vector form (:semfile info) % (:sumo info) (:cyc info)) tonto)))
            $$)))

(with-open [out-file (clojure.java.io/writer "/tmp/out.csv")]
  (csv/write-csv out-file (concat [["Word", "Semfile", "TONTO", "SUMO", "CYC"]] --result)))




