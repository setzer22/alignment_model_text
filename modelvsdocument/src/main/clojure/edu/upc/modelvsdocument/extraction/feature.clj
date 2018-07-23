(ns edu.upc.modelvsdocument.extraction.feature
  (:use [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.extraction.macros])
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [edu.upc.modelvsdocument.wordnet :as wn])
  (:gen-class))

(defrecord Feature [ftype argument weight]
  java.lang.Comparable
  (compareTo [this f] 
    (let [cmp (compare (:ftype this) (:ftype f))] 
      (if 
        (zero? cmp)
        (compare (:argument this) (:argument f))
        cmp))))

; Feature comparison should not use = but this:
(spec-fn feature= ::t/feature ::t/feature)
(defn feature= [f1 f2]
  (= 0 (compare f1 f2)))

(defn feature-combinator [combinator-fn {:keys [ftype argument] :as f1} f2]
  (map->Feature {:ftype ftype 
                 :argument argument 
                 :weight (combinator-fn (:weight f1) (:weight f2))}))

(spec-fn distinct-features ::t/feature-vector -> ::t/feature-vector)
(defn distinct-features 
  "Returns the distinct features in fv without considering the weight in the computation. for
  repeated ocurrences the maximum weight is kept."
  [fv]
  (distinct-by fv 
               (fn [f] [(:ftype f) (:argument f)]) 
               (partial feature-combinator max)))

(defn fv-union [fv1 fv2]
  (distinct-features (concat fv1 fv2)))

; Intersection for feature vectors
(spec-fn fv-intersection ::t/feature-vector ::t/feature-vector)
(defn fv-intersection [fv1 fv2]
  (let [sh (shortest fv1 fv2)
        ln (longest fv1 fv2)]
    (mapcat (fn [feat] (filter #(feature= feat %) sh)) ln)))

; Override pprint behaviour
(spec-fn pprint-feature ::t/feature)
(defn pprint-feature [{argument :argument ftype :ftype weight :weight}]
  (print (str weight "*" (name ftype) (if (vector? argument) (seq argument) (str "(" argument ")")))))
(. clojure.pprint/simple-dispatch addMethod Feature pprint-feature)

; Multimethod for feature explanatory string
(defmulti explain-feature :ftype)
(defmethod explain-feature :default [f] (pprint-feature f))

; Feature definition (see deffeatures macro)
(deffeatures
  (has-form [form] :weight 2.0
            "Contains the lemma \"%1$s\"")

  (has-lemma [pos lemma] :weight 8.0
             "Contains the %1$s \"%2$s\"")

  (has-action [action] :weight 5.0
              "Contains the action \"%s\"")

  (agent-head [lemma pos] :weight 10.0
              "The agent main word is \"%1$s\" (%2$s)")

  (patient-head [lemma pos] :weight 10.0
                "The direct object main word is \"%1$s\" (%2$s)")

  (in-agent [lemma pos] :weight 2.0
            "The agent contains the %2$s \"%1$s\"")

  (in-patient [lemma pos] :weight 2.0
              "The object contains the %2$s \"%1$s\"")

  (has-synset [synset] :weight 1.0
              "Contains the synset %s")

  (has-parent-synset [synset] :weight 0.3
                     "Contains the synset %s")

  (lemma-conditional-pred [lemma pos] :weight 4.0
                          "Precedes a conditional clause containing the %2$s \"%1$s\"  ")

  (lemma-conditional-follow [lemma pos] :weight 4.0
                            "Goes after a conditional clause containing the %2$s \"%1$s\"  ")

  (has-discourse-marker [marker-type] :weight 10.0
                        "Contains a %1$s discourse marker"))

; NOTE: Support for complex features that need more info than its arguments
;       is not supported by the deffeatures DSL. So special cases should
;       be added manually.
(defmethod explain-feature :has-synset [{[wn-id] :argument}]
  (str "Contains the synset " wn-id " (" (wn/sense-of wn-id) ")"))

(defmethod explain-feature :has-parent-synset [{[wn-id] :argument}]
  (str "Contains an hyponym of " wn-id " (" (wn/sense-of wn-id) ")"))
