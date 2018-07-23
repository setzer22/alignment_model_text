(ns edu.upc.modelvsdocument.wordnet
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [clojure.spec :as spec]
            [clojure.java.io :as io]
            [clojure.spec.test :as stest]
            [edu.upc.modelvsdocument.schemas :as t :refer :all]
            [clojure.set :as set :refer [map-invert]]))

(spec-fn read-dictionaries string?)
(defn read-dictionaries [wordnet-path]
  (let [relations-lines (map #(clojure.string/split % #" ")
                             (line-seq (io/reader (io/file wordnet-path "wn30.src"))))
        senses-lines    (map #(clojure.string/split % #" ")
                             (line-seq (io/reader (io/file wordnet-path "senses30.src"))))
        wn->fullname    (into {} (map (fn [[wn-id & senses]] [wn-id, senses]) senses-lines))
        wn->name        (into {} (map (fn [[fst [fst-sense & _]]] [fst fst-sense]) wn->fullname))
        wn->parent      (into {} (map (fn [[child parent & _]]
                                        [child, (when (not= "-" parent) parent)])
                                      relations-lines))
        wn->children    (dissoc 
                         (reduce (fn [rev-map [k v]]
                                   (merge-with
                                    concat
                                    rev-map
                                    {v [k]}))
                                 {}
                                 wn->parent)
                         nil)]
    {:all-senses wn->fullname
     :sense wn->name
     :parent-of wn->parent
     :children-of wn->children}))

(def wordnet nil)

(spec-fn read-wordnet-dictionaries string?)
(defn read-wordnet-dictionaries [wordnet-path]
  (def wordnet (read-dictionaries wordnet-path)))

(spec-fn senses-of (spec/nilable ::t/wordnet-id))
(defn senses-of [wn-id]
  (if wordnet
    (get (get wordnet :all-senses) wn-id)))

(spec-fn sense-of (spec/nilable ::t/wordnet-id))
(defn sense-of [wn-id]
  (if wordnet
    (get (get wordnet :sense) wn-id)))

(spec-fn parents-of (spec/nilable ::t/wordnet-id))
(defn parents-of [wn-id]
  (if wordnet
    (let [parent-ids (get (get wordnet :parent-of) wn-id)]
      (if parent-ids
        (apply vector (remove empty? (clojure.string/split parent-ids #":")))
        []))))

(spec-fn children-of (spec/nilable ::t/wordnet-id))
(defn children-of [wn-id]
  (if wordnet
    (get (get wordnet :children-of) wn-id)))

(spec-fn hiperonimy-chain-of (spec/nilable ::t/wordnet-id))
(defn hiperonimy-chain-of [wn-id]
  (let [hiperonimy-chain-of'
        (fn [wn-id]
          (let [parent-ids (parents-of wn-id)]
            (lazy-seq (into
                        parent-ids
                        (if parent-ids
                          (apply interleave (map hiperonimy-chain-of parent-ids)) [])))))]
    (distinct (hiperonimy-chain-of' wn-id))))
