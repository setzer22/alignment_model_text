(ns edu.upc.nlp4bpm-commons.label-parser-v2
  (:import [edu.upc.Jfreeling MacoOptions Tokenizer Splitter
            Maco HmmTagger ChartParser DepTxala Nec Senses
            Ukb ListSentenceIterator ListWordIterator
            ListAnalysisIterator OutputJson Document Paragraph Util]
           [edu.upc.nlp4bpm_commons AnalyzedLabel]
           [edu.stanford.nlp.trees Tree TreeFactory]
           [edu.stanford.nlp.trees.tregex TregexPattern TregexMatcher]
           [edu.upc.nlp4bpm_commons.tree FreelingTree FreelingTreeFactory]
           [java.util.regex Pattern])
  (:require [edu.upc.nlp4bpm-commons.freeling-seq :refer :all]
            [edu.upc.nlp4bpm-commons.core :refer [init-freeling]]
            [edu.upc.nlp4bpm-commons.freeling-output-format :refer :all]
            [edu.upc.nlp4bpm-commons.config :as config]
            [edu.upc.nlp4bpm-commons.utils :as utils :refer :all]
            [clojure.data.json :as json]
            [com.rpl.specter :as specter :refer :all]
            [edu.upc.nlp4bpm-commons.label-parser :as label-parser]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn encode-map-pattern [all-keys m]
  (let [q #(Pattern/quote (str %))]
    (str/join
     ";"
     (map
      #(str (q %) "=" (if (get m %) (q (get m %)) ".*"))
      all-keys))))

(def NIL-MARK "____nil____")

(defn encode-map [all-keys m]
  (str/join
   ";"
   (map
    #(str % "=" (get m % NIL-MARK))
    all-keys)))

(defn decode-map [m]
  #_"XXX: This decode function may fail if the ids contain ';' or '='"
  (apply hash-map
         (mapcat
          #(let [[k v] (str/split % #"=")]
             (when (not= v NIL-MARK)[(keyword (subs k 1)) v]))
          (str/split m #";"))))

(defn constituents->stanford:tree [constituents]
  (let [f (FreelingTreeFactory.)
        all-keys [:token :label]
        encode-node (fn encode-node [node]
                      (encode-map all-keys node))
        mk-tree (fn mk-tree [{:keys [label children head token] :as node}]
                  (cond
                    children #_=> (let [ch (java.util.ArrayList. (map mk-tree children))]
                                    (.newTreeNode f (encode-node node) ch))
                    token #_=> (.newLeaf f (encode-node node))
                    ))]
    (mk-tree constituents)))

(defn re-pos [re s]
  (let [matcher (re-matcher re s)]
    (take-while some? (repeatedly #(if (.find matcher) [(.start matcher) (.end matcher)] nil)))))


(defn encode-pattern [all-keys pattern]
  #_"XXX: Pre: Pattern doesn't contain nested { }"
  (let [positions (re-pos #"\Q{\E.*\Q}\E" pattern)]
    (reduce
     (fn [pattern [start end]]
       (let [original (subs pattern start end)
             val (->> original
                      (read-string)
                      (transform [MAP-VALS] str)
                      (encode-map-pattern all-keys))]
         (str/replace
          pattern
          original
          (str "/" val "/"))))
     pattern
     (reverse positions))))

#_(encode-pattern [:token :label]
                  (str "action"
                       "< {:label verbPhrase}=predicate"
                       "< nounPhrase=object"
                       "?< {:label /(n|v|vb)PrepPhrase/}=complement"))

#_(str/replace "a" #"a" "\\\\Q")

#_(constituents->stanford:tree
   {:label "action", :children [{:label "verb-phrase", :head "1", :children [{:leaf "1", :head "1", :token "t10.1", :word "destroy"}]} {:label "noun-phrase", :children [{:leaf "1", :head "1", :token "t10.2", :word "documents"}]} {:label "vb-prep-phrase", :children [{:leaf "1", :head "1", :token "t10.3", :word "about"} {:label "noun-phrase", :children [{:leaf "1", :token "t10.4", :word "old"} {:leaf "1", :head "1", :token "t10.5", :word "cases"}]}]}]})


#_(let [f (FreelingTreeFactory.)
        children (java.util.ArrayList. (repeat 10 (.newLeaf f "hello")))]
    (.newTreeNode f "hello" children))

(defn re-find-all [pattern str]
  (let [matcher (re-matcher pattern str)]
    (take-while
     identity
     (repeatedly #(re-find matcher)))))

#_ (defn get-head-and-words [parse-tree id]
     (let [find-subtree (fn find-subtree [{:keys [token children] :as node}]
                          (if (= token id)
                            node
                            (if children
                              (some find-subtree children)
                              nil)))]
       {:head id
        :words 2}))

#_ (defn try-pattern [pattern-str, parse-tree]
     (let [st-tree (Tree/valueOf (freeling:tree->stanford:tree parse-tree))
           pattern (TregexPattern/compile
                    pattern-str)
           matcher (.matcher pattern st-tree)]
       (when (.find matcher)
         (def --matcher matcher)
         (let [names (->> (re-find-all #"=[a-zA-Z]+" pattern-str)
                          (map #(subs % 1)))]
           (zipmap
            (map keyword names)
            (map #(.getNode matcher %) names))))))

#_ (let [tree {:label "action", :children [{:label "verb-phrase", :head "1", :children [{:leaf "1", :head "1", :token "t10.1", :word "destroy"}]} {:label "noun-phrase", :children [{:leaf "1", :head "1", :token "t10.2", :word "documents"}]} {:label "vb-prep-phrase", :children [{:leaf "1", :head "1", :token "t10.3", :word "about"} {:label "noun-phrase", :children [{:leaf "1", :token "t10.4", :word "old"} {:leaf "1", :head "1", :token "t10.5", :word "cases"}]}]}]}]

     (try-pattern
      (str "action"
           "< verbPhrase=predicate"
           "< nounPhrase=object"
           "?< /(n|v|vb)PrepPhrase/=complement")
      tree))

#_ (re-find --m)


