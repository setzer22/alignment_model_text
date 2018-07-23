(ns edu.upc.modelvsdocument.repl.tregex-test
  (:import [edu.stanford.nlp.trees Tree]
           [edu.stanford.nlp.trees.tregex TregexPattern TregexMatcher])
  (:require [clojure.string :as str]))


(def --test-tree {:label "action", :children [{:label "verb-phrase", :head "1", :children [{:leaf "1", :head "1", :token "t1.1", :word "decide"}]} {:label "vb-prep-phrase", :children [{:leaf "1", :head "1", :token "t1.2", :word "about"} {:label "noun-phrase", :children [{:leaf "1", :head "1", :token "t1.3", :word "document"}]}]}]})

(def --test2-tree
  {:label "action", :children [{:label "verb-phrase", :head "1", :children [{:leaf "1", :head "1", :token "t10.1", :word "destroy"}]} {:label "noun-phrase", :children [{:leaf "1", :head "1", :token "t10.2", :word "documents"}]} {:label "vb-prep-phrase", :children [{:leaf "1", :head "1", :token "t10.3", :word "about"} {:label "noun-phrase", :children [{:leaf "1", :token "t10.4", :word "old"} {:leaf "1", :head "1", :token "t10.5", :word "cases"}]}]}]})

(defn dash-case->camelCase [s]
  (let [[x & xs] (str/split s #"-")
        capitalize-first (fn [s] (apply str (.toUpperCase (str (first s))) (rest s)))]
    (apply str x (map capitalize-first xs))))

(defn freeling:tree->stanford:tree [{:keys [label children head leaf token word]}]
  (if children
    (apply str
           (concat ["(" (dash-case->camelCase label) " "]
                   (map freeling:tree->stanford:tree children)
                   [")"]))
    (str token "")))

(comment
  (do (let [tree (Tree/valueOf (freeling:tree->stanford:tree --test2-tree))

            p (TregexPattern/compile
               (str "action"
                    "< (verbPhrase < @__=predicate)"
                    "< (nounPhrase < @__=object)"
                    "?< (/(n|v|vb)PrepPhrase/ < @__=complement)"))
            m (.matcher p tree)]
        (def --matcher m))
      (.find --matcher)
      (.getNode --matcher "complement")))




