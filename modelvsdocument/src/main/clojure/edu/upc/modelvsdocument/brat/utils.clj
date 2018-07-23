(ns edu.upc.modelvsdocument.brat.utils
  (:require [clj-pipeline.core :as pipeline]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer :all]
            [edu.upc.modelvsdocument.utils :refer :all]
            [clojure.set :as set]))

(def token-re #"t(\d+)\.(\d+)")

(defn token-path
  "Returns a specter path that efficiently navegates to token with tk-id
   in a freeling document"
  [tk-id]
  (let [[_ s-id t-pos] (re-matches token-re tk-id)
        s-id (try-or (dec (Integer/parseInt s-id)) nil)
        t-pos (try-or (dec (Integer/parseInt t-pos)) nil)]
    (when-not (and s-id t-pos)
      (throw (Exception. (str "Error when parsing token id: " tk-id))))
    [:paragraphs ALL :sentences (keypath s-id) :tokens (keypath t-pos)]))

(defn adj-with-type [relation-type relation-graph node-id]
  (get-in relation-graph [relation-type node-id]))

(defn enrich-Tag [original-text analyzed-text {:keys [start end] :as tag}]
  (let+ [tagged-text :dbg (subs original-text start end)
         start :dbg (+ start (count (take-while #(Character/isWhitespace %) tagged-text)))
         end :dbg (- end (count (take-while #(Character/isWhitespace %) (reverse tagged-text))))
         start-token :dbg (select-one [:paragraphs ALL :sentences ALL
                                       :tokens ALL #(= (:begin %) (str start))]
                                      analyzed-text)
         end-token :dbg (select-one [:paragraphs ALL :sentences ALL
                                     :tokens ALL #(= (:end %) (str end))]
                                    analyzed-text)]
    (assert (pos? (- end start)) "Annotation range must be non-empty")
    (assert (and start-token end-token) (str "Annotation bounds [" start "," end
                                             "] should match some token."))
    (assoc tag
           :start-token start-token
           :end-token end-token)))

(defn get-tokens-in-ann-range [analyzed-text {:keys [start-token end-token] :as ann}]
  (let [ids-range (fn [{id-start :id} {id-end :id}]
                    (let [[_ sentence start] (re-matches token-re id-start)
                          start (Integer/parseInt start)
                          [_ _ end] (re-matches token-re id-end)
                          end (Integer/parseInt end)]
                      (map #(str "t" sentence "." %) (range start (inc end)))))]
    (mapv #(select-one (token-path %) analyzed-text)
          (ids-range start-token end-token))))


(defn nest
  "plain-map is a map with sequences as keys. Returns a map where
  {[x y] z} becomes {x {y z}} for all bindings in the map."
  [plain-map]
  (reduce
   (fn [nested-map [ks v]]
     (assoc-in nested-map ks v))
   {}
   plain-map))

(defn copy-to-clipboard!
  "Copies the selected string to the system clipboard"
  [str]
  (let [selection (java.awt.datatransfer.StringSelection. str)
        clipboard (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit))]
    (.setContents clipboard selection nil)))
