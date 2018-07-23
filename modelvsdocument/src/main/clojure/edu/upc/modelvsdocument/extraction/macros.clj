(ns edu.upc.modelvsdocument.extraction.macros
  (:use [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.config])
  (:require [edu.upc.modelvsdocument.schemas :as t]
            [clojure.spec :as spec]
            [clojure.core.match :as match :refer [match]]))

(defmacro model-extractor [name args body]
  `(defn ~name ~args ~body))

(spec/def ::model-extractor-input
  (spec/cat
    :extractor-name symbol?
    :extractor-type (spec/spec (spec/* symbol?))
    :docstring (spec/? string?)
    :args (spec/spec (spec/cat :task-id symbol? :model symbol? :tf-idf-table symbol?))
    :body any?))

(spec/def ::text-extractor-input
  (spec/cat
    :extractor-name symbol?
    :extractor-type (spec/spec (spec/* symbol?))
    :docstring (spec/? string?)
    :args (spec/spec (spec/cat :sentence symbol? :tf-idf-table symbol?))
    :body any?))


(defmacro symbol-in-caller-ns [name]
  (symbol
   (str *ns*)
   name))

(defmacro model-extractor
  {:style/indent 2}
  [& input]
  (if (not (spec/valid? ::model-extractor-input input))
    (throw (Exception. (str "Invalid input: " (spec/explain-str ::model-extractor-input input)))))
  (let [{:keys [extractor-name extractor-type args docstring body]
         :or {docstring ""}} (spec/conform ::model-extractor-input input)]
    `(do
       (let [extractor-fn# ~(concat `(fn ~extractor-name)
                                    [(mapv second args)]
                                    [body])]

         (defn ~extractor-name ~docstring ~(mapv second args) (extractor-fn# ~@(map second args)))
         (spec-fn ~extractor-name ::t/id ::t/bpmn ::t/analyzed-model ::t/tf-idf-table ~'-> ::t/feature-vector)
         (doseq [t# ~extractor-type]
           (swap! (edu.upc.modelvsdocument.extraction.macros/symbol-in-caller-ns "extractor-functions")
                  assoc-in
                  [t# ~(keyword extractor-name)]
                  extractor-fn#))))))

;;TODO: Now that we're using symbol-in-caller-ns, is this a literal copy-paste of model-extractor?
(defmacro text-extractor
  {:style/indent 2}
  [& input]
  ;@Copypasted
  (if (not (spec/valid? ::text-extractor-input input))
    (throw (Exception. (str "Invalid input: " (spec/explain-str ::text-extractor-input input)))))
  (let [{:keys [extractor-name extractor-type args docstring body]} (spec/conform ::text-extractor-input input)
        docstring (if docstring docstring "")]
    `(do
       (let [extractor-fn# ~(concat `(fn ~extractor-name)
                                    [(mapv second args)]
                                    [body])]

         (defn ~extractor-name ~docstring ~(mapv second args) (extractor-fn# ~@(map second args)))
         (spec-fn ~extractor-name ::t/sentence ::t/tf-idf-table ~'-> ::t/feature-vector)
         (doseq [t# ~extractor-type]
           (swap! (edu.upc.modelvsdocument.extraction.macros/symbol-in-caller-ns "extractor-functions")
                 assoc-in
                 [t# ~(keyword extractor-name)]
                 extractor-fn#))))))




; Mini DSL to define new features. Semantics are:

;(deffeatures
;  (feature-name [arg1 arg2 ... argN] :weight weights
;     "Explanatory string %2$s %3$s")) ; Args will be applied in order to
;                                        the explanatory string using a java formatter

; NOTE: Explanatory strings and weights are overriden from the config file

(defmacro deffeatures [& features]
  (let [names (map first features)
        weights (map #(nth % 3) features)
        explanations (map #(nth % 4) features)
        explanation-arguments (map #(nth % 1) features)
        feature-weights (gensym 'feature-weights)
        mk-feature (gensym 'mk-feature)
        fweights-def `(def ~feature-weights ~(into {} (map vector (map keyword names) weights)))
        mkfeature-def `(defn ~mk-feature
                         ([ftype# argument#]
                          (Feature. ftype# argument# (if (contains? (config :feature-weight-overrides) ftype#)
                                                       (ftype# (config :feature-weight-overrides))
                                                       (ftype# ~feature-weights))))
                         ([ftype# argument# weight-mul#]
                          (Feature. ftype# argument# (* weight-mul#
                                                        (if (contains? (config :feature-weight-overrides) ftype#)
                                                          (ftype# (config :feature-weight-overrides))
                                                          (ftype# ~feature-weights))))))
                                        ; Hack: The explain-feature multimethod is defined in a namespace that requires
                                        ;       this macro.
        explain-feature 'edu.upc.modelvsdocument.extraction.feature/explain-feature
        feature-funcs-def (apply list 'do
                                 (map (fn make-func [n]
                                        `(defn ~(symbol n)
                                           ([arg#] (~mk-feature ~(keyword n) arg#))
                                           ([arg# weight#] (~mk-feature ~(keyword n) arg# weight#))))
                                      names))
        feature-explain-def (conj (for [[n e a] (map vector names explanations explanation-arguments)]
                                    `(defmethod ~explain-feature ~(keyword n) [{~a :argument}]
                                       (apply format
                                              (if (contains? (config :feature-explain-overrides) ~(keyword n))
                                                (~(keyword n) (:feature-explain-overrides config))
                                                ~e)
                                              ~a))) 'do)]
    (list 'do fweights-def mkfeature-def feature-funcs-def feature-explain-def)))

(defmacro feature+synsets
  ;;"Extracts the feature specified by feature-fn as well as synset
  ;;and hypernym features corresponding to its feature type"
  {:style/indent 3}
  [feature-fn token args tf-idf]
  ;;TODO: token: once-only
  `(concat [(~feature-fn
             ~(mapv (fn [arg] (if (keyword? arg) `(get ~token ~arg) ~arg))
                    args)
             ~tf-idf)]
           (edu.upc.modelvsdocument.extraction.common/token-sense-features
            ~(-> feature-fn name keyword)
            ~token
            ~tf-idf)))

;; Constituents pattern matching language

;; Syntax
;; (tree-match [value-to-match]
;;   tree-pattern-1 something-1
;;   tree-pattern-2 something-2
;;   ...
;;   tree-pattern-n something-n) 

;; Example syntax
;;(tree-match [(enrich-with-tokens s2 (first (:constituents s2)))]
;;["claus" ["adv" "if"] & rst] 
;;(do-something rst)

;;["sub-cl" "if" & rst] 
;;(do-something rst))

(defn se [n]
  (match [n] 
         [[root & children]] {:label root
                              :children (mapv se children)}
         [node] (cond
                  (string? node) {:leaf "1", :token {:lemma node}}
                  (symbol? node) node)))

(defmacro se-match [m & bindings]
  (let [bindings' (mapcat (fn [[fst snd]] [(if (= :else fst) :else [(se fst)]) snd]) (partition 2 bindings))] 
    `(match ~m
            ~@bindings')))

(defmacro tree-match [[T] & bindings]
  (assert (even? (count bindings)) "You must provide an even number of patterns, each with its result")
  (assert (every? #(not= :else %) (map first (partition 2 bindings))) "No else clause is allowed.")
  (assert (every? not-nil? (map second (partition 2 bindings))) "No pattern can return nil as a value.")
  (let [bindings' (concat bindings [:else nil])] 
    `((fn treematches# [{label# :label children# :children :as T#}] 
        (let [match# (se-match [T#] 
                               ~@bindings')] 
          (cond 
            match# match#
            (and label# children#) (remove nil? (flatten (map #(treematches# %) children#)))
            :else nil))) 
      ~T)))

