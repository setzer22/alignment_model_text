(ns edu.upc.nlp4bpm-commons.Freeling
  (:require [edu.upc.nlp4bpm-commons.core :refer :all]
            [edu.upc.nlp4bpm-commons.config :as config]
            [edu.upc.nlp4bpm-commons.freeling-output-format :refer :all]
            [edu.upc.nlp4bpm-commons.utils :refer :all])
  (:import [edu.upc.Jfreeling Analyzer ConfigOptions InvokeOptions Document
            OutputJson OutputXml AnalysisLevel TaggerAlgorithm DependencyParser
            WSDAlgorithm ForceSelectStrategy]))


;; @CopyPasted in label_parser.clj
(defn mk-config-options [lang]
  (let [freeling-dir-path (config/-getConfig "freeling-dir-path")
        path freeling-dir-path
        lpath (str freeling-dir-path "/" lang "/")]
    (doto (ConfigOptions.)
      ;; Tokenizer configuration file
     (.setTOK_TokenizerFile (str-or-throw lpath "tokenizer.dat"))
     ;; Splitter configuration file
     (.setSPLIT_SplitterFile (str-or-throw lpath "splitter.dat"))
     ;; Morphological analyzer options
     (.setMACO_Decimal ".")
     (.setMACO_Thousand ",")
     (.setMACO_LocutionsFile  (str-or-throw lpath "locucions.dat"))
     (.setMACO_QuantitiesFile  (str-or-throw lpath "quantities.dat"))
     (.setMACO_AffixFile  (str-or-throw lpath  "afixos.dat"))
     (.setMACO_ProbabilityFile  (str-or-throw lpath "probabilitats.dat"))
     (.setMACO_DictionaryFile  (str-or-throw lpath "dicc.src"))
     (.setMACO_NPDataFile  (str-or-throw lpath "np.dat"))
     (.setMACO_PunctuationFile  (str-or-throw path "common/punct.dat"))
     (.setMACO_ProbabilityThreshold  0.001)
     ;; NEC config file
     (.setNEC_NECFile  (str-or-throw lpath  "nerc/nec/nec-ab-poor1.dat"))
     ;; Sense annotator and WSD config files
     (.setSENSE_ConfigFile  (str-or-throw lpath "senses.dat"))
     (.setUKB_ConfigFile  (str-or-throw lpath "ukb.dat"))
     ;; Tagger options
     (.setTAGGER_HMMFile  (str-or-throw lpath "tagger.dat"))
     (.setTAGGER_ForceSelect ForceSelectStrategy/RETOK)
     ;; Chart parser config file
     (.setPARSER_GrammarFile  (str-or-throw lpath "chunker/grammar-chunk.dat"))
     ;; Dependency parsers config files
     (.setDEP_TxalaFile  (str-or-throw lpath "dep_txala/dependences.dat"))
     (.setDEP_TreelerFile  (str-or-throw lpath "dep_treeler/dependences.dat")) ; This was in dep_treeler/labeled/dependences.dat originally.
     ;; Coreference resolution config file
     (.setCOREF_CorefFile  (str-or-throw lpath "coref/relaxcor_dep/relaxcor.dat"))
     ;; Semantic graph config file
     (.setSEMGRAPH_SemGraphFile (str-or-throw lpath "semgraph/semgraph-SRL.dat")))))

(def textserver:level->freeling:level
  #(or ({"semgraph" AnalysisLevel/SEMGRAPH
         "coref"    AnalysisLevel/COREF
         "dep"      AnalysisLevel/DEP
         "ident"    AnalysisLevel/IDENT
         "morfo"    AnalysisLevel/MORFO
         "parsed"   AnalysisLevel/PARSED
         "tagged"   AnalysisLevel/TAGGED
         "senses"   AnalysisLevel/SENSES
         "shallow"  AnalysisLevel/SHALLOW} %)
       (throw (Exception. (str "Processing level " % " not recognised.")))))

(defn mk-invoke-options [level]
  (doto (InvokeOptions.)
    (.setInputLevel AnalysisLevel/TEXT)
    (.setOutputLevel (textserver:level->freeling:level level))
    ; Activate/deactivate morphological analyzer modules
    (.setMACO_UserMap  false)
    (.setMACO_AffixAnalysis  true)
    (.setMACO_MultiwordsDetection  true)
    (.setMACO_NumbersDetection  true)
    (.setMACO_PunctuationDetection  true)
    (.setMACO_DatesDetection  true)
    (.setMACO_QuantitiesDetection   true)
    (.setMACO_DictionarySearch  true)
    (.setMACO_ProbabilityAssignment  true)
    (.setMACO_CompoundAnalysis  false)
    (.setMACO_NERecognition  true)
    (.setMACO_RetokContractions  false)
    (.setNEC_NEClassification  true)
    (.setSENSE_WSD_which WSDAlgorithm/UKB)
    (.setTAGGER_which TaggerAlgorithm/HMM)
    (.setDEP_which  DependencyParser/TREELER)))


(defonce analyzers (atom {}))

(defn mk-analyzer [lang]
  (let [config-options (mk-config-options lang)]
    (Analyzer. config-options)))

;;TODO: Thread safety
(defn analyze [& {:keys [text lang output level]
                  :or {output "json", lang "en", level "semgraph"}}]
  (init-freeling)
  (if (not (get @analyzers lang)) (swap! analyzers assoc lang (mk-analyzer lang)))
  (let [analyzer (get @analyzers lang)
        doc (Document.)]
    (.setCurrentInvokeOptions analyzer (mk-invoke-options level))
    (.analyze analyzer text doc true)
    (((output-fn lang) output) doc)))
