(ns edu.upc.nlp4bpm-commons.core
  (:require [com.rpl.specter :refer :all]
            [clojure.pprint :refer :all]))

(defn init-freeling
  "Loads the JNI freeling modules. Must be called before any local freeling functions.
   Can be called multiple times and subsequent calls will be ignored."
  []
  (try
    (defonce --freeling-has-been-initialized ; A symbol we won't re-use
      (do (clojure.lang.RT/loadLibrary "Jfreeling")
          (edu.upc.Jfreeling.Util/initLocale "default")))
    (catch java.lang.UnsatisfiedLinkError e
      (throw (Exception.
                (str "No jfreeling native library found in java.library.path, cannot continue.\n"
                     "CASUE:    " (.getMessage e)))))))
