(ns edu.upc.nlp4bpm-commons.CredentialsParser
  (:gen-class
   :name edu.upc.nlp4bpm_commons.CredentialsParser
   :methods [#^{:static true} [parseConfig [String] java.util.Map]]))

(defn -parseConfig [path]
  (java.util.HashMap
   (let [m (read-string (slurp path))]
     {"username" (:username m)
      "password" (:password m)})))
