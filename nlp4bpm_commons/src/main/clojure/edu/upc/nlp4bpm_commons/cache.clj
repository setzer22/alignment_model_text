(ns edu.upc.nlp4bpm-commons.cache
  (:require [com.rpl.specter :refer :all]
            [clojure.pprint :refer :all]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:gen-class 
    :name edu.upc.nlp4bpm_commons.Cache
    :methods [#^{:static true} [initialize [String] void]
              #^{:static true} [createArgumentKey [String String String] Object]
              #^{:static true} [addItemOverwriting [Object String] void]
              #^{:static true} [addItemIfNotExists [Object String] void]
              #^{:static true} [retrieveItem [Object] String]
              #^{:static true} [saveCache [] void]
              #^{:static true} [enableCacheAutosave [] void]
              #^{:static true} [disableCacheAutosave [] void]]
    :prefix ""))

(def cache-path nil)
(def cache nil)

(defn initialize
  ([] (initialize nil))
  ([path]
   (do (def cache-path (if-not path
                         (str (System/getProperty "user.home") "/.textserver-cache.clj")
                         path))
       (if-not (.exists (io/file cache-path))
         (spit cache-path "{}"))
       (def cache (atom (binding [*read-eval* false]
                          (read-string (slurp cache-path))))))))

(defn createArgumentKey [text lang output]
  [text lang output])

(defmacro when-cache [& body]
  `(if (and cache-path cache)
     (do ~@body)
     (throw (java.lang.IllegalStateException. "The cache has not been initialized."))))

(defmacro if-cache [cache no-cache]
  `(if (and cache-path cache)
     ~cache
     ~no-cache))

(defn addItemOverwriting [text analyzed-text]
  (when-cache 
    (swap! cache assoc text analyzed-text)))

(defn addItemIfNotExists [text analyzed-text]
  (when-cache 
    (when-not (contains? @cache text)
      (addItemOverwriting text analyzed-text))))

(def cache-autosave-future nil)
(defn enableCacheAutosave [] 
  (when-cache 
    (def cache-autosave-future 
      (future (while true (do (Thread/sleep 60000) 
                              (spit cache-path @cache)))))))
(defn disableCacheAutosave []
  (when-cache
    (if cache-autosave-future 
      (future-cancel cache-autosave-future))))

(defn saveCache []
  (when-cache
    (spit cache-path @cache)))

(defn retrieveItem [text]
  "Retrieves an item from the cache. Returns null when not found."
  (when-cache 
    (let [item (get @cache text)]
      (if item item nil))))

      
