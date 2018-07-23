(ns edu.upc.modelvsdocument.textserver-cache
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [clojure.java.shell :refer [sh]]
            [edu.upc.modelvsdocument.initialization :as init] 
            [clojure.java.io :as io]
            [schema.core :as s :refer [fn defn defrecord]]))

; TODO: Remove this

(comment
  (def cache-path (str (System/getProperty "user.home") "/.textserver-cache2.clj"))
  (if-not (.exists (io/file cache-path))
    (spit cache-path "{}"))
  (def cache (atom (binding [*read-eval* false] 
                     (read-string (slurp cache-path)))))
  (comment (clojure.pprint/pprint (count (keys @cache))))

  (defn overwrite-item [text analyzed-text]
    (swap! cache assoc text analyzed-text))

  (defn add-item [text analyzed-text]
    (when-not (contains? @cache text)
      (overwrite-item text analyzed-text)))

  (defn start-autosave [] 
    (future (while true (do (Thread/sleep 60000) 
                            (spit cache-path @cache)))))

  (defn save-cache []
    (spit cache-path @cache))

  (defn retrieve-item [text]
    "Retrieves an item from the cache. Returns null when not found."
    (identity text)
    (let [item (get @cache text)]
      (if item 
        (do (sh "notify-send" "cache hit!")
            (println "hit!")
            item)
        (do (sh "notify-send" "cache miss")
            (println "miss!")
            nil)))))

      
