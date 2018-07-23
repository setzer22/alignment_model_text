(ns edu.upc.nlp4bpm-commons.utils)

(defn str-or-throw [& args]
  (let [s (apply str args)
        f (java.io.File. s)]
    (if (not (.exists f))
      (throw (java.io.FileNotFoundException.
              (str "Freeling error: File " s " was not found in the filesystem."
                   "This happened due to the requested language not being fully"
                   "supported by the current freeling installation.")))
      s)))


(defmacro let-def [defs & body]
  (let [defs (partition 2 defs)
        defs' (map
               (fn spy-on [[x y]]
                 [(symbol "__") `(intern ~'*ns* '~(symbol (str "--" (name x))) ~x)])
               defs)]
    `(let ~(into [] (mapcat identity (interleave defs defs')))
       ~@body)))
