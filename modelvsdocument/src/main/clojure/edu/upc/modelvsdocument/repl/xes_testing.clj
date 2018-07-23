(ns edu.upc.modelvsdocument.repl.xes-testing
  (:import [org.deckfour.xes.in XesXmlParser]
           [org.deckfour.xes.model XVisitor]))

(defmacro attr-literal [k v]
  `(org.deckfour.xes.model.impl.XAttributeLiteralImpl. ~k ~v))

(defn attr-map [m]
  (let [m' (reduce
            (fn [m [k v]] (assoc m k (attr-literal k v))) {} m)]
   (org.deckfour.xes.model.impl.XAttributeMapImpl. m')))

(defmacro xid []
  `(org.deckfour.xes.id.XID.))

(do (def standard-attributes
      {"concept:name" (fn [ev name] (.assignName (org.deckfour.xes.extension.std.XConceptExtension/instance) ev name))})

    (def standard-attribute-names
      (keys standard-attributes)))

(defn event [attrs]
  (let [std-attrs (select-keys attrs standard-attribute-names)
        remaining-attrs (apply dissoc attrs standard-attribute-names)
        ev (org.deckfour.xes.model.impl.XEventImpl. (xid) (attr-map remaining-attrs))]
    (doseq [[std-attr val] std-attrs]
      ((standard-attributes std-attr) ev val))
    ev))

(.getAttributes (event {"concept:name" "Pedo"}))

(defn mk-log
  ([traces] (mk-log traces {}))
  ([traces attrs]
   (let [log (org.deckfour.xes.model.impl.XLogImpl. (attr-map attrs))]
                                        ; What does the map do?
     (doseq [t traces]
       (.add log t))
     log)))

(defn mk-trace
  ([events] (mk-trace events {}))
  ([events attrs]
   (let [trace (org.deckfour.xes.model.impl.XTraceImpl. (attr-map attrs))]
     (doseq [e events]
       (.add trace e))
     trace)))

(def log (mk-log [(mk-trace [(event {"Role" "HR"}) (event {"Role" "HR"})])]))

(defn save-to-xml [log file]
  (.serialize (org.deckfour.xes.out.XesXmlSerializer.)
              log
              (clojure.java.io/output-stream file)))
