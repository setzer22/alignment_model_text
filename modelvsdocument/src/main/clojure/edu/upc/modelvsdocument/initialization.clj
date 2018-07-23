(ns edu.upc.modelvsdocument.initialization
  (:use [edu.upc.modelvsdocument.utils]))

(def registered-funcs [])

(defn add [func]
  (alter-var-root #'registered-funcs conj func))

(defmacro on-start [& body]
  `(add (fn [] ~@body)))

(identity registered-funcs)

