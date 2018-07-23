(ns edu.upc.modelvsdocument.atds
  (:require [edu.upc.modelvsdocument.text :as text]
            [edu.upc.atdlib.brat-parser :as parser]
            [edu.upc.atdlib.atd2fl :as fl]
            [edu.upc.atdlib.atd2optional :as optional]
            [edu.upc.atdlib.atd2ordermatrix :as order]
            [edu.upc.atdlib.base-nlp :as base]
            [edu.upc.atdlib.atd2roles :as roles]
            [clojure.java.io :as io]))

(defn- get-ann-file [ann-filename]
  (let [f (io/file (System/getProperty "user.home") "BPMN" "annotations" ann-filename)]
    (when-not (.exists f) (throw (Exception. (str "Please make sure this file exists: " (.getAbsolutePath f)))))
    f))

(defn annotation-exists? [case-name]
  (.exists (io/file (System/getProperty "user.home") "BPMN" "annotations" (str case-name ".ann"))))

(defn get-case-info [case-name]
  (let [
        ann-file (get-ann-file (str case-name ".ann"))
        atd (parser/parse (slurp ann-file))

        text-file (get-ann-file (str case-name ".txt"))
        text (slurp text-file)
        base (base/analyze text)

        fl (fl/atd2fl base atd)
        optional (optional/atd2optional atd)
        order (order/atd2ordermatrix atd)
        roles (roles/atd2roles atd base)]
    {:atd atd
     :text text
     :text-an fl
     :optional-task-ids optional
     :order-matrix order
     :roles roles}))
