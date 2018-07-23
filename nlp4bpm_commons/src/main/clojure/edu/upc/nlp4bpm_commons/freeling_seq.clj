(ns edu.upc.nlp4bpm-commons.freeling-seq)

;; Freeling's swig support for list iteration is not currently the best.
;; A concrete type (ListString, ListSentence) is created for every kind of
;; generic list instantiation in C++. These types are not only non-generic
;; but do not implement any of java's interfaces. This section implements
;; two helper functions fl->seq and seq->fl that transparently handle converting
;; from/to freeling list types.

;; TODO: @Performance: Cache constructors in a protocol to avoid dynamic
;; class lookup for each iteration of the list

(def aliases
  ;; These values are aliases freeling uses. For example, Document can be casted toString
  ;; a list of Paragraph objects. These have to be manually specified
  {"edu.upc.Jfreeling.Document" "edu.upc.Jfreeling.ListParagraph"
   "edu.upc.Jfreeling.Paragraph" "edu.upc.Jfreeling.ListSentence"
   "Document" "ListParagraph"
   "Paragraph" "ListSentence"
   "Long" "Int"})

(defn typename [x]
  (if (aliases x)
    (aliases x)
    x))

(defn- get-list-constructor [list-type]
  (let [constructor (.getConstructor
                     (Class/forName (str "edu.upc.Jfreeling." (typename list-type)))
                     (make-array java.lang.Class 0))]
    #(.newInstance constructor (make-array java.lang.Object 0))))

(defn- get-iterator [list]
  (let [constructor-class-name (str (typename (-> list .getClass .getName)) "Iterator")
        constructor (.getConstructor
                     (Class/forName constructor-class-name)
                     (into-array java.lang.Class [(Class/forName (-> list .getClass .getName typename))]))]
    (.newInstance constructor (into-array java.lang.Object [list]))))

(defn- it->vec [it]
  (loop [it it
         result []]
    (if (.hasNext it)
      (recur it (conj result (.next it)))
      result)))

(defn fl->seq [lst]
  (let [it (get-iterator lst)]
    (it->vec it)))

(defn- seq->FreelingList [constructor, l]
  (let [add (fn [list element] (.pushBack list element) list)]
    (loop [result (constructor)
           [x & xs] l]
      (if x
        (recur
         (add result x)
         xs)
        result))))

(defn seq->fl [[x & xs :as lst]]
  (assert x "Cannot determine list type because list is empty.")
  ;; NOTE: Some list element types, like string won't have the
  ;; edu.upc.Jfreeling prefix, but their list type will always
  ;; be in that package.
  (let [element-class-name (-> x .getClass .getName)
        idx (.lastIndexOf element-class-name ".")
        list-class-name (str "List" (typename (subs element-class-name (inc idx))))
        list-constructor (get-list-constructor list-class-name)]
    (seq->FreelingList list-constructor lst)))

(defn fl-forall [fun lst]
  (if (.empty lst)
    lst
    (let [it (get-iterator lst)]
      (loop [it it]
        (if (.hasNext it)
          (do (fun (.next it))
              (recur it))
          it))
      lst)))
