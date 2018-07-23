(ns edu.upc.modelvsdocument.utils
  (:import [javax.swing JFrame JPanel JProgressBar SwingUtilities]) 
  (:require [com.rpl.specter :as specter]
            [clojure.core.matrix :as m]
            [clojure.spec :as spec] 
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn grep-methods 
  ([clss re]
   (filter #(re-matches re %) (map #(.getName %) (.getMethods clss))))
  ([clss] 
   (grep-methods clss #".*")))

(defmacro comp-if [expr neg zero pos]
  "Returns neg, zero or pos depending on the numerical value of expr"
  `(let [res# ~expr] 
     (cond (< res# 0) ~neg
           (= res# 0) ~zero 
           :else       ~pos)))


;; NOTE: sortest and longest have "complementary" default behaviour so you can 
;;       bind two alias names to both the shortest and longest of two sequences
;;       like so: 
;;         (let [long (longest c1 c2)
;;               short (shortest c1 c2)])
(defn shortest [coll1 coll2]
  "Returns the shortest collection. When equal size coll1 is returned"
  (if (<= (count coll1) (count coll2)) coll1 coll2))

(defn longest [coll1 coll2]
  "Returns the longest collection. When equal size coll2 is returned"
  (if (> (count coll1) (count coll2)) coll1 coll2))

(defn approx-eq [epsilon x y] 
  (< (- x y) epsilon))

(def approx? (partial approx-eq 0.00001))

(defn map-by [lst f]
  "lst is a list of maps, returns a map with lst's elements indexed by their k key"
  (reduce 
    (fn [new-map e] (assoc new-map (f e) e))
    {}
    lst))

(defn index-of [pred lst] 
  (let [index (count (take-while (complement pred) lst))]
    (if (= index (count lst))
      (throw (Exception. (str "index-of: No element in " lst " is true for predicate " pred)))
      index)))

(defn in? [seq elm]  
  "True if seq contains elm"
  (if (some #(= elm %) seq) true false))

(defn zip [L1 L2]
  (map vector L1 L2))

(defn nth2 [coll i j]
  "Coll is a list of lists. Returns the element (i,j) of coll" 
  (nth (nth coll i) j))

; Alias for map to better express the intent of zipping
(def zip-with map)

(def not-nil? (comp not nil?))

(defn seq-intersection [fv1, fv2]
  (seq (set/intersection (into (sorted-set) fv1) (into (sorted-set) fv2))))

(defn seq-difference [fv1, fv2]
  (seq (set/difference (into (sorted-set) fv1) (into (sorted-set) fv2))))

(defn get-matrix [mat i j]
  (let [N (count mat)
        M (count (nth mat 0))]
    (if (or (>= i N) (>= j M))
      (throw (Exception. (str "Out of bounds access for indices ["i "," j "].")))
      (get-in mat [i j]))))

(defn remove-nil [lst] (remove nil? lst))

(defn without-repeated [lst]
  (distinct lst))

(defn span [pred lst]
  (reduce
    (fn [[Y N] x] (if (pred x)
                    [(concat Y [x]) N]
                    [Y (concat N [x])]))
    [[] []]
    lst))

(defn seq-to-matrix [lst n]
  "Converts lst into a matrix with columns of size n"
  (into [] (map #(into [] %) (partition n lst))))

(defmacro square [x] (list * x x))

(defn pair-with 
  ([f]
   (map (fn [x] [(f x) x])))
  ([f lst]
   (sequence (pair-with f) lst)))

(defn pair-with' 
  ([f]
   (map (fn [x] [x (f x)])))
  ([f lst]
   (sequence (pair-with' f) lst)))

(def pair-with-l pair-with)
(def pair-with-r pair-with')

(defn find-in-seq [seq f]
    (reduce #(when (f %2) (reduced %2)) nil seq))

(spec/def ::ponderate-atom #(or (list? %) (number? %) (symbol? %)))
(spec/def ::ponderate-input (spec/* (spec/cat :coef ::ponderate-atom :value ::ponderate-atom)))
;(spec/conform ::ponderate-input [3 '(+ 1 2), 4 'x, 5 2])
(defmacro ponderate [& ponderations]
  (let [input (spec/conform ::ponderate-input ponderations)]
    (if (= :clojure.spec/invalid input)
      (throw (Exception. (str "Wrong syntax for macro:\n" 
                           (with-out-str (spec/explain ::ponderate-input ponderations))))))
    (conj (for [{:keys [coef value]} input]
            `(* ~coef ~value)) `+)))

(defn avg [lst]
  (if-not (empty? lst) 
    (/ (reduce + lst) (count lst))
    0))

(defn harmonic-mean [lst]
  (/ (double (count lst)) (double (reduce + (map #(/ 1.0 %) lst)))))

(defn format-numbers [structure]
  (specter/transform (specter/walker number?) 
                     #(format "%.4f" (double %))
                     structure))

(defn extension [file]
  "Returns the file extension of file"
  (let [s (.getName file)]
    (nth (re-find #".*\.(.*)" s) 1)))

(defn filename [path]
  (let [f (clojure.java.io/file path)]
    (.getName f)))

(defn strip-extension [file-or-path]
  "Returns the file name without extension"
  (let [file (io/file file-or-path)
        s (.getName file)]
    (nth (re-find #"(.*)\.(.*)" s) 1)))

(defn strip-path-extension [path]
  (subs path 0 (.lastIndexOf path ".")))

(defn get-path-extension [path]
  (subs path (inc (.lastIndexOf path ".")) (count path)))

(defn map-columns [f m]
  (mapv (fn [index] (f (m/get-column m index))) 
       (range 0 (second (m/shape m)))))

(defn map-rows [f m]
  (mapv (fn [index] (f (m/get-row m index))) 
       (range 0 (first (m/shape m)))))

(defn indices-such-that [pred coll]
  (keep identity (map-indexed #(when (pred %2) %1) coll)))

(defn keys-such-that
  "Returns m's keys for which (pred (m k)) is true"
  [pred m]
  (reduce-kv
   (fn [keys-satisfiying-pred, k, v]
     (if (pred v)
       (conj keys-satisfiying-pred k)
       keys-satisfiying-pred))
   []
   m))

(defmacro dofolder
  "Executes body for each leaf file in folder"
  [[it folder-path] & body]
  `(let [folder# (io/file ~folder-path)
         files# (file-seq folder#)]
     (doseq [~it files#]
       ~@body)))

(defn mapify [list-of-pairs]
  (apply hash-map (apply concat list-of-pairs)))

(defn mapify-ordered [list-of-pairs]
  (apply array-map (flatten list-of-pairs)))

(defn levenshtein  [w1 w2]
  (letfn [(cell-value [same-char? prev-row cur-row col-idx]
            (min (inc (nth prev-row col-idx))
                 (inc (last cur-row))
                 (+ (nth prev-row  (dec col-idx)) 
                    (if same-char? 0 1))))]
    (loop [row-idx 1
           max-rows (inc (count w2))
           prev-row (range (inc (count w1)))]
      (if (= row-idx max-rows)
        (last prev-row)
        (let [ch2 (nth w2 (dec row-idx))
              next-prev-row (reduce (fn [cur-row i]
                                      (let [same-char? (= (nth w1  (dec i)) ch2)]
                                        (conj cur-row (cell-value same-char?  prev-row cur-row i))))
                                    [row-idx] (range 1 (count prev-row)))]
          (recur (inc row-idx) max-rows next-prev-row))))))

(defn get-files-with-extension [ext path]
  (let [files (.listFiles (clojure.java.io/file path))]
    (filter #(= (extension %) ext) files)))

(defn distinct-by
  "Returns the disticnt elements in coll using eq-fn as the equality function. If two equal
   elements are found they are combined with the result of applying the combinator function."
  [coll eq-fn combinator]
  (vals
    (reduce
      #(if-not (contains? %1 (eq-fn %2))
         (assoc %1 (eq-fn %2) %2)
         (assoc %1 (eq-fn %2) (combinator (get %1 (eq-fn %2)) %2)))
      (sorted-map)
      coll)))

(defn is?
  "Returns a predicate function that invokes key on its value and compares it against val"
  [k v]
  (fn [x] (= (k x) v)))

(update {} :a
        (fn [old new]
          (if (nil? old)
            #{new}
            (conj old new)))
        2)

(defn repeated-values
  "Returns the repeated values in the sequence"
  [lst]
  (map first (filter (fn [[id freq]] (> freq 1)) (frequencies lst))))

(defn repeated-indices
  "Returns the indices of the repeated values in the sequence"
  [lst]
  (let [repeated-values (set (repeated-values lst))]
    (indices-such-that repeated-values lst)))

(defn remove-indices
  "Removes the elements at the specified indices. Not particularly efficient."
  [lst indices]
  (let [m (into (sorted-map) (zip (range) lst))]
    (into [] (vals (reduce dissoc m indices)))))

(defn filter-indices
  "Returns the elements at the specified indices. Not particularly efficient."
  [lst indices]
  (keep (vec lst) indices))

(defn split-while
  [pred coll]
  (let [pre (take-while pred coll)
        n (count pre)]
    [pre (drop n coll)]))

(defn assoc0
  ([m] m)
  ([m & kvs]
   (apply assoc m kvs)))

(defn home-relative
  "Returns path relative to home folder"
  [path]
  (.getAbsolutePath (io/file (System/getProperty "user.home") path)))

(defn mk-progress-bar []
  (let [pbar (doto (JProgressBar.)
               (.setMinimum 0)
               (.setMaximum 100))
        pane (doto (JPanel.)
               (.add pbar))
        frame (doto (JFrame.)
                (.setType java.awt.Window$Type/UTILITY)
                (.setTitle "clj-progress-bar")
                (.setLocationRelativeTo nil)
                (.setContentPane pane)
                (.pack)
                (.setVisible true))]
    {:pbar pbar
     :pane pane
     :frame frame}))

(defn set-progress-bar [bar value]
  (.setValue (:pbar bar) value))

(defn close-progress-bar [bar]
  (.dispose (:frame bar)))

(defmacro let-def
  [defs & body]
  (let [defs (partition 2 defs)
        defs' (map
               (fn spy-on [[x y]]
                 [(symbol "__") `(def ~(symbol (str "--" (name x))) ~x)])
               defs)]
    `(let ~(into [] (mapcat identity (interleave defs defs')))
       ~@body)))

(spec/def :let+/bindings (spec/*
                          (spec/cat :sym :clojure.core.specs/binding-form :dbg (spec/? #(= :dbg %)) :val any?)))

(defmacro let+ [bindings & body]
  (let [bindings* (spec/conform :let+/bindings bindings)
        __ (if (spec/invalid? bindings*)
             (throw (Exception. (str "Invalid let+ form.\n" (spec/explain-str :let+/bindings bindings)))))
        defs (mapcat
              (fn [{:keys [dbg val sym]}]
                (let [;;NOTE: Since we parsed binding forms with the standard spec, we
                      ;;     need to unform it to recover the original value. Additionally,
                      ;;     we need to recover seq binding forms to vectors or let will fail
                      sym (spec/unform :clojure.core.specs/binding-form sym)
                      sym (if (seq? sym) (vec sym) sym)] 
                  (if (and dbg (not (symbol? sym))) (throw (Exception. (str "TODO: :dbg cannot be used with destructure forms:" sym))))
                  (cond-> []
                    true (conj sym val)
                    dbg  (conj (symbol "__") `(def ~(symbol (str "--" (name sym))) ~sym)))))
              bindings*)]
    `(let ~(vec defs)
       ~@body)))

(defn transpose [xs]
  (apply map list xs))

(defn map-progress
  "Same as map, but shows a progress bar window.
  Useful for long operations"
  [fun & [x & _ :as seqs]]
  (let [N (count x)
        step (int (Math/ceil (/ 100.0 N)))
        progress (atom 0)
        bar (mk-progress-bar)
        fun' (fn map-progress-fun [& args]
               (let [retval (apply fun args)]
                 (swap! progress #(+ step %))
                 (set-progress-bar bar @progress)
                 retval))]
        ;; NOTE: It makes no sense to have a progress bar if the
        ;;       computation is lazy, because it will be closed
        ;;       right away
    (try (doall (apply map fun' seqs))
         (catch Exception e (close-progress-bar bar) (throw e))
         (finally (close-progress-bar bar)))))

(defn normalize [v]
  (let [norm (Math/sqrt (reduce + (map #(square %) v)))]
    (mapv #(/ % norm) v)))

(defmacro try-or
  "Tries each of the arguments in order, returns the first non-throwing expression
   or throws the last exception if all expressions throw. Example:
  (try-or (Integer/parseInt x) (Float/parseFloat x) nil)"
  [x & xs]
  (if xs
      `(try ~x
            (catch Throwable e# (try-or ~@xs)))
      x))

(defn concat-seq [seq-of-seqs]
  (if (seq seq-of-seqs)
    (apply concat seq-of-seqs)
    seq-of-seqs))

(defn safe-apply
  "Same as apply but returns default if the number of arguments
   supplied is not correct"
  [fn & {:keys [default, args]
         :or {default nil}}]
  (try-or (apply fn args)
          default))

(defmacro eager-for
  {:style/indent 2}
  [& body]
  `(doall (for ~@body)))

(defn parallel-map
  "Like `pmap', except n workers are used instead"
  ([n f coll]
   (let [rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))
  ([n f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (parallel-map n #(apply f %) (step (cons coll colls))))))


(defmacro if-class
  "Utility macro for conditional compilation. If the fully-qualified 'class'
   symbol corresponds to a class found in the classpath (i.e. eval-ing does not
   throw exception) returns if-branch. Otherwise, else-branch."
  [class if-branch else-branch]
  (try-or (let [x (eval class)]
            if-branch)
          else-branch))

(defn export-bpmn-as-png [model path]
  (let [gen (org.activiti.image.impl.DefaultProcessDiagramGenerator.)
        img (.generateImage gen model "" [] [] 1.0)]
    (javax.imageio.ImageIO/write img "png" (java.io.File. path))))
