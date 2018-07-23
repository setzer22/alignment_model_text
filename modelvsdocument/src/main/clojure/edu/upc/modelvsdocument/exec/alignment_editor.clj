(ns edu.upc.modelvsdocument.exec.alignment-editor
  (:require [com.rpl.specter :refer :all]
            [edu.upc.nlp4bpm-commons.cache :as cache]
            [edu.upc.modelvsdocument.utils :refer :all]
            [edu.upc.modelvsdocument.alignable :as a]
            [edu.upc.modelvsdocument.text :as text]
            [edu.upc.modelvsdocument.config :as config]
            [edu.upc.modelvsdocument.predictors :as predictors]
            [edu.upc.nlp4bpm-commons.freeling-api :as freeling-api]
            [edu.upc.modelvsdocument.wordnet :as wordnet]
            [edu.upc.modelvsdocument.bpmn-alignable :as bpmn]
            [edu.upc.modelvsdocument.verification.new-groundtruth :as groundtruth]
            [edu.upc.modelvsdocument.gt-structure :as gt-structure]
            [clojure.java.io :as io])
  (:import [javax.swing JFrame JLabel JPanel BoxLayout JList JRadioButton ButtonGroup ImageIcon SwingConstants JButton Box JComponent JScrollPane]
           [java.awt BorderLayout FlowLayout Font]
           [java.awt.event ActionListener])
  (:gen-class
   :name edu.upc.modelvsdocument.AlignmentEditor
   :main true))

(defn mk-window [^String title ^long width ^long height]
  (doto (JFrame. title)
    (.setSize width height)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE) ;TODO: Replace with EXIT_ON_CLOSE for release
    (.setVisible true)))

(defmacro apply-cutre [fun object args]
  `(case (count ~args)
     0 (~fun ~object)
     1 (~fun ~object (first ~args))
     2 (~fun ~object (first ~args) (second ~args))
     3 (~fun ~object (first ~args) (second ~args) (nth ~args 2))))

(defn add! [container, element & args]
  (let [container
        (cond
          (instance? JFrame container) (.getContentPane container)
          :else container)]
    (apply-cutre .add container (cons element args))
    (.invalidate container)
    (.validate container)
    (.repaint container)))

(defn mk-panel [] (JPanel.))

(defn set-box-layout [panel axis]
  (let [box (BoxLayout. panel axis)]
    (.setLayout panel box)))

(defmulti build-ui* first)

(defmethod build-ui* :l [[l s]]
  (JLabel. s))

(defmethod build-ui* :r [[radio data label]]
  (doto (JRadioButton. label)
    (.setActionCommand (str data))))

(defmethod build-ui* :vradio [[vradio {:keys [on-changed]} & children]]
  (let [radio-buttons (map build-ui* children)
        button-group (ButtonGroup.)
        panel (mk-panel)
        action-listener (reify
                          ActionListener
                          (actionPerformed [this ev] (on-changed (-> ev .getActionCommand read-string))))]
    (set-box-layout panel BoxLayout/Y_AXIS)
    (doseq [r radio-buttons]
      (.add button-group r)
      (.addActionListener r action-listener)
      (add! panel r)) 
    (.setVisible panel true)
    panel))

(defmethod build-ui* :v [[v {:keys [alignment] :or {alignment JComponent/CENTER_ALIGNMENT}} & children]]
  (let [panel (mk-panel)]
    (set-box-layout panel BoxLayout/Y_AXIS)
    (doseq [c children]
      (add! panel (doto (build-ui* c)
                    (.setAlignmentX alignment))))
    (.setVisible panel true)
    panel))

(defmethod build-ui* :h [[h {:keys [alignment] :or {alignment JComponent/CENTER_ALIGNMENT}} & children]] #_@CopyPasted
  (let [panel (mk-panel)]
    (set-box-layout panel BoxLayout/X_AXIS)
    (doseq [c children]
      (add! panel (doto (build-ui* c)
                    (.setAlignmentY alignment))))
    (.setVisible panel true)
    panel))

(defmethod build-ui* :scroll [[scrl child]]
  (JScrollPane. (build-ui* child)))

(defmethod build-ui* :comp [[comp component]]
  component)

(defmethod build-ui* :ico [[ico path]]
  (JLabel. (ImageIcon. path)))

(defmethod build-ui* :button [[button {:keys [on-press]} label]]
  (doto (JButton. label)
    (.addActionListener (reify ActionListener
                          (actionPerformed [this ev]
                            (on-press))))))

(defmethod build-ui* :vspace [[vspace amount]]
  (Box/createVerticalStrut amount))

(defmethod build-ui* :hspace [[vspace amount]]
  (Box/createHorizontalStrut amount))


(defn build-ui [ui-spec]
  (let [window (mk-window "Alignment Viewer" 1600 900)
        panel (build-ui* ui-spec)]
    (add! window panel)
    (.setVisible window true)))


(defn assoc-toggle [m k v]
  (if (= (m k) v)
    (dissoc m k)
    (assoc m k v)))

(defn alignment-fix [alignment tasks]
  (->> (merge
        (zipmap tasks (repeat ::predictors/missing))
        alignment)
       seq
       (map (fn [[a b]] [b a]))
       (into [])))

(defn json->alignment
  "Converts a loaded exported JSON file into a real-time alignment"
  [json model text]
  (let [data (groundtruth/load-json json)]
    (reduce
     (fn [alignment [t-id s-id]]
       (if (= s-id :gt/no-match)
         alignment
         (let [task (a/get-element model t-id)
               sentence (a/get-element text (-> s-id inc str))]
           (assoc alignment task sentence))))
     {}
     (:gt/alignment data))))

(defn run [text-path model-path]
  (let [
        sentence-label (doto (JLabel. "No selected sentence")
                         (.setHorizontalAlignment SwingConstants/CENTER))
        task-label (doto (JLabel. "No selected task")
                     (.setHorizontalAlignment SwingConstants/CENTER))
        task-icon (doto (build-ui* [:ico "./res/empty-task.png"]) 
                    (.setLayout (BorderLayout.))
                    (add! task-label BorderLayout/CENTER))
        alignment-label (JLabel. "")

        alignment (atom {})
        __ (def --alignment alignment)


        selected-task (atom nil)
        selected-sentence (atom nil)

        align-button (JButton. "Align")
        set-button-text #(cond
                           (and
                            @selected-sentence
                            @selected-task
                            (= (@alignment @selected-task)
                               @selected-sentence))
                           ,,(.setText align-button "Undo Align")
                           (and
                            (@alignment @selected-task))
                           ,,(.setText align-button "Redefine Align")
                           :else
                           (.setText align-button "Align"))

        __ (println "Analyzing text...")
        text (text/analyze :text (slurp text-path))
        __ (def --text text)
        __ (println "Analyzing model...")
        model (bpmn/analyze model-path)
        __ (def --model model)
        __ (println "Done, saving in local cache...")

        __ (cache/saveCache)

        ;; LOAD ALIGNMENT IF PRESENT IN FOLDER TODO: WIL:
        alignment-path (str (strip-path-extension model-path) "-alignment.json")
        __ (if (.exists (io/file alignment-path))
             (swap! alignment
                    (constantly (json->alignment
                                 (slurp alignment-path)
                                 model
                                 text))))

        a-label (fn a-label-proxy [alignable]
                  (if (and (not (bpmn/task? alignable)) (.startsWith (a/label alignable) "Unnamed"))
                    (let [next-label (some->> alignable .getOutgoingFlows first .getTargetRef
                                              (a/get-element model)
                                              a-label-proxy)]
                      (str "[" (get-path-extension (.getName (type alignable))) "] -> " next-label))
                    (a/label alignable)))

        __ (.addActionListener
            align-button
            (reify ActionListener
              (actionPerformed [this ev]
                (when (and
                       @selected-sentence
                       @selected-task)
                  (swap! alignment assoc-toggle
                         @selected-task
                         @selected-sentence)
                  (set-button-text)
                  (.setText alignment-label (str "<html>Alignment:<br/>"
                                                 (with-out-str
                                                   (doseq [[t s] (transform [ALL ALL] a-label @alignment)]
                                                     (println t " &lt -&gt " s "<br/>")))
                                                 "</html>"))))))

        on-changed-sentence (fn [{:keys [sentence id]}]
                              (.setFont sentence-label (Font. "Serif" Font/ITALIC 18))
                              (.setText sentence-label (str "<html> <p>" sentence "</p></html>"))
                              (reset! selected-sentence (a/get-element text id))
                              (set-button-text))

        on-changed-task (fn [{:keys [task id]}]
                          (.setText task-label task)
                          (reset! selected-task (a/get-element model id))
                          (set-button-text))

        tasks (a/get-all-alignables model)
        sentences (remove text/dummy? (a/get-all-alignables text))]
    (.setText alignment-label (str "<html>Alignment:<br/>"
                                   (with-out-str
                                     (doseq [[t s] (transform [ALL ALL] a-label @alignment)]
                                       (println t " &lt -&gt " s "<br/>")))
                                   "</html>"))
    (build-ui [:scroll
               [:v {:alignment JComponent/CENTER_ALIGNMENT}
                [:vspace 10]
                [:comp sentence-label]
                [:vspace 20]
                [:l "Aligned to:"]
                [:vspace 20]
                [:comp task-icon]
                [:h {}
                 [:v {}
                  [:l "Model Elements:"]
                  (apply
                   conj
                   [:vradio {:on-changed on-changed-task}]
                   (for [t tasks]
                     [:r {:task (a-label t)
                          :id (a/id t)}
                      (a-label t)]))]
                 [:v {}
                  [:l "Sentences:"]
                  (apply
                   conj [:vradio {:on-changed on-changed-sentence}]
                   (for [s sentences]
                     [:r {:id (a/id s)
                          :sentence (a-label s)}
                      (a-label s)]))]]
                [:h {}
                 [:comp align-button]
                 [:hspace 10]
                 [:button {:on-press #(spit
                                       alignment-path
                                       (groundtruth/export-json
                                        (:extended-match-struct
                                         (gt-structure/compute-extended-gt-structure
                                          sentences
                                          tasks
                                          (alignment-fix @alignment tasks)))))}
                  "Export"]]
                [:comp alignment-label]]])))

(defn error-msg [msg]
  (javax.swing.JOptionPane/showMessageDialog nil msg "Error" javax.swing.JOptionPane/ERROR_MESSAGE)
  (throw (Exception. msg)))

(defn -main [& [path & args]]
  (freeling-api/set-mode "online")
  (freeling-api/set-credentials-path! (str (if path (str path "/") "") "./textserver-credentials"))
  (cache/initialize (str (or path ".") "/" "./textserver-cache.clj"))
  (config/with-config {:use-gurobi false, :enable-cache true, :enable-anchors true, :use-vu-similarity false}
    (let [[model-file & _] (sort-by #(.getName %) (get-files-with-extension "bpmn" (str (if path (str path "/") "") "./")))
          [text-file & _] (sort-by #(.getName %) (get-files-with-extension "txt" (str (if path (str path "/") "") "./")))]
      (when-not model-file (error-msg "No model file found. Make sure there is a .bpmn file in this folder"))
      (when-not text-file (error-msg "No text file found. Make sure there is a .txt file in this folder"))
      (println "Loading model " (.getAbsolutePath model-file))
      (println "Loading text " (.getAbsolutePath text-file))
      (run (.getAbsolutePath text-file) (.getAbsolutePath model-file)))))

;;(-main "/home/josep/VM_Shared/AlignmentEditor")

(comment
  PRELOAD CACHE

  ;;TODO
  (require '[edu.upc.nlp4bpm-commons.cache :as cache])
  (edu.upc.nlp4bpm-commons.freeling-api/set-mode "local")

  (let [text-files (get-files-with-extension "txt" "/home/josep/DSS_BENCHMARK/Texts")]
    (cache/initialize "/home/josep/DSS_BENCHMARK/textserver-cache.clj")
    (config/with-config {:enable-cache true}
      (doseq [t text-files]
        (println t)
        (text/analyze :text (slurp t))))
    (cache/saveCache))

  END)

(comment

  @--alignment


  (def --json (groundtruth/load-json (slurp "/home/josep/VM_Shared/AlignmentEditor/1650735497_rev6-alignment.json")))

  --json

  --json

  (defn json->real-time-alignment [json model text]
    "Loads the JSON alignment and defines a structure representing the alignment for the current session."
    (let ))

  (json->alignment (slurp "/home/josep/VM_Shared/AlignmentEditor/Zoo-alignment.json")
                   --model
                   --text)

  )





