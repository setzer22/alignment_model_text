package edu.upc.modelvsdocument;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import java.io.File;

public class StandaloneMain {

    static {
        require("edu.upc.nlp4bpm-commons.core");
        require("edu.upc.nlp4bpm-commons.freeling-api");
        require("edu.upc.nlp4bpm-commons.cache");

        require("edu.upc.modelvsdocument.core");
        require("edu.upc.modelvsdocument.wordnet");
        require("edu.upc.modelvsdocument.config");
        require("edu.upc.modelvsdocument.verification.new-groundtruth");
    }

    public static void require(String ns) {
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read(ns));
    }

    public static void init() {
        IFn set_fl_mode = Clojure.var("edu.upc.nlp4bpm-commons.freeling-api", "set-mode");
        IFn init_fl = Clojure.var("edu.upc.nlp4bpm-commons.core", "init-freeling");
        IFn init_cache = Clojure.var("edu.upc.nlp4bpm-commons.cache", "initialize");
        IFn read_wordnet = Clojure.var("edu.upc.modelvsdocument.wordnet", "read-wordnet-dictionaries");
        IFn set_config = Clojure.var("edu.upc.modelvsdocument.config", "set-value-in-config");

        // PLEASE CHANGE THE NECESSARY CONFIG PARAMETERS BELOW

        // FreeLing parameters
        init_fl.invoke();
        set_fl_mode.invoke("local"); // Online mode will use the textserver and avoid a local FreeLing installation.
                                      // This makes can make the NLP considerably slower.

        // Cache parameters
        init_cache.invoke(System.getProperty("user.home") + "/.textserver-cache.clj"); // This will enable a cache file at the
                                                                                       // User's home folder. The cache is used
                                                                                       // to avoid running FreeLing twice for the same file

        // Wordnet dictionaries
        read_wordnet.invoke(System.getProperty("user.home") + "/repos/alignment_model_text/modelvsdocument/wordnet"); // The path where the wordnet senses30.src
                                                                                                  // and wn30.src files are located.

        // Misc. configuration parameters
        set_config.invoke("use-gurobi", true); // Whether to use the gurobi solver. NOTE: This is now the only supported option
        set_config.invoke("enable-cache", true); // Whether the cache is active.
        set_config.invoke("enable-anchors", true); // Whether to enable the anchor points feature.
        set_config.invoke("gurobi-tmp-path", "BPMN/tmp"); // The path where the gurobi solver temporary files will be stored. This is
                                                          // relative to the user's home folder.

        // The feature family weights.
        Object weightOverrides = Clojure.read("{:has-lemma 0.01226, :has-parent-synset 0.03927, :in-agent 0.19403," +
                                              ":agent-head 0.03089, :has-form 0.82536, :patient-head 0.37729," +
                                              ":in-patient 0.84569, :has-discourse-marker 0.83401, :has-synset 0.5691," +
                                              ":has-action 0.95106}");

        set_config.invoke("feature-weight-overrides", weightOverrides);

        set_config.invoke("enable-cutoff", true); // Use missing task predictors
        set_config.invoke("threshold-missing", 0.1); // The threshold for missing task predictors

        set_config.invoke("enable-wrong-order-cutoff", true); // Use wrong order predictors
        set_config.invoke("max-constrained-threshold", 0.99); // Threshold for wrong order predictors

        set_config.invoke("return-trivial-alignment", false); // If this is set, order constraints won't be used
        set_config.invoke("remove-back-edges", true); // Remove loops' back edges for the behavioral profile computation

    }

    public static Object parseGroundtruth(File groundtruthFile) {
        IFn slurp = Clojure.var("clojure.core", "slurp");
        IFn load_json_gt = Clojure.var("edu.upc.modelvsdocument.verification.new-groundtruth", "load-json");

        return load_json_gt.invoke(slurp.invoke(groundtruthFile.getAbsolutePath()));
    }

    public static Object compareAlignments(Object groundtruthAlignment, Result result) {
        IFn compare_gts = Clojure.var("edu.upc.modelvsdocument.verification.new-groundtruth", "compare-extended-groundtruths");
        // NOTE: The first parameter must be set to the same value as :enable-anchors!!!!
        return compare_gts.invoke(true,
                                  ((java.util.Map)result.getClojureExtraData()).get(Clojure.read(":model")),
                                  ((java.util.Map)result.getClojureExtraData()).get(Clojure.read(":extended-match-struct")),
                                  groundtruthAlignment);
    }

    public static Result computeAlignment(File modelFile, File textFile) {
        IFn main_with_files = Clojure.var("edu.upc.modelvsdocument.core", "main-with-files");
        return (Result)main_with_files.invoke(modelFile, textFile);
    }

    public static void main(String[] args) {
        if (!((args[0].equals("execution") && args.length == 3) || (args[0].equals("evaluation") && args.length == 4))) {
            System.err.println("Usage:  \n\tstandalonemain execution <model-path> <text-path>\n\tstandalonemain evaluation <model-path> <text-path> <groundtruth-path>");
        } else {
	    if (args[0].equals("evaluation")) {
                File modelFile = new File(args[1]);
                File textFile = new File(args[2]);
                File groundtruthFile = new File(args[3]);
                init();
                Result result = computeAlignment(modelFile, textFile);
                Object groundtruthAlignment = parseGroundtruth(groundtruthFile);
                Object comparison = compareAlignments(groundtruthAlignment, result);

                IFn pprint = Clojure.var("clojure.pprint", "pprint");
                pprint.invoke(comparison);

                System.exit(0);
            }
	    else if (args[0].equals("execution")) {
                File modelFile = new File(args[1]);
                File textFile = new File(args[2]);
                init();
                Result result = computeAlignment(modelFile, textFile);
                System.out.println(result.getLog());

                System.exit(0);

            }
        }
    }


}
