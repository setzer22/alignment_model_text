package edu.upc.nlp4bpm_commons;

import java.util.Arrays;
import java.util.List;

import clojure.java.api.Clojure;
import clojure.lang.Keyword;
import clojure.lang.IFn;

public class FreelingAPI {

    static {
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("edu.upc.nlp4bpm-commons.freeling-api"));
        require.invoke(Clojure.read("edu.upc.nlp4bpm-commons.config"));
    }

    public static String analyze (String text, String lang, String output, String level) {
        IFn analyze = Clojure.var("edu.upc.nlp4bpm-commons.freeling-api", "analyze");
        return (String) analyze.invoke(Keyword.intern("text"), text,
                                       Keyword.intern("lang"), lang,
                                       Keyword.intern("output"), output,
                                       Keyword.intern("level"), level);
    }

    public static String analyzeCached (String text, String lang, String output, String level) {
        IFn analyzeCached = Clojure.var("edu.upc.nlp4bpm-commons.freeling-api", "analyze-cached");
        return (String) analyzeCached.invoke(Keyword.intern("text"), text,
                                             Keyword.intern("lang"), lang,
                                             Keyword.intern("output"), output,
                                             Keyword.intern("level"), level);
    }

    public static String analyzeCachedIfAvailable(String text, String lang, String output, String level) {
        IFn analyzeCached = Clojure.var("edu.upc.nlp4bpm-commons.freeling-api", "analyze-cached-if-available");
        return (String) analyzeCached.invoke(Keyword.intern("text"), text,
                                             Keyword.intern("lang"), lang,
                                             Keyword.intern("output"), output,
                                             Keyword.intern("level"), level);
    }

    /**
       Analyzes a list of labels as string. Returns a list of analyzed labels (see AnalyzedLabel.java for structure details)
       The analysis can be one of: "task", "swimlane" or "other". The analysis type of task is for task labels, swimlane is for
       swimlane/pool labels while type other performs only a token-based analysis without parsing or predicate extraction.
       TODO: WIP: Add "swimlane" analysis type
       TODO: Only works for english
     **/
    @SuppressWarnings("unchecked")
	public static List<AnalyzedLabel> analyzeLabels (List<String> labels, String language, String analysis_type) {
        IFn analyzeLabels = Clojure.var("edu.upc.nlp4bpm-commons.freeling-api", "analyze-labels-java");
        return (List<AnalyzedLabel>) analyzeLabels.invoke(Keyword.intern("labels"), labels,
                                             Keyword.intern("lang"), language,
                                             Keyword.intern("label-type"), analysis_type);
    }

    //NOTE: analyzedLabelsCached cannot be implemented b.c. freeling "Sentence"s cannot be serialized.

    public static void setCredentialsPath (String path) {
        IFn setCredentialsPath = Clojure.var("edu.upc.nlp4bpm-commons.freeling-api", "set-credentials-path!");
        setCredentialsPath.invoke(path);
    }

    public static void setConfig (String key, String val) {
        IFn setConfig = Clojure.var("edu.upc.nlp4bpm-commons.config", "-setConfig");
        setConfig.invoke(key, val);
    }

    public static void getConfig (String key) {
        IFn getConfig = Clojure.var("edu.upc.nlp4bpm-commons.config", "-getConfig");
        getConfig.invoke(key);
    }

    public static void setMode (String mode) {
        IFn setMode = Clojure.var("edu.upc.nlp4bpm-commons.freeling-api", "set-mode");
        setMode.invoke(mode);
    }
    
    public static void main (String[] args) {
    	// Sample input data
    	List<String> labels = Arrays.asList("Deliver customer card", "Process blue form", "Instruct employees");
    	
    	// Call the analyzer module
    	List<AnalyzedLabel> l = analyzeLabels(labels, "en", "task");
    	
    	// Get the results
    	int tk_idx = l.get(0).getHeadOf("predicate"); //Tokens are returned as indices
    	List<Integer> word_idxs = l.get(0).getWordsOf("predicate");
    	edu.upc.Jfreeling.Word w = l.get(0).getSentence().getWords().get(tk_idx); //You can retrieve the token data from the sentence
    	System.out.println(w.getLemma());
    }
}
