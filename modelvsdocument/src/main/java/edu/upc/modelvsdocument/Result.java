package edu.upc.modelvsdocument;

import java.util.List;

public class Result {
    private double total_score;
    private List<Match> matches;
    private String log;
    private Object clojureExtraData;

    public Result (double total_score, String log,  List<Match> matches, Object clojureExtraData) {
        this.total_score = total_score;
        this.matches = matches;
        this.log = log;
        this.clojureExtraData = clojureExtraData;
    }

    // Returns the match information for a given sentence and task pair.
    @Deprecated
    public Match getmatch(int sentenceIndex, String taskId) {
        //NOTE: Left for compatibility
        //TODO: Remove when unused
        return getMatch(taskId);
    }

    public Match getMatch(String taskId) {
        for (Match m : matches) {
            if(m.getTaskId() == taskId) {
                return m;
            }
        }
        return null;
    }

    public double getTotalScore() {return total_score;}
    public List<Match> getMatches() {return matches;}
    public String getLog() {return log;}
    public Object getClojureExtraData() {return clojureExtraData;}
}
