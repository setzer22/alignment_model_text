package edu.upc.modelvsdocument;

import java.util.List;

public class Match {

    String sentence;
    String taskId;
    String sentenceId;
    String taskName;
    List<String> commonFeatures;
    double score;
    boolean isGood;

    public Match (String sentenceId, String sentence, String taskId, String taskName, List<String> commonFeatures, double score, boolean isGood) {
        this.sentence = sentence;
        this.sentenceId = sentenceId;
        this.taskId = taskId;
        this.commonFeatures = commonFeatures;
        this.isGood = isGood;
        this.score = score;
        this.taskName = taskName;
    }

    public String getSentenceId() {return sentenceId;}
    public String getSentence() {return sentence;}
    public String getTaskId() {return taskId;}
    public String getTaskName() {return taskName;}
    public List<String> getCommonFeatures() {return commonFeatures;}
    public double getScore() {return score;}
    public boolean isGood() {return isGood;}
}
