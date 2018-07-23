package edu.upc.modelvsdocument;

import java.util.List;

public class SentenceIdsPair {
    
    private int sentenceIndex;
    private List<String> ids;

    public SentenceIdsPair(int sentenceIndex, List<String> ids) {
        this.sentenceIndex = sentenceIndex;
        this.ids = ids;
    }

    public int getSentenceIndex() {return sentenceIndex;}

    public List<String> getTaskIds() {return ids;}

    @Override
    public String toString() {
        String ids_string = "{";
        for(int i = 0; i < ids.size(); ++i) {
            ids_string += ids.get(i) + ",";
        }
        ids_string += "}";

        return "Sentence "+sentenceIndex+ " -> " + ids_string;
    }
    
}
