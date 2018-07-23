package edu.upc.nlp4bpm_commons.tree;

import edu.stanford.nlp.ling.*;

public class DummyLabel implements Label {

    static DummyLabelFactory factory;
    static {
        factory = new DummyLabelFactory();
    }

    private String label;

    public DummyLabel (String label) {
        this.label = label;
    }

    public LabelFactory labelFactory() {
        return factory;
    }

    public void setFromString(String labelStr) {
        this.label = labelStr;
    }

    public void setValue (String value) {
        this.label = value;
    }

    public String toString () {
        return this.label;
    }

    public String value () {
        return this.label;
    }



}
