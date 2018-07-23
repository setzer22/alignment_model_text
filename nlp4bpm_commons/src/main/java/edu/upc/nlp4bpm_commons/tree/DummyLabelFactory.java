package edu.upc.nlp4bpm_commons.tree;

import edu.stanford.nlp.ling.*;

public class DummyLabelFactory implements LabelFactory {
    public Label newLabel (Label oldLabel) {
        return new DummyLabel(oldLabel.value());
    }

    public Label newLabel (String labelStr) {
        return new DummyLabel(labelStr);
    }

    public Label newLabel(String labelStr, int options) {
        return new DummyLabel(labelStr);
    }

    public Label newLabelFromString (String encodedLabelStr) {
        return new DummyLabel(encodedLabelStr);
    }
}
