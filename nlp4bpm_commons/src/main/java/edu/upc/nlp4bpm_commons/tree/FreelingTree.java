package edu.upc.nlp4bpm_commons.tree;

import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.trees.*;
import java.util.List;

public class FreelingTree extends Tree {

    static FreelingTreeFactory factory;
    static {
        factory = new FreelingTreeFactory();
    }

    public Tree[] children;
    public Label label;

    public FreelingTree(Label label, Tree[] children) {
        this.label = label;
        this.children = children;
    }


    @Override
    public Tree[] children() {
        return this.children;
    }

    @Override
    public TreeFactory treeFactory() {
        return this.factory;
    }

    @Override
    public Label label() {
        return this.label;
    }

}
