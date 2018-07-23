package edu.upc.nlp4bpm_commons.tree;

import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.trees.*;
import java.util.List;

public class FreelingTreeFactory implements TreeFactory {

    public Tree newLeaf(Label label) {
        Tree[] children = new Tree[0];
        FreelingTree tree = new FreelingTree(label, children);
        return tree;
    }

    public Tree newLeaf (String word) {
        Tree[] children = new Tree[0];
        FreelingTree tree = new FreelingTree(new DummyLabel(word), children);
        return tree;
    }

    public Tree newTreeNode(Label label, List<Tree> children) {
        return new FreelingTree(label, children.toArray(new Tree[children.size()]));
    }

    public Tree newTreeNode(String parent, List<Tree> children) {
        return new FreelingTree(new DummyLabel(parent), children.toArray(new Tree[children.size()]));
    }
}
