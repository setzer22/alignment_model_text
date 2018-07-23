package edu.upc.nlp4bpm_commons;

import java.util.List;
import edu.upc.Jfreeling.Sentence;

/**

   This interface represents an analyzed label. An analyzed label has an associated Freeling analyzed
   sentence, which can ge retrieved with getSentence(). Additionally, has two methods getHeadOf and
   getWordsOf which, given an argument type return the main word token index and the list of token indices
   for the requested argument.

   For Task Labels, (analysis type: "task") argument types can be:

   - "predicate": The main action, usually the verb.
   - "object": The "patient" to which the action is directed, usually the direct object.
   - "complement": Other kinds of complements describing *how*, *when* or *where* the action takes place.

   For Swimlane or Pool labels (analysis type: "swimlane"), a single argument type can be obtained:

   - "agent": The entity indicated by the swimlane. Usually a noun phrase

 **/
public interface AnalyzedLabel {

    public Sentence getSentence();

    public Integer getHeadOf(String argument_type);

    public List<Integer> getWordsOf(String argument_type);

}
