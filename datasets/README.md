This repository contains the aligned model-text pair benchmark for the evaluation of our model and text alignment tool. 

# Folder Structure

- The *NewDataset* folder contains the 25 new model-text pairs obtained from the BPM Academic Initiative dataset.

- The *OldDataset* folder contains the 49 model-text pairs that were used in our previous evaluations[1,2].

Each folder contains 4 subfolders:

- *Models*: Contains Bpmn 2.0 (XML) files for the models.
- *Texts*: Contains plain text files for the textual descriptions.
- *Image*: Contains image files (pdf and/or svg) for a visual representation of the models(*)
- *Groundtruth*: Contains json files with the alignment information between tasks and sentences.

(*): The bpmn files may have undergone have some minor syntactical corrections, and thus may differ from the image files.

# Groundtruth Format: 

The json groundtruth format is a document with the following keys:

- "tasks": Contains an array of process node ids (strings). An alignment is only provided for those elements. 

- "num-sentences" is a single integer with the number of sentences in the textual description.

- "alignment" is a sub-document with one key for each of the elements of the node ids. The value is the number of the sentence that the particular node is aligned to, or "no-match" if there is no such sentence.

- "errors" is a sub-document with a similar structure as "alignment", which contains the type of error a particular process node has, if any. The error codes are single-character strings and can be: "m" - missing from the textual description, "w" - its order is not consistent with the text, "a" - the node meaning is ambiguous.

# References 

[1] 2017, Sanchez, Cadmona and Padró. "Aligning Textual and Graphical Descriptions of Processes Through ILP Techniques"
[2] 2017, van der Aa, Leopold, Reijers. "Detecting Inconsistencies between Process Models and Textual Descriptions"
