(ns edu.upc.modelvsdocument.alignable)

"The alignable protocol encapsulates all kinds of object that can be aligned by our
 solver algorithm. An alignable is defined only inside its context: For example, a text
 (the context) may have sentences as alignables."

(defprotocol Alignable
  (label [this context] [this] "Prints a human-readable representation of the alignable")
  (id [this] "Returns a unique identifier of the alignable whithin its context"))

(defn alignable? [x]
  (satisfies? Alignable x))

(defprotocol Context
  (get-element [this a-id] "Returns an alignable given its ID")
  (get-all-alignables [this] "Returns all alignable elements. The order of the elements between calls must be deterministic.")
  (get-output-alignables [this] "Returns the alignable elements that are part of the output"))
