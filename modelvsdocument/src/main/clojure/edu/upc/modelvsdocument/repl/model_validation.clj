(ns edu.upc.modelvsdocument.model-validation)

(defmacro section
  {:style/indent 1}
  [& body]
  `(do ~@body))

(defmacro scratch
  {:style/indent 1}
  [& body]
  `(comment ~@body))

(section "First of all we should define the properties of a well-formed model."

  "In this context, we'll say a model is well formed whenever it satisfies the following
   list of properties"

  "1. Only one start task per process (swimpool)"
  "2. At least one end task per process (swimpool)"
  "3. Tasks and events have always incoming and outgoing degree 1."
  "4. Gateways have either incoming degree 1 or outgoing degree 1. Both can't be > 1."
  "5. A messageflow is always originated inside a task or an event and always goes to a task or an event. Never a gateway."
  "6. ")
