(use staab.schema/utils)
(use staab.schema/multi)
(import staab.schema/types :as types)
(import staab.schema/constraints :as constraints)

(defn normalize [schema]
  (cond
   # If it's a schema, we're good
   (and (map? schema) (schema :t)) schema
   # If it's just a type keyword turn it into an object
   (keyword? schema) {:t schema}
   # Arrays are a special case
   (sequence? schema) {:t :sequence :items (first schema)}
   # Objects are a special case
   (map? schema) {:t :map :properties schema}
   # All else is invalid
   true (error (string/format "Got invalid schema: %q" schema))))

(defn check-schema [schema]
  (error "Not implemented"))

(defn iter-errors [schema data path]
  (let [schema (normalize schema)
        {:t t :enum enum} schema
        typedef (types/type-registry t)
        _ (when (nil? typedef) (errfmt `No definition registered for "%q"` t))
        type-is-valid (typedef :type-is-valid)
        _ (when (nil? type-is-valid) (errfmt `type-is-valid is not implemented for "%q"` t))
        iter-type-errors (get typedef :iter-type-errors no-op)
        iter-child-tuples (get typedef :iter-child-tuples no-op)]
    # Check that the type is basically correct before we assume anything further
    (when (not (type-is-valid data))
      (yield
       {:path path
        :code :type-error
        :expected t
        :actual (type data)
        :message (string (summarize data) " is not a " t)}))
    # If it's a whitelist, just check equality
    (when (and enum (not (find |(= $ enum) data)))
      (yield
       {:path path
        :code :enum
        :expected t
        :actual (type data)
        :message (string (summarize data) " is not one of " (summarize enum))}))
    # Check type-related errors before recurring
    (iter-type-errors schema data path)
    # Recur into child schemas/data
    (loop [tup :generate (fiber/new |(iter-child-tuples schema data))]
      (if-let [[child-schema child k] tup]
        (iter-errors child-schema child [;path k])))
    # Check that all constraints hold
    (each constraint (get schema :constraints [])
      (constraints/iter-constraint-errors constraint data))))

(defn get-error [schema data &opt path]
  (resume (fiber/new |(iter-errors schema data (or path [])))))
