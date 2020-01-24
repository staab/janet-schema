(use local/utils)
(use multimethod)
(import pseudo-set :as set)


# -----------------------------------------------------------------------------
# Schema type definitions


(var type-registry @{})

(defn define-type [name &opt opts]
  (let [typedef (merge opts {:name name :schema {:t name}})]
    (when (nil? (typedef :type-is-valid))
      (error ":type-is-valid is required when defining a schema type"))
    (put type-registry name typedef)))

(defn peg-validator [pattern]
  (let [pattern (peg/compile ~(* ,pattern -1))]
    |(and (string? $) (peg/match pattern $))))

(def uuid-pattern
  '{:digit (range "09" "af" "AF")
    :digit4 (between 4 4 :digit)
    :digit8 (between 8 8 :digit)
    :digit12 (between 12 12 :digit)
    :main (* :digit8 "-" :digit4 "-" :digit4 "-" :digit4 "-" :digit12 -1)})

(def year-pattern '(between 4 4 (range "09")))
(def month-pattern '(+ (* "0" (range "19")) (* "1" (range "02"))))
(def day-pattern '(+ (* "0" (range "19")) (* (range "12") (range "09")) (* "3" (range "01"))))
(def hour-pattern '(+ (* "0" (range "09")) (* "1" (range "09")) (* "2" (range "03"))))
(def minute-pattern '(+ (* "0" (range "09")) (* (range "15") (range "09"))))
(def second-pattern '(+ (* "0" (range "09")) (* (range "15") (range "09"))))
(def ms-pattern '(between 3 3 (range "09")))
(def date-pattern ~(* ,year-pattern "-" ,month-pattern "-" ,day-pattern))
(def time-pattern ~(* ,hour-pattern ":" ,minute-pattern ":" ,second-pattern "." ,ms-pattern))
(def datetime-pattern ~(* ,date-pattern "T" ,time-pattern "Z"))

(define-type :nil
  {:type-is-valid nil?})

(define-type :any
  {:type-is-valid (always true)})

(define-type :int
  {:type-is-valid int?})

(define-type :num
  {:type-is-valid number?})

(define-type :str
  {:type-is-valid string?})

(define-type :bool
  {:type-is-valid boolean?})

(define-type :true
  {:type-is-valid |(= $ true)})

(define-type :false
  {:type-is-valid |(= $ false)})

(define-type :uuid
  {:type-is-valid (peg-validator uuid-pattern)})

(define-type :date
  {:type-is-valid (peg-validator date-pattern)})

(define-type :time
  {:type-is-valid (peg-validator time-pattern)})

(define-type :datetime
  {:type-is-valid (peg-validator datetime-pattern)})

(define-type :seq
  {:type-is-valid indexed?
   :iter-child-tuples
   (fn [schema data]
     (let [child-schema (get schema :items :any)]
       (each [i x] (enumerate data)
         (yield [child-schema x i]))))})

(define-type :map
  {:type-is-valid dictionary?
   :iter-child-tuples
   (fn [schema data]
     (let [props (get schema :properties {})]
       (each [k child-schema] (pairs props)
         (when (not (nil? (data k)))
           (yield [child-schema (data k) k])))))
   :iter-type-errors
   (fn [schema data path]
     (let [required (get schema :required [])
           props (get schema :properties {})
           closed (get schema :closed false)]
       (each k required
         (when (nil? (data k))
           (yield
            {:path [;path k]
             :code :missing-property
             :expected (get-in schema [k :t])
             :actual :nil
             :message (string k " is a required property")})))
       (when closed
         (each [k v] (pairs data)
           (when (nil? (props k))
             (yield
              {:path [;path k]
               :code :extra-property
               :expected :nil
               :actual (type v)
               :message (string k " is not an allowed property")}))))))})


# -----------------------------------------------------------------------------
# Constraints are properties that can be included on a schema which reference
# disparate data within the data object and impose constraints.


(defmulti iter-constraint-errors (fn [c _] (c :t)))

(defmethod iter-constraint-errors :eq
  [{:paths paths} data]
  (let [xs (mapcat |(reify-path data $) paths)
        {:value expected} (first xs)]
    (each {:path path :value actual} (drop 1 xs)
      (when (not= actual expected)
        (yield
         {:path path
          :code :not-eq
          :expected expected
          :actual actual
          :message (string (summarize actual) " does not equal " (summarize expected))})))))

(defmethod iter-constraint-errors :intersection
  [{:search search :target target} data]
  (let [s (set/create ;(map |($ :value) (reify-path data search)))]
    (each {:path path :value v} (reify-path data target)
      (when (not (set/member? s v))
        (yield
         {:path path
          :code :missing-member
          :expected v
          :actual nil
          :message (string (summarize v) " is not contained in the search path")})))))


# -----------------------------------------------------------------------------
# Schema normalization/validation utils


(defn normalize [schema]
  (cond
   # If it's a schema, we're good
   (and (dictionary? schema) (schema :t)) schema
   # If it's just a type keyword turn it into an object
   (keyword? schema) {:t schema}
   # Sequences are a special case
   (indexed? schema) {:t :seq :items (first schema)}
   # Dictionaries are a special case
   (dictionary? schema) {:t :map :properties schema}
   # All else is invalid
   true (error (string/format "Got invalid schema: %q" schema))))

(defn check-schema [schema]
  (print "WARNING: schema validation is not implemented"))


# -----------------------------------------------------------------------------
# Data validation utils


(defn check-data [schema data &opt path]
  (default path [])
  (let [schema (normalize schema)
        {:t t :enum enum} schema
        typedef (type-registry t)
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
        (check-data child-schema child [;path k])))
    # Check that all constraints hold
    (each constraint (get schema :constraints [])
      (iter-constraint-errors constraint data))))

(defn get-error [schema data &opt path]
  (resume (fiber/new |(check-data schema data (or path [])))))
