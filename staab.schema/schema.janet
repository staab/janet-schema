(use ./util)

(var type-registry @{})

(defn define [name &opt opts]
  (let [typedef (merge opts {:name name :schema {:t name}})]
    (when (nil? (typedef :type-is-valid))
      (error ":type-is-valid is required when defining a schema type"))
    (put type-registry name typedef)))

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
        typedef (type-registry t)
        type-is-valid (typedef :type-is-valid)
        iter-type-errors (typedef :iter-type-errors)]
    (when (nil? typedef)
      (error (string "No definition registered for schema type " t)))
    (when (nil? type-is-valid)
      (error (string "type-is-valid is not implemented for schema type " t)))
    (when (not (type-is-valid data))
      (yield
       {:path path
        :code :type-error
        :expected t
        :actual (type data)
        :message (string (summarize data) " is not a " t)}))
    (when (and enum (not (find |(= $ enum) data)))
      (yield
       {:path path
        :code :enum
        :expected t
        :actual (type data)
        :message (string (summarize data) " is not one of " (summarize enum))}))
    (when iter-type-errors
      (iter-type-errors schema data path))))

(defn get-error [schema data &opt path]
  (resume (fiber/new |(iter-errors schema data (or path [])))))

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

(define :nil
  {:type-is-valid nil?})

(define :any
  {:type-is-valid (always true)})

(define :int
  {:type-is-valid int?})

(define :num
  {:type-is-valid number?})

(define :str
  {:type-is-valid string?})

(define :bool
  {:type-is-valid boolean?})

(define :true
  {:type-is-valid |(= $ true)})

(define :false
  {:type-is-valid |(= $ false)})

(define :uuid
  {:type-is-valid (peg-validator uuid-pattern)})

(define :date
  {:type-is-valid (peg-validator date-pattern)})

(define :time
  {:type-is-valid (peg-validator time-pattern)})

(define :datetime
  {:type-is-valid (peg-validator datetime-pattern)})

(define :sequence
  {:type-is-valid sequence?
   :iter-type-errors
   (fn [schema data path]
     (let [items-schema (get schema :items :any)]
       (map-indexed |(iter-errors items-schema $0 [;path $1]) data)))})

(define :map
  {:type-is-valid map?
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
       (each [k prop-schema] (pairs props)
         (when (not (nil? (data k)))
           (iter-errors prop-schema (data k) [;path k])))
       (when closed
         (each [k v] (pairs data)
           (when (not (nil? (props k)))
             (yield
              {:path [;path k]
               :code :extra-property
               :expected :nil
               :actual (type v)
               :message (string k " is not an allowed property")}))))))})
