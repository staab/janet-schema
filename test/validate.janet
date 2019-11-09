(use assert)
(use janet-schema/schema)

(defn assert-type-error [err]
  (assert= :type-error (err :code)))

# nil
(assert= nil (get-error :nil nil))
(assert-type-error (get-error :nil 1))

# any
(assert= nil (get-error :any nil))
(assert= nil (get-error :any 1))
(assert= nil (get-error :any :something))
(assert= nil (get-error :any {:x 3}))
(assert= nil (get-error :any [3 :ok {}]))

# int
(assert= nil (get-error :int 1093))
(assert-type-error (get-error :int 1093.3))
(assert-type-error (get-error :int "hello"))

# num
(assert= nil (get-error :num 1093))
(assert= nil (get-error :num 109.3))
(assert-type-error (get-error :num "hello"))

# str
(assert= nil (get-error :str "hello"))
(assert-type-error (get-error :str :things))

# bool
(assert= nil (get-error :bool true))
(assert= nil (get-error :bool false))
(assert-type-error (get-error :bool nil))

# true
(assert= nil (get-error :true true))
(assert-type-error (get-error :true false))
(assert-type-error (get-error :true nil))

# false
(assert= nil (get-error :false false))
(assert-type-error (get-error :false true))
(assert-type-error (get-error :false nil))

# uuid
(assert= nil (get-error :uuid "2AE485E0-29A5-4EC6-9001-0B1A2CEAF135"))
(assert= nil (get-error :uuid "2ae485e0-29a5-4ec6-9001-0b1a2ceaf135"))
(assert-type-error (get-error :uuid "2ae485e0-29a5-4ec6-9001-0b1a2ceaf13z"))
(assert-type-error (get-error :uuid nil))

# date
(assert= nil (get-error :date "2019-01-20"))
(assert= nil (get-error :date "0001-12-31"))
(assert-type-error (get-error :date "2019-31-12"))
(assert-type-error (get-error :date nil))

# time
(assert= nil (get-error :time "00:00:00.000"))
(assert= nil (get-error :time "23:59:59.000"))
(assert-type-error (get-error :time "28:12:19.102"))
(assert-type-error (get-error :time "23:59:59.0000"))
(assert-type-error (get-error :time nil))

# (define :time
#   {:type-is-valid |(peg/match time-pattern $)})

# (define :datetime
#   {:type-is-valid |(peg/match datetime-pattern $)})

# (define :sequence
#   {:type-is-valid sequence?
#    :iter-type-errors
#    (fn [schema data path]
#      (let [items (get schema :items [])]
#        (map-indexed data |(iter-errors items $0 [;path $1]))))})

# (define :map
#   {:type-is-valid map?
#    :iter-type-errors
#    (fn [schema data path]
#      (let [required (get schema :required [])
#            props (get schema :properties {})
#            closed (get schema :closed false)]
#        (each k required
#          (when (nil? (data k))
#            (yield
#             {:path [;path k]
#              :code :missing-property
#              :expected (get-in schema [k :t])
#              :actual :nil
#              :message (string k " is a required property")})))
#        (each [k prop-schema] (pairs props)
#          (when (not (nil? (data k)))
#            (iter-errors prop-schema (data k) [;path k])))
#        (when closed
#          (each [k v] (pairs data)
#            (when (not (nil? (props k)))
#              (yield
#               {:path [;path k]
#                :code :extra-property
#                :expected :nil
#                :actual (type v)
#                :message (string k " is not an allowed property")}))))))})

# # export default new Proxy(
# #   {
# #     ...ERROR_CODE,
# #     arr: (items, meta) => new Schema("arr", {items, ...meta}),
# #     obj: (properties, meta) => new Schema("obj", {properties, ...meta}),
# #     open: schema => cloneSchema(schema, {closed: false}),
# #     closed: schema => cloneSchema(schema, {closed: true}),
# #     strict: schema => {
# #       const normalizedSchema = normalize(schema)
# #       const requiredKeys = Object.keys(normalizedSchema.properties)

# #       return cloneSchema(normalizedSchema, {requiredKeys})
# #     },
# #   },
# #   {
# #     get(target, name) {
# #       if (target[name]) {
# #         return target[name]
# #       }

# #       const isCapitalized = Boolean(name.match(/^[A-Z][a-z]+$/))
# #       const T = allTypes[name.toLowerCase()]

# #       if (!T) {
# #         throw new Error(`Invalid schema property ${name}`)
# #       }

# #       // If it starts with a capital letter they're treating it as a type,
# #       // if it's lower case, they're treating it as a builder function
# #       return isCapitalized ? T.schema : T.create
# #     },
# #   }
# # )
