(use assert)
(use schema)

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

# datetime
(assert= nil (get-error :datetime "2019-02-29T08:10:00.023Z"))
(assert-type-error (get-error :datetime "2019-02-29 08:10:00.023"))
(assert-type-error (get-error :datetime "23:59:59.000"))
(assert-type-error (get-error :datetime nil))

# seq
(assert= nil (get-error :seq []))
(assert= nil (get-error :seq @[]))
(assert= nil (get-error :seq [1 2 3]))
(assert= nil (get-error [:int] [1 2 3]))
(assert-type-error (get-error :seq "asdf"))
(assert-type-error (get-error :seq {:a 1}))
(assert-type-error (get-error [:int] [{:a 1}]))

# map
(assert= nil (get-error :map {}))
(assert= nil (get-error :map @{}))
(assert= nil (get-error :map {:a 1}))
(assert= nil (get-error {:a :int} {:a 1}))
(assert-type-error (get-error :map "asdf"))
(assert-type-error (get-error :map [1 2 3]))
(assert-type-error (get-error {:a :str} {:a 1}))
(assert= :extra-property ((get-error {:t :map :closed true} {:a 1}) :code))
