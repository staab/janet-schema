(use staab.assert/assert)
(use staab/schema)

# standalone schema validation
(assert= nil (check-schema {:a 1}))

# Check deeply nested paths
(let [err (get-error {:a [{:b :str}]} {:a [{:b "hi"} {:b 1}]})]
  (assert= :type-error (err :code))
  (assert= [:a 1 :b] (err :path)))
