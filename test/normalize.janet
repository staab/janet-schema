(use assert)
(use schema)

# Handles tuples, and does not recur
(assert= (normalize [{:x :int}]) {:t :seq :items {:x :int}})

# Handles structs, and does not recur
(assert= (normalize {:x [:int]}) {:t :map :properties {:x [:int]}})

# Is idempotent
(assert= (normalize {:x [:int]}) (normalize (normalize {:x [:int]})))

# Turns type keyword into an object
(assert= (normalize :int) {:t :int})

# Raises on invalid value
(assert-err (normalize "something"))
