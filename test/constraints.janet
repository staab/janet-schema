(use staab.assert/assert)
(use staab/schema)

(assert=
 {:code :not-eq
  :path [:a 2 :b]
  :expected 2
  :actual 3
  :message "3 does not equal 2"}
 (get-error
   {:t :map :constraints [{:t :eq :paths [[:a "[1:]" :b] [:c]]}]}
   {:a [{:b 1} {:b 2} {:b 3}] :c 2}))

(assert=
 {:code :missing-member
  :path [:b 1]
  :expected 2
  :message "2 is not contained in the search path"}
 (get-error
   {:t :map :constraints [{:t :intersection :search [:a "[:]"] :target [:b "[:]"]}]}
   {:a [1 3] :b [1 2 3]}))
