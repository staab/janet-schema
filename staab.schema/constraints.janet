(use staab.schema/utils)
(use staab.schema/multi)
(import staab.schema/set :as set)

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
