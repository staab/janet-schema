(use staab.schema/utils)
(use staab.schema/multi)

(defmulti iter-constraint-errors (fn [c _]) (c :t))

(defmethod iter-constraint-errors :eq [{:paths paths} data]
  (let [xs (map |(get-path data $) paths))
    )
