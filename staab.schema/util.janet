(defn sequence? [x] (or (tuple? x) (array? x)))
(defn map? [x] (or (table? x) (struct? x)))
(defn comma-join [xs] (string/join xs ", "))
(defn always [x] (fn [& args] x))
(defn map-indexed [f xs] (for i 0 (dec (length xs)) (f (xs i) i)))

(defn summarize [target &opt depth]
  (default depth 3)
  (cond
   (and (= depth 0) (or (sequence? target) (map? target))) (type target)
   (sequence? target)
   (let [res (map |(summarize $ (dec depth)) target)
         len (length res)]
     (if (< len 3)
       (comma-join res)
       (string (comma-join (take 2 res)) " ...and " (- len 3) " more")))
   (map? target)
   (comma-join (map (fn [[k v]] (string k ": " (summarize v (dec depth)))) (pairs target)))
   true (string/format "%q" target)))
