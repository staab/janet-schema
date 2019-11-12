(defn sequence? [x] (or (tuple? x) (array? x)))
(defn map? [x] (or (table? x) (struct? x)))
(defn comma-join [xs] (string/join xs ", "))
(defn always [x] (fn [& args] x))
(defn when-not [c & body] (when (not c) ;body))
(defn no-op [& args])

(defn map-indexed [f xs]
  (let [result @[]]
    (for i 0 (dec (length xs))
      (array/push result (f (xs i) i)))
    (tuple ;result)))

(defn get-path [data [head tail] path]
  (cond
   (= head "[]") (map-indexed |(do {}) data))))

(defmacro errfmt [& args] ~(error (string/format ,;args)))

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
