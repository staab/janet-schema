(defn no-op [& args])
(defn comma-join [xs] (string/join xs ", "))
(defn always [x] (fn [& args] x))

(defn tp [& xs]
  (pp (string/join (map |(if (string? $) $ (string/format "%q" $)) xs) " "))
  (last xs))

(defn enumerate [xs]
  (let [result @[]]
    (loop [i :range [0 (length xs)]]
      (array/push result [i (xs i)]))
  result))

(defn map-indexed [f xs]
  (map
   (fn [[k v]] (f k v))
   (if (dictionary? xs) (pairs xs) (enumerate xs))))

(def- slice-pattern (peg/compile '(* "[" (<- (any (range "09"))) ":" (<- (any (range "09"))) "]" -1)))

(defn- get-slice-indexes [str]
  (if-let [[from to] (peg/match slice-pattern str)]
    [(if (empty? from) 0 (scan-number from))
     (if (empty? to) -1 (scan-number to))]))

(defmacro errfmt [& args] ~(error (string/format ,;args)))

(defn summarize [target &opt depth]
  (default depth 4)
  (cond
   (and (= depth 0) (or (indexed? target) (dictionary? target))) (type target)
   (indexed? target)
   (let [res (map |(summarize $ (dec depth)) target)
         len (length res)]
     (string/format
      "[%s]"
      (if (< len 4)
        (comma-join res)
        (string (comma-join (take 2 res)) " ...and " (- len 3) " more"))))
   (dictionary? target)
   (string/format
    "{%s}"
    (comma-join (map (fn [[k v]] (string k ": " (summarize v (dec depth)))) (pairs target))))
   true (string/format "%q" target)))

(defn reify-path
  "Takes data and a tuple of keys into the data and returns a tuple of tuples,
   each of which is a concrete path into the data and a value. Keys may be
   keys into an associative or indexed data structure, or one of the following:

   - [:] returns all elements of an array/tuple, or all values of a table/struct.
   - [a:b] returns all elements in the given slice of an array/tuple"
  [data pathspec &opt p]
  (default p [])
  (when (not (indexed? pathspec))
    (errfmt "Pathspec must be a sequence (not `%q`)" pathspec))
  (let [head (first pathspec)
        tail (drop 1 pathspec)
        descend (fn [[k v]]
                        (if (empty? tail)
                          {:path [;p k] :value v}
                          (reify-path v tail [;p k])))
        ]
    (flatten
     (if-let [[from to] (get-slice-indexes head)
              to (max (length data) to)]
      (map descend (array/slice (enumerate data) from to))
      [(descend [head (get data head)])]))))
