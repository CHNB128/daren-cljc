(ns daren-cljc.core)

(defn remove-by-values
  "Take map and remove keys by predicate"
  [pred? hashmap]
  (->> (remove #(pred? (second %)) hashmap)
       (reduce (fn [o [k v]] (assoc o k v)) {})))

(defn zipvector [& args]
  "Zip identical by length vectors
   Example:
   => (zipvector [1 2 3] [2 3 4] [3 4 5])
   => ([1 2 3] [2 3 4] [3 4 5])
   => (zipvector [1 2 3] [2 3 4] [3 4 5 6])
   => ([1 2 3] [2 3 4] [3 4 5])"
  (apply map vector args))

(defmacro flip'
  "Flip function with 2 args"
  [f]
  `#(~f %2 %1))

(defmacro flip''
  "Flip function with 3 args"
  [f]
  `#(~f %3 %2 %1))

(defmacro infer-map
  "Create a map from sequence
   Example:
   => (infer-map name login password)
   => {:name name :login login :password password"
  [& args]
  {:pre [(every? symbol? args)]}
  `(hash-map
    ~@(interleave
       (map (comp keyword name) args)
       args)))
