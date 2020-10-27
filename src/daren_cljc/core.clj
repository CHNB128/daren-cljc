(ns daren-cljc.core)

(defn remove-keys
  "Take map and remove keys by predicate on key"
  {:test #(let [a {:a 1 :b 2 :c 3}]
            (assert (= (remove-keys a [:a :b]) {:c 3})))}
  [-map keycoll]
  (into {} (remove (comp #(contains? (set keycoll) %) first) -map)))

(defn remove-values
  "Take map and remove keys by predicate on value"
  {:test #(let [a {:a 1 :b 2 :c 3}]
            (assert (= (remove-values #(> % 2) a) {:c 3})))}
  [pred -map]
  (into {} (remove (comp pred second) -map)))

(defn distinct-by
  "Return distinct maps from collection by specific key"
  [key col]
  (->> (group-by key col)
       (map #(-> % second first))))

(defn zipvector
  "Zip identical by length vectors"
  {:test #(do
            (assert (= (remove-values [1 2 3] [2 3 4] [3 4 5])
                       [[1 2 3] [2 3 4] [3 4 5]]))
            (assert (= (remove-values [1 2 3] [2 3 4] [3 4 5 6])
                       [[1 2 3] [2 3 4] [3 4 5]])))}
  [& args]
  (apply map vector args))

(defmacro flip
  "Flip function args in reverse way"
  {:test #(let [-fn #(> %1 %2)]
            (assert (= (-fn 1 2) ((flip -fn) 2 1))))}
  [f]
  `#(apply ~f (reverse %&)))

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
