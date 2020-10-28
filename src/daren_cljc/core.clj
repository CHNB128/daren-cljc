(ns daren-cljc.core)

(defn remove-values
  "Take map and remove keys by predicate on value"
  {:test (fn []
           (let [a {:a 1 :b 2 :c 3}]
             (assert (= (remove-values #(> % 2) a) {:c 3}))))}
  [-map pred]
  (into {} (remove (comp pred second) -map)))

(defn select-keys-by
  "Select only keys, that confirm predicate"
  [map keyseq pred]
  (remove-values (select-keys map keyseq) pred))

(defn select-values
  "Select values from map into seq"
  {:test #(let [a {:a 1 :b 2 :c 3}]
            (assert (= (select-values a [:a :b :c]) [1 2 3]))
            (assert (= (select-values a [:a :b :c :d]) [1 2 3]))
            (assert (= (select-values a [:d]) [])))}
  [-map keyseq]
  (reduce #(if-let [value (-map %2)] (conj %1 value) %1) [] keyseq))

(defn remove-keys
  "Take map and remove keys by predicate on key"
  {:test #(let [a {:a 1 :b 2 :c 3}]
            (assert (= (remove-keys a [:a :b]) {:c 3})))}
  [-map keyseq]
  (into {} (remove (comp #(contains? (set keyseq) %) first) -map)))

(defn distinct-by
  "Return distinct maps from collection by specific key"
  [key col]
  (->> (group-by key col)
       (map #(-> % second first))))

(defn zipvector
  "Zip identical by length vectors"
  {:test #(do
            (assert (= (zipvector [1 2 3] [2 3 4] [3 4 5])
                       [[1 2 3] [2 3 4] [3 4 5]]))
            (assert (= (zipvector [1 2 3] [2 3 4] [3 4 5 6])
                       [[1 2 3] [2 3 4] [3 4 5]])))}
  [& args]
  (apply map vector args))

(defmacro flip
  "Flip function args in reverse way"
  {:test (fn []
           (let [-fn #(> %1 %2)]
             (assert (= (-fn 1 2) ((flip -fn) 2 1)))))}
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
