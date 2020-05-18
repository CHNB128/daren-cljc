(ns daren-cljc.core
  (:require [clojure.set :as cset]))

(defn distinct-by [key col]
  "Take distinct maps from collection by specific key"
  (->> (cset/index [key])
       (map #(-> % second first))))

(defn remove-by
  "Take map and remove keys by predicate on value"
  [pred map]
  (->> (remove #(pred (second %)) map)
       (reduce (fn [o [k v]] (assoc o k v)) {})))

(defn zipvector [& args]
  "Zip identical by length vectors
   Example:
   => (zipvector [1 2 3] [2 3 4] [3 4 5])
   => ([1 2 3] [2 3 4] [3 4 5])
   => (zipvector [1 2 3] [2 3 4] [3 4 5 6])
   => ([1 2 3] [2 3 4] [3 4 5])"
  (apply map vector args))

(defmacro flip
  "Flip function args in reverse way"
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
