(ns daren-cljc.core)

(defn flip
  [f]
  (fn [& args] (apply f (reverse args))))

(defmacro infer-map
  [& args]
  {:pre [(every? symbol? args)]}
  `(hash-map
     ~@(interleave
         (map (comp keyword name) args)
         args)))
