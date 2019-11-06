(ns daren-cljc.core)

(defmacro infer-map [& args]
  {:pre [(every? symbol? args)]}
  `(hash-map
     ~@(interleave
         (map (comp keyword name) args)
         args)))
