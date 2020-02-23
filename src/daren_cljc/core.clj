(ns daren-cljc.core)

(defn zipvector [& args]
  (apply map vector args))

(defmacro flip'
  [f]
  `#(~f %2 %1))

(defmacro flip''
  [f]
  `#(~f %3 %2 %1))

(defmacro infer-map
  [& args]
  {:pre [(every? symbol? args)]}
  `(hash-map
     ~@(interleave
         (map (comp keyword name) args)
         args)))
