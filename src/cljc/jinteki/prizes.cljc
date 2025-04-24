(ns jinteki.prizes)

#?(:clj
   (defmacro load-card-backs [base-card-backs sym]
     "Load the card-backs at compile-time for cljs, from the given data file is present"
     (let [res (clojure.java.io/file "data/card-backs.edn")
           data (or (try
                      (and res (-> res slurp clojure.edn/read-string))
                      (catch Exception e
                        (println "Exc: " e)))
                    {})
           _ (println (str (count (-> res slurp clojure.edn/read-string)) " card backs loaded"))]
       `(def ~sym (merge ~data ~base-card-backs)))))
