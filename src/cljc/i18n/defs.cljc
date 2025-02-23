(ns i18n.defs)

(defmulti render-map (fn [lang input] lang) :default "en")

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro try-catchall
  "A cross-platform variant of try-catch that catches all exceptions.
   Does not (yet) support finally, and does not need or want an exception class."
  [& body]
  (let [try-body (butlast body)
        [catch sym & catch-body :as catch-form] (last body)]
    (assert (= catch 'catch))
    (assert (symbol? sym))
    (if (cljs-env? &env)
      `(try ~@try-body (~'catch js/Object ~sym ~@catch-body))
      `(try ~@try-body (~'catch Throwable ~sym ~@catch-body)))))

(defmacro pprint-to-string
  [val]
  (if (cljs-env? &env)
    `(with-out-str (cljs.pprint/pprint val))
    `(with-out-str (clojure.pprint/pprint val))))
