(ns jinteki.i18n.fluent
  (:require
   ["@fluent/bundle" :refer [FluentBundle FluentDateTime FluentNone FluentNumber FluentResource]]))

(defn custom-functions [_locale-str]
  {:NUMBER (fn NUMBER
             [[arg] opts]
             (cond
               (instance? FluentNone arg) (FluentNone. (str "NUMBER(" (.valueOf arg) ")"))
               (instance? FluentNumber arg) (FluentNumber. (.valueOf arg) opts)
               (instance? FluentDateTime arg) (FluentDateTime. (.valueOf arg) opts)
               :else (throw (js/TypeError. (str "Invalid argument to NUMBER: " arg)))))})

(defn build [locale-str resource]
  (let [locale-str (if (= "la-pig" locale-str) "en" locale-str)
        builder (FluentBundle. (clj->js locale-str)
                               (clj->js {:functions (custom-functions locale-str)}))
        ftl-res (FluentResource. resource)
        errors (.addResource builder ftl-res)]
    (when (seq errors)
      (throw (ex-info "Errors adding resources:" {:resource resource
                                                  :errors errors})))
    builder))

(defn format
  ([bundle entry] (format bundle entry nil))
  ([^FluentBundle bundle entry args]
   (let [entry (clj->js entry)
         message (.getMessage bundle entry)]
     (when-let [v (and message (.-value message))]
       (.formatPattern bundle v (clj->js args))))))

(comment
  (let [input "hello-world = {NUMBER($percent, style: \"percent\")}"
        bundle (build "de" input)]
    (prn :prn (format bundle "hello-world" {:percent 0.89}))
    (println :println (format bundle "hello-world" {:percent 0.89}))))
