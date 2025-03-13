(ns i18n.fluent
  (:require
   ["@fluent/bundle" :refer [FluentBundle FluentResource]]))

(defn build [locale-str resource & resources]
  (let [builder (FluentBundle. (clj->js locale-str))]
    (doseq [r (cons resource resources)
            :let [ftl-res (FluentResource. resource)
                  errors (.addResource builder ftl-res)]]
      (when (seq errors)
        (throw (ex-info "Errors adding resources:" {:resource r
                                                    :errors errors}))))
    builder))

(defn format
  ([bundle entry] (format bundle entry nil))
  ([^FluentBundle bundle entry args]
   (let [entry (clj->js entry)
         message (.getMessage bundle entry)]
     (when-let [v (and message (.-value message))]
       (try (.formatPattern bundle v (clj->js args))
            (catch :default _ nil))))))

(comment
  (let [input "# Simple things are simple.
hello-world = Hello, world!
hello-user = Hello, {$user-name}!

# Complex things are possible.
shared-photos =
    {$user-name} {$photo-count ->
        [one] added a new photo
       *[other] added {$photo-count} new photos
    } to {$user-gender ->
        [male] his stream
        [female] her stram
       *[other] their stream
    }."
        bundle (build "en-US" input)]
    (println (format bundle "hello-world"))
    (println (format bundle :hello-user {:user-name "Noah"}))))
