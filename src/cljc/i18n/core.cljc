(ns i18n.core
  (:require
   [clojure.string :as str]
   [i18n.fluent :as i18n]))

(def fluent-dictionary
  (atom nil))

(defn insert-lang! [lang content]
  (swap! fluent-dictionary assoc (keyword lang) {:content content
                                                 :ftl (i18n/build lang content)}))

(defn get-content
  [lang]
  (get-in @fluent-dictionary [(keyword lang) :content]))

(defn get-bundle
  [lang]
  (get-in @fluent-dictionary [(keyword lang) :ftl]))

(defn tr-impl [app-state resource & params]
  (let [lang (get-in @app-state [:options :language] "en")
        [id fallback] resource
        id (-> id
               (symbol)
               (str)
               (str/replace "." "_"))
        bundle (get-bundle lang)]
    (or (when bundle
          (i18n/format bundle id))
        fallback)))
