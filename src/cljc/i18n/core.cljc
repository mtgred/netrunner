(ns i18n.core
  (:refer-clojure :exclude [format])
  (:require
   [i18n.fluent :as i18n]
   #?(:cljs
     [reagent.core :as r])))

(defonce fluent-dictionary
  #?(:clj (atom nil)
     :cljs (r/atom {})))

(def allow-fallback? true)

(defn insert-lang! [lang content]
  (swap! fluent-dictionary assoc (keyword lang) {:content content
                                                 :ftl (i18n/build lang content)}))

(defn get-content
  [lang]
  (get-in @fluent-dictionary [(keyword lang) :content]))

(defn get-bundle
  [lang]
  (get-in @fluent-dictionary [(keyword lang) :ftl]))

(defn format
  ([app-state resource] (format app-state resource nil))
  ([app-state resource params]
   (let [lang (get-in @app-state [:options :language] "en")
         resource (if (vector? resource) resource [resource])
         [raw-id fallback] resource
         id (name raw-id)
         bundle (get-bundle lang)]
     (or (when bundle
           (i18n/format bundle id params))
         (when allow-fallback?
           fallback)))))
