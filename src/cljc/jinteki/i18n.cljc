(ns jinteki.i18n
  (:refer-clojure :exclude [format])
  (:require
   [noahtheduke.fluent :as fluent]
   #?@(:clj [[clojure.java.io :as io]
             [clojure.string :as str]])
   #?(:cljs
     [reagent.core :as r])))

(defonce fluent-dictionary
  #?(:clj (atom nil)
     :cljs (r/atom {})))

(defn insert-lang! [lang content]
  (swap! fluent-dictionary assoc lang {:content content
                                       :ftl (let [lang (if (= "la-pig" lang) "en" lang)]
                                              (fluent/build lang content))}))

#?(:clj
   (defn load-dictionary!
     [dir]
     ;; List of supported language files (based on resources/public/i18n directory)
     (let [languages ["en" "fr" "ja" "ko" "la-pig" "pl" "pt" "ru" "zh-simp"]
           ;; Try loading each language directly as a resource (works in both jar and filesystem)
           langs (->> languages
                      (keep (fn [lang]
                              (let [res-path (str dir "/" lang ".ftl")
                                    ;; Try to load from classpath (including jar)
                                    res (io/resource res-path)]
                                (when res
                                  [lang (slurp res)])))))
           errors (volatile! [])]
       (doseq [[lang content] langs]
         (try (insert-lang! lang content)
              (catch Throwable t
                (println "Error inserting i18n data for" lang)
                (println (ex-message t))
                (vswap! errors conj lang))))
       @errors)))

#?(:clj
   (comment
     (load-dictionary! "resources/public/i18n")))

(defn get-content
  [lang]
  (get-in @fluent-dictionary [lang :content]))

(defn get-bundle
  [lang]
  (get-in @fluent-dictionary [lang :ftl]))

(defn get-translation
  [bundle id params]
  (when bundle
    (when-let [ret (fluent/format bundle id params)]
      (when-not (identical? "undefined" ret)
        ret))))

(defn format
  ([lang-cursor resource] (format lang-cursor resource nil))
  ([lang-cursor resource params]
   (let [lang (or @lang-cursor "en")
         resource (if (vector? resource) resource [resource])
         [raw-id fallback] resource
         id (name raw-id)
         bundle (get-bundle lang)]
     (or (get-translation bundle id params)
         fallback
         (get-translation (get-bundle "en") id params)))))
