(ns translations.core
  (:require
   [taoensso.tempura :as tempura]
   [translations.en]
   [translations.fr]
   [translations.ja]
   [translations.ko]
   [translations.la-pig]
   [translations.pl]
   [translations.pt]
   [translations.ru]
   [translations.zh-simp]))

(def translation-dictionary
  {:dict
   {:en translations.en/translations
    :fr translations.fr/translations
    :ja translations.ja/translations
    :ko translations.ko/translations
    :la-pig translations.la-pig/translations
    :pl translations.pl/translations
    :pt translations.pt/translations
    :ru translations.ru/translations
    :zh-simp translations.zh-simp/translations}})

(defn opts [] translation-dictionary)

(defn tr-impl [app-state resource & params]
  (let [lang (keyword (get-in @app-state [:options :language] :en))]
    (tempura/tr (opts) [lang :en] resource (vec params))))
