(ns i18n.core
  (:require
   [i18n.defs]
   [i18n.en]
   [i18n.fr]
   [i18n.ja]
   [i18n.ko]
   [i18n.la-pig]
   [i18n.pl]
   [i18n.pt]
   [i18n.ru]
   [i18n.zh-simp]
   [taoensso.tempura :as tempura]))

(defn translation-dictionary []
  {:en i18n.en/translations
   :fr i18n.fr/translations
   :ja i18n.ja/translations
   :ko i18n.ko/translations
   :la-pig i18n.la-pig/translations
   :pl i18n.pl/translations
   :pt i18n.pt/translations
   :ru i18n.ru/translations
   :zh-simp i18n.zh-simp/translations})

(def opts {:dict (translation-dictionary)})

(defn tr-impl [app-state resource & params]
  (let [lang (keyword (get-in @app-state [:options :language] :en))]
    (tempura/tr opts [lang :en] resource (vec params))))
