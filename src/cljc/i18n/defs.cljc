(ns i18n.defs)

(defmulti render-map (fn [lang input] lang) :default "en")

