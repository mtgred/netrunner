(ns styles.fonts
  (:require [garden.def :refer [defstyles]]))

(defn at-font-face
  "Create a CSS @font-face rule.  Garden library not functional for this"
  [& properties]
  [(keyword "@font-face") properties])

(defn titilium-web
  [type weight]
  {:font-family "'Titillium Web'"
   :font-style  :normal
   :font-weight weight
   :src        [(str "url(\".. / fonts/TitilliumWeb-" type  ".woff\") format('woff')")
                (str "url(\".. / fonts/TitilliumWeb-" type ".ttf\") format('truetype')")]})

(def anr-icon
  {:font-family "'anr-icon'"
   :font-style  :normal
   :font-weight :normal
   :src [(str "url(\"../fonts/netrunner.eot\") format('eot'))")
         (str "url(\"../fonts/netrunner.eot?#iefix\") format('embedded-opentype')")
         (str "url(\"../fonts/netrunner.woff\") format('woff')")
         (str "url(\"../fonts/netrunner.ttf\") format('truetype')")
         (str "url(\"../fonts/netrunner.svg\") format('svg') active;")]})

(def fontello
  {:font-family "'fontello'"
   :font-style  :normal
   :font-weight :normal
   :src [(str "url(\"../fonts/fontello.eot\")")
         (str "url(\"../fonts/fontello.eot#iefix\") format('embedded-opentype')")
         (str "url(\"../fonts/fontello.woff\") format('woff')")
         (str "url(\"../fonts/fontello.ttf\") format('truetype')")
         (str "url(\"../fonts/netrunner.svg\") format('svg') active;")]})

(defstyles fonts
           (at-font-face (titilium-web "ExtraLight" 200))
           (at-font-face (titilium-web "Light" 300))
           (at-font-face (titilium-web "Regular" 400))
           (at-font-face (titilium-web "Bold" 700))
           (at-font-face anr-icon)
           (at-font-face fontello))
