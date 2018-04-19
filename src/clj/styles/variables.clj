(ns styles.variables
  (:require [garden.color :refer [rgba]]))

(def light-grey "#ddd")
(def grey "#999")
(def dark-grey "#383838")
(def obsidian "#282828") ; Very very dark grey
(def blue "#00AEFF")
(def text-color "#333")
(def orange "#FFA500")

;; Darker colours in the spirit of each faction
(def anarch-orange "#cc5200")
(def shaper-green "#29a329")
(def criminal-blue "#007acc")

(def transparent-orange (rgba 255 165 0 0.35))
(def transparent-blue (rgba 39 56 76 0.82))
(def transparent-lightblue (rgba 0 174 255 0.85))
(def transparent-green (rgba 50 205 50 0.85))
(def transparent-red (rgba 255 0 0 0.85))
(def transparent-orange-bright (rgba 255 165 0 0.90))
(def transparent-gold (rgba 200 130 0 0.90))
(def transparent-darkblue (rgba 0 0 102 0.85))
(def transparent-purple (rgba 67 0 102 0.85))
(def transparent-black (rgba 0 0 0 0.6))
