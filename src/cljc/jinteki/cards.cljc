(ns jinteki.cards
  #?(:cljs
     (:require [reagent.core :as r])))


(defonce all-cards #?(:clj (atom {})
                      :cljs (r/atom {})))

(defonce mwl (atom []))

(defonce sets (atom []))

(defonce cycles (atom []))
