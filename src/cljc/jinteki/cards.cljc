(ns jinteki.cards)

(defonce all-cards #?(:clj (atom {})
                      :cljs (reagent.core/atom {})))

(defonce mwl (atom []))

(defonce sets (atom []))

(defonce cycles (atom []))