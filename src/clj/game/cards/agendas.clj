(ns game.cards.agendas
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [jinteki.utils :refer [str->int other-side]]
            [jinteki.cards :refer [all-cards]]))

(defn ice-boost-agenda [subtype]
  (letfn [(count-ice [corp]
            (reduce (fn [c server]
                      (+ c (count (filter #(and (has-subtype? % subtype)
                                                (rezzed? %))
                                          (:ices server)))))
                    0 (flatten (seq (:servers corp)))))]
    {:msg (msg "gain " (count-ice corp) " [Credits]")
     :interactive (req true)
     :effect (effect (gain-credits (count-ice corp))
                     (update-all-ice))
     :swapped {:effect (req (update-all-ice state side))}
     :events {:pre-ice-strength {:req (req (has-subtype? target subtype))
                                 :effect (effect (ice-strength-bonus 1 target))}}}))
