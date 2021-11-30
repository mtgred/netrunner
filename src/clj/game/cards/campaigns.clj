(ns game.cards.campaigns
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.string :as string]
            [clojure.set :as clj-set]))

; Campaign: A Breeze in the Ddark
(defcard "A Breeze in the Dark"
  {:events [{:event :runner-turn-begins
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

