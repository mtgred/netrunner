(ns game.cards
  (:require [game.core :refer [game-states pay gain draw] :as core]))

(def cards
  {"Andromeda: Dispossessed Ristie"
   {:game-init (fn [state side args]
                 (gain state side :link 1)
                 (draw state side 4))}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:game-init (fn [state side args]
                 (gain state side :link 1))}

   "GRNDL: Power Unleashed"
   {:game-init (fn [state side args]
                 (gain state side :credit 5 :bad-publicity 1))}})

