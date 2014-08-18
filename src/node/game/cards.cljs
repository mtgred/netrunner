(ns game.cards
  (:require-macros [game.macros :refer [do!]])
  (:require [game.core :refer [game-states pay gain draw] :as core]))

(def cards
  {"Andromeda: Dispossessed Ristie"
   {:game-init (do! {:effect [(gain :link 1) (draw 4)]})}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:game-init (do! {:effect [(gain :link 1)]})}

   "GRNDL: Power Unleashed"
   {:game-init (do! {:effect [(gain :credit 5 :bad-publicity 1)]})}})
