(ns game.cards
  (:require-macros [game.macros :refer [ability]])
  (:require [game.core :refer [pay gain draw] :as core]))

(def cards
  {"Andromeda: Dispossessed Ristie"
   {:game-init (ability {:effect [(gain :link 1) (draw 4)]})}

   "Chaos Theory: Wünderkind"
   {:game-init (ability {:effect [(gain :memory 1)]})}

   "GRNDL: Power Unleashed"
   {:game-init (ability {:effect [(gain :credit 5 :bad-publicity 1)]})}

   "Hedge Fund"
   {:ability (ability {:cost [:credit 5] :effect [(gain :credit 9)]})}

   "NBN: The World is Yours*"
   {:game-init (ability {:effect [(gain :max-hand-size 1)]})}

   "Sure Gamble"
   {:ability (ability {:cost [:credit 5] :effect [(gain :credit 9)]})}


   ;; partial implementation
   "Exile: Streethawk"
   {:game-init (ability {:effect [(gain :link 1)]})}

   "Iain Stirling: Retired Spook"
   {:game-init (ability {:effect [(gain :link 1)]})}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:game-init (ability {:effect [(gain :link 1)]})}

   "Nasir Meidan: Cyber Explorer"
   {:game-init (ability {:effect [(gain :link 1)]})}

   "Reina Roja: Freedom Fighter"
   {:game-init (ability {:effect [(gain :link 1)]})}

   })
