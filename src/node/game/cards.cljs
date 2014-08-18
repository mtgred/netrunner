(ns game.cards
  (:require-macros [game.macros :refer [do!]])
  (:require [game.core :refer [game-states pay gain draw] :as core]))

(def cards
  {"Andromeda: Dispossessed Ristie"
   {:game-init (do! {:effect [(gain :link 1) (draw 4)]})}

   "Chaos Theory: Wünderkind"
   {:game-init (do! {:effect [(gain :memory 1)]})}

   "GRNDL: Power Unleashed"
   {:game-init (do! {:effect [(gain :credit 5 :bad-publicity 1)]})}

   "NBN: The World is Yours*"
   {:game-init (do! {:effect [(gain :max-hand-size 1)]})}

   ;; partial implementation
   "Exile: Streethawk"
   {:game-init (do! {:effect [(gain :link 1)]})}

   "Iain Stirling: Retired Spook"
   {:game-init (do! {:effect [(gain :link 1)]})}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:game-init (do! {:effect [(gain :link 1)]})}

   "Nasir Meidan: Cyber Explorer"
   {:game-init (do! {:effect [(gain :link 1)]})}

   "Reina Roja: Freedom Fighter"
   {:game-init (do! {:effect [(gain :link 1)]})}

   })
