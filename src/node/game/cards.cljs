(ns game.cards
  (:require-macros [game.macros :refer [ability]])
  (:require [game.core :refer [pay gain draw] :as core]))

(def cards
  {"Andromeda: Dispossessed Ristie" {:game-init (ability {:effect [(gain :link 1) (draw 4)]})}
   "Anonymous Tip" {:ability (ability {:effect [(draw 3)]})}
   "Beanstalk Royalties" {:ability (ability {:effect [(gain :credit 3)]})}
   "Biotic Labor" {:ability (ability {:effect [(gain :click 2)]})}
   "Chaos Theory: Wünderkind" {:game-init (ability {:effect [(gain :memory 1)]})}
   "Diversified Portfolio" {:ability {:effect #(gain %1 %2 :credit (count (get-in @%1 [:corp :remote-servers])))}}
   "Diesel" {:ability (ability {:effect [(draw 3)]})}
   "Easy Mark" {:ability (ability {:effect [(gain :credit 3)]})}
   "Green Level Clearance" {:ability (ability {:effect [(gain :credit 3) (draw)]})}
   "GRNDL: Power Unleashed" {:game-init (ability {:effect [(gain :credit 5 :bad-publicity 1)]})}
   "Hedge Fund" {:ability (ability {:effect [(gain :credit 9)]})}
   "NBN: The World is Yours*" {:game-init (ability {:effect [(gain :max-hand-size 1)]})}
   "Quality Time" {:ability (ability {:effect [(draw 5)]})}
   "Restructure" {:ability (ability {:effect [(gain :credit 15)]})}
   "Sure Gamble" {:ability (ability {:effect [(gain :credit 9)]})}
   "Sweeps Week"{:ability {:effect #(gain %1 %2 :credit (count (get-in @%1 [:runner :hand])))}}

   ;; partial implementation
   "Exile: Streethawk" {:game-init (ability {:effect [(gain :link 1)]})}
   "Iain Stirling: Retired Spook" {:game-init (ability {:effect [(gain :link 1)]})}
   "Kate \"Mac\" McCaffrey: Digital Tinker" {:game-init (ability {:effect [(gain :link 1)]})}
   "Nasir Meidan: Cyber Explorer" {:game-init (ability {:effect [(gain :link 1)]})}
   "Reina Roja: Freedom Fighter" {:game-init (ability {:effect [(gain :link 1)]})}
   })
