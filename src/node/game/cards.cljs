(ns game.cards
  (:require-macros [game.macros :refer [effect]])
  (:require [game.core :refer [pay gain lose draw shuffle-into-deck] :as core]
            [game.utils :refer [has?]]))

(def cards
  {"Andromeda: Dispossessed Ristie" {:effect (effect (gain :link 1) (draw 4))}
   "Anonymous Tip" {:effect (effect (draw 3))}
   "Beanstalk Royalties" {:effect (effect (gain :credit 3))}
   "Blue Level Clearance" {:effect (effect (gain :credit 5) (draw 2))}
   "Biotic Labor" {:effect (effect (gain :click 2))}
   "Calling in Favors"
   {:effect (effect (gain :credit (count (filter (fn [c] (has? c :subtype "Connection")) (:resources rig)))))}
   "Chaos Theory: Wünderkind" {:effect (effect (gain :memory 1))}
   "Corporate Shuffle" {:effect (effect (shuffle-into-deck :hand) (draw 5))}
   "Diversified Portfolio" {:effect (effect (gain :credit (count (:remote servers))))}
   "Diesel" {:effect (effect (draw 3))}
   "Easy Mark" {:effect (effect (gain :credit 3))}
   "Green Level Clearance" {:effect (effect (gain :credit 3) (draw))}
   "GRNDL: Power Unleashed" {:effect (effect (gain :credit 5 :bad-publicity 1))}
   "Hedge Fund" {:effect (effect (gain :credit 9))}
   "Lawyer Up" {:effect (effect (draw 3) (lose :tag 2))}
   "Levy AR Lab Access" {:effect (effect (shuffle-into-deck :hand :discard) (draw 5))}
   "Lucky Find" {:effect (effect (gain :credit 9))}
   "NBN: The World is Yours*" {:effect (effect (gain :max-hand-size 1))}
   "Power Nap" {:effect (effect (gain (+ 2 (count (filter (fn [c] has? c :subtype "Double") heap)))))}
   "Quality Time" {:effect (effect (draw 5))}
   "Restructure" {:effect (effect (gain :credit 15))}
   "Sure Gamble" {:effect (effect (gain :credit 9))}
   "Sweeps Week"{:effect (effect (gain :credit (count grip)))}

   ;; partial implementation
   "Exile: Streethawk" {:effect (effect (gain :link 1))}
   "Iain Stirling: Retired Spook" {:effect (effect (gain :link 1))}
   "Kate \"Mac\" McCaffrey: Digital Tinker" {:effect (effect (gain :link 1))}
   "Nasir Meidan: Cyber Explorer" {:effect (effect (gain :link 1))}
   "Reina Roja: Freedom Fighter" {:effect (effect (gain :link 1))}
   })
