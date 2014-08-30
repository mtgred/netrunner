(ns game.cards
  (:require-macros [game.macros :refer [effect req]])
  (:require [game.core :refer [pay gain lose draw move damage shuffle-into-deck] :as core]
            [game.utils :refer [has?]]))

(def cards
  {"Access to Globalsec" {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}
   "Akamatsu Mem Chip" {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}
   "Andromeda: Dispossessed Ristie" {:effect (effect (gain :link 1) (draw 4))}
   "Anonymous Tip" {:effect (effect (draw 3))}
   "Beanstalk Royalties" {:effect (effect (gain :credit 3))}
   "Blue Level Clearance" {:effect (effect (gain :credit 5) (draw 2))}
   "Big Brother" {:req (req tagged) :effect (effect (gain :runner :tag 2))}
   "Biotic Labor" {:effect (effect (gain :click 2))}
   "Borrowed Satellite" {:effect (effect (gain :link 1 :max-hand-size 1))
                         :leave-play (effect (lose :link 1 :max-hand-size 1))}
   "Calling in Favors"
   {:effect (effect (gain :credit (count (filter (fn [c] (has? c :subtype "Connection"))
                                                 (get-in runner [:rig :resource])))))}
   "Chaos Theory: Wünderkind" {:effect (effect (gain :memory 1))}
   "Closed Accounts" {:req (req tagged) :effect (effect (lose :runner :credit :all))}
   "Corporate Shuffle" {:effect (effect (shuffle-into-deck :hand) (draw 5))}
   "CyberSolutions Mem Chip" {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))}
   "Diversified Portfolio" {:effect (effect (gain :credit (count (get-in corp [:servers :remote]))))}
   "Diesel" {:effect (effect (draw 3))}
   "Dyson Mem Chip" {:effect (effect (gain :link 1 :memory 1))
                     :leave-play (effect (lose :link 1 :memory 1))}
   "Easy Mark" {:effect (effect (gain :credit 3))}
   "Green Level Clearance" {:effect (effect (gain :credit 3) (draw))}
   "GRNDL: Power Unleashed" {:effect (effect (gain :credit 5 :bad-publicity 1))}
   "Hedge Fund" {:effect (effect (gain :credit 9))}
   "Lawyer Up" {:effect (effect (draw 3) (lose :tag 2))}
   "Levy AR Lab Access"
   {:effect (effect (shuffle-into-deck :hand :discard) (draw 5) (move (first (:play-area runner)) :rfg))}
   "Lucky Find" {:effect (effect (gain :credit 9))}
   "Magnum Opus" {:abilities [{:cost [:click 1] :effect (effect (gain :credit 2)) :msg "gain 2 [Credits]"}]}
   "Medical Research Fundraiser" {:effect (effect (gain :credit 8) (gain :runner :credit 3))}
   "NBN: The World is Yours*" {:effect (effect (gain :max-hand-size 1))}
   "Neural EMP" {:req (req (:made-run register)) :effect (effect (damage :net 1))}
   "Paper Tripping" {:req (req (not (:spent-click register))) :effect (effect (lose :tag :all))}
   "Power Nap"
   {:effect (effect (gain :credit (+ 2 (count (filter (fn [c] (has? c :subtype "Double")) (:discard runner))))))}
   "Professional Contacts" {:abilities [{:cost [:click 1] :effect (effect (gain :credit 1) (draw))
                                         :msg "gain 1 [Credits] and draw 1 card"}]}
   "Quality Time" {:effect (effect (draw 5))}
   "Restructure" {:effect (effect (gain :credit 15))}
   "Sure Gamble" {:effect (effect (gain :credit 9))}
   "Scorched Earth" {:req (req tagged) :effect (effect (damage :meat 4))}
   "Successful Demonstration" {:prereq [()]}
   "Sweeps Week" {:effect (effect (gain :credit (count (:hand runner))))}
   "Witness Tampering" {:effect (effect (lose :bad-publicity 2))}

   ;; partial implementation
   "Astrolabe" {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}
   "Exile: Streethawk" {:effect (effect (gain :link 1))}
   "Grimoire" {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))}
   "Desperado" {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}
   "Doppelgänger" {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}
   "Iain Stirling: Retired Spook" {:effect (effect (gain :link 1))}
   "Kate \"Mac\" McCaffrey: Digital Tinker" {:effect (effect (gain :link 1))}
   "Logos" {:effect (effect (gain :memory 1 :max-hand-size 1))
            :leave-play (effect (lose :memory 1 :max-hand-size 1))}
   "Monolith" {:effect (effect (gain :memory 3)) :leave-play (effect (lose :memory 3))}
   "Nasir Meidan: Cyber Explorer" {:effect (effect (gain :link 1))}
   "Rabbit Hole" {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}
   "Reina Roja: Freedom Fighter" {:effect (effect (gain :link 1))}
   "Spinal Modem" {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}
   "The Helpful AI" {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}
   "The Toolbox" {:effect (effect (gain :link 2 :memory 2))
                  :leave-play (effect (lose :link 2 :memory 2))}
   })
