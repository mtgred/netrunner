(ns game.cards
  (:require-macros [game.macros :refer [effect req]])
  (:require [game.core :refer [pay gain lose draw move damage shuffle-into-deck trash] :as core]
            [game.utils :refer [has?]]))

(def cards
  {"Access to Globalsec"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}

   "Akamatsu Mem Chip"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Armitage Codebusting"
   {:data {:counter 12}
    :abilities [{:cost [:click 1] :counter-cost 2 :msg "gain 2 [Credits]"
                 :effect #(do (gain %1 :runner :credit 2)
                              (when (= (:counter %3) 0)
                                (trash %1 :runner %3)))}]}

   "Andromeda: Dispossessed Ristie"
   {:effect (effect (gain :link 1) (draw 4))}

   "Anonymous Tip"
   {:effect (effect (draw 3))}

   "Beanstalk Royalties"
   {:effect (effect (gain :credit 3))}

   "Blue Level Clearance"
   {:effect (effect (gain :credit 5) (draw 2))}

   "Big Brother"
   {:req (req tagged) :effect (effect (gain :runner :tag 2))}

   "Biotic Labor"
   {:effect (effect (gain :click 2))}

   "Borrowed Satellite"
   {:effect (effect (gain :link 1 :max-hand-size 1))
    :leave-play (effect (lose :link 1 :max-hand-size 1))}

   "BOX-E"
   {:effect (effect (gain :memory 2 :max-hand-size 2))
    :leave-play (effect (lose :memory 2 :max-hand-size 2))}

   "Cache"
   {:abilities [{:counter-cost 1 :effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}]
    :data {:counter 3}}

   "Calling in Favors"
   {:effect (effect (gain :credit (count (filter (fn [c] (has? c :subtype "Connection"))
                                                 (get-in runner [:rig :resource])))))}

   "Cerebral Imaging: Infinite Frontiers"
   {:effect #(add-watch % :cerebral-imaging
                        (fn [k ref old new]
                          (let [credit (get-in new [:corp :credit])]
                            (when (not= (get-in old [:corp :credit]) credit)
                              (swap! ref assoc-in [:corp :max-hand-size] credit)))))}

   "Chaos Theory: Wünderkind"
   {:effect (effect (gain :memory 1))}

   "Closed Accounts"
   {:req (req tagged) :effect (effect (lose :runner :credit :all))}

   "Corporate Shuffle"
   {:effect (effect (shuffle-into-deck :hand) (draw 5))}

   "Corporate War"
   {:effect #(if (> (get-in % [:corp :credit]) 6)
               (gain % :corp :credit 7)
               (lose % :corp :credit :all))}

   "CyberSolutions Mem Chip"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))}

   "Diversified Portfolio"
   {:effect (effect (gain :credit (count (get-in corp [:servers :remote]))))}

   "Diesel"
   {:effect (effect (draw 3))}

   "Duggar’s"
   {:abilities [{:cost [:click 4] :effect (effect (draw 10)) :msg "draw 10 cards"}]}

   "Dyson Mem Chip"
   {:effect (effect (gain :link 1 :memory 1)) :leave-play (effect (lose :link 1 :memory 1))}

   "Easy Mark"
   {:effect (effect (gain :credit 3))}

   "Gila Hands Arcology"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 3)) :msg "gain 3 [Credits]"}]}

   "Government Contracts"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 4)) :msg "gain 4 [Credits]"}]}

   "Green Level Clearance"
   {:effect (effect (gain :credit 3) (draw))}

   "GRNDL: Power Unleashed"
   {:effect (effect (gain :credit 5 :bad-publicity 1))}

   "Hedge Fund"
   {:effect (effect (gain :credit 9))}

   "Hostile Takeover"
   {:effect (effect (gain :credit 7 :bad-publicity 1))}

   "Lawyer Up"
   {:effect (effect (draw 3) (lose :tag 2))}

   "Levy AR Lab Access"
   {:effect (effect (shuffle-into-deck :hand :discard) (draw 5)
                    (move (first (:play-area runner)) :rfg))}

   "Liberated Account"
   {:data {:counter 16}
    :abilities [{:cost [:click 1] :counter-cost 4 :msg "gain 4 [Credits]"
                 :effect #(do (gain %1 :runner :credit 4)
                              (when (= (:counter %3) 0)
                                (trash %1 :runner %3)))}]}

   "Lucky Find"
   {:effect (effect (gain :credit 9))}

   "Magnum Opus"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 2)) :msg "gain 2 [Credits]"}]}

   "Medical Research Fundraiser"
   {:effect (effect (gain :credit 8) (gain :runner :credit 3))}

   "Melange Mining Corp."
   {:abilities [{:cost [:click 3] :effect (effect (gain :credit 7)) :msg "gain 7 [Credits]"}]}

   "NBN: The World is Yours*"
   {:effect (effect (gain :max-hand-size 1))}

   "Neural EMP"
   {:req (req (:made-run register)) :effect (effect (damage :net 1))}

   "Paper Tripping"
   {:req (req (not (:spent-click register))) :effect (effect (lose :tag :all))}

   "Philotic Entanglement"
   {:effect (effect (damage :net (count (:scored runner))))}

   "Private Contracts"
   {:data {:counter 14}
    :abilities [{:cost [:click 1] :counter-cost 2 :msg "gain 2 [Credits]"
                 :effect #(do (gain %1 :corp :credit 2)
                              (when (= (:counter %3) 0)
                                (trash %1 :corp %3)))}]}

   "Private Security Force"
   {:abilities [{:req (req tagged) :cost [:click 1] :effect (effect (damage :meat 1))
                 :msg "do 1 meat damage"}]}

   "Public Sympathy"
   {:effect (effect (gain :max-hand-size 2)) :leave-play (effect (lose :max-hand-size 2))}

   "Power Nap"
   {:effect (effect (gain :credit (+ 2 (count (filter (fn [c] (has? c :subtype "Double"))
                                                      (:discard runner))))))}

   "Professional Contacts"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 1) (draw))
                 :msg "gain 1 [Credits] and draw 1 card"}]}

   "Quality Time"
   {:effect (effect (draw 5))}

   "Restructure"
   {:effect (effect (gain :credit 15))}

   "Scorched Earth"
   {:req (req tagged) :effect (effect (damage :meat 4))}

   "Subliminal Messaging"
   {:effect #(do (gain % :corp :credit 1)
                 (when-not (get-in % [:register :subliminal-messaging])
                   (gain % :corp :click 1))
                 (swap! % assoc-in [:register :subliminal-messaging] true))}

   "Successful Demonstration"
   {:req (req (:unsucessful-run register)) :effect (gain :credit 7)}

   "Sure Gamble"
   {:effect (effect (gain :credit 9))}

   "Sweeps Week"
   {:effect (effect (gain :credit (count (:hand runner))))}

   "Theophilius Bagbiter"
   {:effect #(do (lose % :runner :credit :all)
                 (add-watch % :theophilius-bagbiter
                            (fn [k ref old new]
                              (let [credit (get-in new [:runner :credit])]
                                (when (not= (get-in old [:runner :credit]) credit)
                                  (swap! ref assoc-in [:runner :max-hand-size] credit))))))
    :leave-play #(remove-watch % :theophilius-bagbiter)}

   "Veterans Program"
   {:effect (effect (lose :bad-publicity 2))}

   "Witness Tampering"
   {:effect (effect (lose :bad-publicity 2))}

   ;; partial implementation
   "Astrolabe"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Bad Times"
   {:req (req tagged)}

   "Exile: Streethawk"
   {:effect (effect (gain :link 1))}

   "Grimoire"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))}

   "Desperado"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Doppelgänger"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Early Bird"
   {:req (req (not (:spent-click register)))}

   "Emergency Shutdown"
   {:req (req (some :hq (:successful-run register)))}

   "Iain Stirling: Retired Spook"
   {:effect (effect (gain :link 1))}

   "Freelancer"
   {:req (req tagged)}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:effect (effect (gain :link 1))}

   "Kraken"
   {:req (req (:stole-agenda register))}

   "Logos"
   {:effect (effect (gain :memory 1 :max-hand-size 1))
    :leave-play (effect (lose :memory 1 :max-hand-size 1))}

   "Midseason Replacements"
   {:req (req (:stole-agenda register))}

   "Monolith"
   {:effect (effect (gain :memory 3)) :leave-play (effect (lose :memory 3))}

   "Nasir Meidan: Cyber Explorer"
   {:effect (effect (gain :link 1))}

   "Power Shutdown"
   {:req (req (:made-run register))}

   "Psychographics"
   {:req (req tagged)}

   "Rabbit Hole"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}

   "Reina Roja: Freedom Fighter"
   {:effect (effect (gain :link 1))}

   "SEA Source"
   {:req (req (:sucessful-run register))}

   "Spinal Modem"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "The Helpful AI"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}

   "The Toolbox"
   {:effect (effect (gain :link 2 :memory 2)) :leave-play (effect (lose :link 2 :memory 2))}

   "Three Steps Ahead"
   {:req (req (not (:spent-click register)))}})
