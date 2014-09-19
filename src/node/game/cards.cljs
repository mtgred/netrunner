(ns game.cards
  (:require-macros [game.macros :refer [effect req msg]])
  (:require [game.core :refer [pay gain lose draw move damage shuffle-into-deck trash purge
                               add-prop set-prop once-per-turn system-msg end-run] :as core]
            [game.utils :refer [has?]]))

(def cards
  {"Access to Globalsec"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}

   "Adonis Campaign"
   {:data {:counter 12}
    :events {:turn-begins {:msg "gain 3 [Credits]" :counter-cost 3
                           :effect #(do (gain %1 :corp :credit 3)
                                        (when (zero? (:counter %3)) (trash %1 :corp %3)))}}}

   "Akamatsu Mem Chip"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "All-nighter"
   {:abilities [{:cost [:click 1] :effect (effect (trash card) (gain :click 2))
                 :msg "gain [Click][Click]"}]}

   "Armitage Codebusting"
   {:data {:counter 12}
    :abilities [{:cost [:click 1] :counter-cost 2 :msg "gain 2 [Credits]"
                 :effect #(do (gain %1 :runner :credit 2)
                              (when (zero? (:counter %3)) (trash %1 :runner %3)))}]}

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

   "Cyberdex Trial"
   {:effect (effect (purge))}

   "CyberSolutions Mem Chip"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))}

   "Daily Casts"
   {:data {:counter 8}
    :events {:turn-begins {:msg "gain 2 [Credits]" :counter-cost 2
                           :effect #(do (gain %1 :runner :credit 2)
                                        (when (zero? (:counter %3)) (trash %1 :runner %3)))}}}

   "Diversified Portfolio"
   {:effect (effect (gain :credit (count (get-in corp [:servers :remote]))))}

   "Diesel"
   {:effect (effect (draw 3))}

   "Duggars"
   {:abilities [{:cost [:click 4] :effect (effect (draw 10)) :msg "draw 10 card"}]}

   "Dyson Mem Chip"
   {:effect (effect (gain :link 1 :memory 1)) :leave-play (effect (lose :link 1 :memory 1))}

   "Easy Mark"
   {:effect (effect (gain :credit 3))}

   "Eve Campaign"
   {:data {:counter 16}
    :events {:turn-begins {:msg "gain 3 [Credits]" :counter-cost 2
                           :effect #(do (gain %1 :corp :credit 2)
                                        (when (zero? (:counter %3)) (trash %1 :corp %3)))}}}

   "Executive Retreat"
   {:data {:counter 1}
    :abilities [{:cost [:click 1] :counter-cost 1
                 :effect (effect (shuffle-into-deck :hand) (draw 5))}]}

   "Geothermal Fracking"
   {:data {:counter 2}
    :abilities [{:cost [:click 1] :counter-cost 1
                 :effect (effect (gain :credit 7 :bad-publicity 1))}]}

   "Gila Hands Arcology"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 3)) :msg "gain 3 [Credits]"}]}

   "Government Contracts"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 4)) :msg "gain 4 [Credits]"}]}

   "Green Level Clearance"
   {:effect (effect (gain :credit 3) (draw))}

   "GRNDL: Power Unleashed"
   {:effect (effect (gain :credit 5 :bad-publicity 1))}

   "Hard at Work"
   {:events {:turn-begins {:msg "gain 2 [Credits] and lose [Click]"
                           :effect (effect (lose :click 1) (gain :credit 2))}}}

   "Hedge Fund"
   {:effect (effect (gain :credit 9))}

   "Hostile Takeover"
   {:effect (effect (gain :credit 7 :bad-publicity 1))}

   "Kati Jones"
   {:abilities
    [{:cost [:click 1] :msg "store 3 [Credits]"
      :effect #(once-per-turn %1 %2 %3 (effect (add-prop card :counter 3)))}
     {:cost [:click 1] :msg (msg "gain " (:counter card) " [Credits]")
      :effect #(once-per-turn %1 %2 %3 (effect (gain :credit (:counter card))
                                               (set-prop card :counter 0)))}]}

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

   "Mandatory Upgrades"
   {:effect (effect (gain :click 1 :click-per-turn 1))}

   "Marked Accounts"
   {:abilities [{:cost [:click 1] :message "store 3 [Credits]"
                 :effect (effect (add-prop card :counter 3))}]
    :events {:turn-begins {:msg "gain 1 [Credits]" :counter-cost 1
                           :effect (effect (gain :credit 1))}}}

   "Medical Research Fundraiser"
   {:effect (effect (gain :credit 8) (gain :runner :credit 3))}

   "Melange Mining Corp."
   {:abilities [{:cost [:click 3] :effect (effect (gain :credit 7)) :msg "gain 7 [Credits]"}]}

   "Mental Health Clinic"
   {:effect (effect (gain :runner :max-hand-size 1))
    :leave-play (effect (lose :runner :max-hand-size 1))
    :events {:turn-begins {:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "NBN: The World is Yours*"
   {:effect (effect (gain :max-hand-size 1))}

   "Neural EMP"
   {:req (req (:made-run runner-reg)) :effect (effect (damage :net 1))}

   "PAD Campaign"
   {:events {:turn-begins {:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Order of Sol"
   {:effect #(add-watch % :order-of-sol
                        (fn [k ref old new]
                          (when (zero? (get-in new [:runner :credit]))
                            (once-per-turn % :runner %3
                                           (effect (system-msg "gain 1 credit with Order of Sol")
                                                   (gain :credit 1))
                                           :order-of-sol))))
    :leave-play #(remove-watch % :order-of-sol)}

   "Paper Tripping"
   {:req (req (not (:spent-click runner-reg))) :effect (effect (lose :tag :all))}

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

   "Rachel Beckman"
   {:effect #(do (gain % :runner :click 1 :click-per-turn 1)
                 (add-watch % :rachel-beckman
                            (fn [k ref old new]
                              (when (> (get-in new [:runner :tag]) 0)
                                (trash ref :runner %3)
                                (system-msg ref %2 "trash Rachel Beckman for being tagged")))))
    :leave-play #(do (remove-watch % :rachel-beckman)
                     (lose %1 %2 :click 1 :click-per-turn 1))}

   "Restructure"
   {:effect (effect (gain :credit 15))}

   "Scorched Earth"
   {:req (req tagged) :effect (effect (damage :meat 4))}

   "Subliminal Messaging"
   {:effect #(do (gain % :corp :credit 1)
                 (once-per-turn %1 %2 %3 (effect (gain :click 1)) :subliminal-messaging))}

   "Successful Demonstration"
   {:req (req (:unsucessful-run runner-reg)) :effect (gain :credit 7)}

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

   "Tri-maf Contact"
   {:abilities [{:cost [:click 1] :msg "gain 2 [Credits]"
                 :effect #(once-per-turn %1 %2 %3 (effect (gain :credit 2)))}]
    :leave-play (effect (damage :meat 3))}

   "Underworld Contact"
   {:events {:turn-begins {:msg "gain 1 [Credits]" :req (req (>= (:link runner) 2))
                           :effect (effect (gain :credit 1))}}}

   "Veterans Program"
   {:effect (effect (lose :bad-publicity 2))}

   "Witness Tampering"
   {:effect (effect (lose :bad-publicity 2))}

   "Wyldside"
   {:events {:turn-begins {:msg "draw 2 cards and lose [Click]"
                           :effect (effect (lose :click 1) (draw 2))}}}

   ;; ICE
   "Bastion"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Chimera"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Curtain Wall"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Data Mine"
   {:abilities [{:msg "do 1 net damage" :effect (effect (trash card) (damage :net 1))}]}

   "Datapike"
   {:abilities [{:label "The runner must pay 2 [Credits] if able"
                 :msg "force the runner to pay 2 [Credits] if able"
                 :effect (effect (lose :runner :credit 2))}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Eli 1.0"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Enigma"
   {:abilities [{:label "The runner lose 1 [Click] if able"
                 :msg "force the runner to lose 1 [Click] if able"
                 :effect (effect (lose :runner :click 1))}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Fenris"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [{:label "Do 1 brain damage" :msg "do 1 brain damage"
                 :effect (effect (damage :brain 1))}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Galahad"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Grim"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))}

   "Guard"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Himitsu-Bako"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Hive"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Hadrians Wall"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Heimdall 1.0"
   {:abilities [{:label "Do 1 brain damage" :msg "do 1 brain damage"
                 :effect (effect (damage :brain 1))}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Heimdall 2.0"
   {:abilities [{:label "Do 1 brain damage" :msg "do 1 brain damage"
                 :effect (effect (damage :brain 1))}
                {:label "Do 1 brain damage and end the run" :msg "do 1 brain damage and end the run"
                 :effect (effect (damage :brain 1))}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Hourglass"
   {:abilities [{:msg "force the runner to lose 1 [Click] if able"
                 :effect (effect (lose :runner :click 1))}]}

   "Ice Wall"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "IQ"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Janus 1.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1))}]}

   "Komainu"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1))}]}

   "Lotus Field"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Mother Goddess"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Neural Katana"
   {:abilities [{:msg "do 3 net damage" :effect (effect (damage :net 3))}]}

   "NEXT Bronze"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "NEXT Silver"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Paper Wall"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Quandary"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Rainbow"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Rototurret"
   {:abilities [{:label "Trash 1 program" :msg "trash 1 program"}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Shinobi"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))}

   "TMI"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Tollbooth"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Viktor 1.0"
   {:abilities [{:label "Do 1 brain damage" :msg "do 1 brain damage"
                 :effect (effect (damage :brain 1))}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Wall of Static"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Wall of Thorns"
   {:abilities [{:label "Do 2 net damage" :msg "do 2 net damage" :effect (effect (damage :net 2))}
                {:label "End the run" :msg "end the run" :effect (effect (end-run))}]}

   "Wraparound"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Wotan"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Yagura"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1))}]}

   ;; partial implementation
   "AstroScript Pilot Program"
   {:data {:counter 1}}

   "Astrolabe"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Bad Times"
   {:req (req tagged)}

   "Crypsis"
   {:abilities [{:cost [:credit 1] :msg "break ICE subroutine"}
                {:cost [:credit 1] :msg "add 1 strength" :effect (effect (add-prop card :strengh 1))}
                {:cost [:click 1] :msg "place 1 virus counter"
                 :effect (effect (add-prop card :counter 1))}]}

   "D4v1d"
   {:data {:counter 3}}

   "Director Haas"
   {:effect (effect (gain :click 1 :click-per-turn 1)) :leave-play (effect (lose :click-per-turn 1))}

   "Exile: Streethawk"
   {:effect (effect (gain :link 1))}

   "Grimoire"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))}

   "Desperado"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Doppelgänger"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Early Bird"
   {:req (req (not (:spent-click runner-reg)))}

   "Eden Shard"
   {:abilities [{:effect (effect (trash card) (draw :corp 2))
                 :msg "force the Corp to draw 2 cards"}]}

   "Efficiency Committee"
   {:data {:counter 3}
    :abilities [{:cost [:click 1] :counter-cost 1 :effect (effect (gain :click 2))
                 :msg "gain [Click][Click]"}]}

   "Emergency Shutdown"
   {:req (req (some :hq (:successful-run runner-reg)))}

   "Fall Guy"
   {:abilities [{:effect (effect (trash card)) :msg "prevent another resource from being trashed"}
                {:effect (effect (trash card) (gain :credit 2)) :msg "gain 2 [Credits]"}]}

   "Freelancer"
   {:req (req tagged)}

   "Gorman Drip v1"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit (:counter card)) (trash card))
                 :msg (msg "gain " (:counter card) " [Credits]")}]}

   "House of Knives"
   {:data {:counter 3}
    :abilities [{:cost [:click 1] :counter-cost 1 :effect (effect (damage :net 1))
                 :msg "do 1 Net damage"}]}

   "Iain Stirling: Retired Spook"
   {:effect (effect (gain :link 1))}

   "Imp"
   {:data {:counter 2}
    :abilities [{:counter-cost 1 :msg "trash at no cost"
                 :effect #(once-per-turn %1 %2 %3 (fn []))}]}

   "Jackson Howard"
   {:abilities [{:cost [:click 1] :effect (effect (draw 2)) :msg "draw 2 cards"}]}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:effect (effect (gain :link 1))}

   "Kraken"
   {:req (req (:stole-agenda runner-reg))}

   "Logos"
   {:effect (effect (gain :memory 1 :max-hand-size 1))
    :leave-play (effect (lose :memory 1 :max-hand-size 1))}

   "Midseason Replacements"
   {:req (req (:stole-agenda runner-reg))}

   "Monolith"
   {:effect (effect (gain :memory 3)) :leave-play (effect (lose :memory 3))}

   "Nasir Meidan: Cyber Explorer"
   {:effect (effect (gain :link 1))}

   "Networking"
   {:effect (effect (lose :tag 1))}

   "Power Shutdown"
   {:req (req (:made-run runner-reg))}

   "Psychographics"
   {:req (req tagged)}

   "Rabbit Hole"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}

   "Reina Roja: Freedom Fighter"
   {:effect (effect (gain :link 1))}

   "Research Station"
   {:effect (effect (gain :max-hand-size 2))
    :leave-play (effect (lose :max-hand-size 2))}

   "SEA Source"
   {:req (req (:sucessful-run runner-reg))}

   "Spinal Modem"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "The Helpful AI"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}

   "The Toolbox"
   {:effect (effect (gain :link 2 :memory 2)) :leave-play (effect (lose :link 2 :memory 2))}

   "Three Steps Ahead"
   {:req (req (not (:spent-click runner-reg)))}})
