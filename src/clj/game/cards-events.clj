(in-ns 'game.core)

(def cards-events
  {"Account Siphon"
   {:effect (effect (run :hq {:replace-access
                              {:msg (msg "force the Corp to lose " (min 5 (:credit corp))
                                         " [Credits], gain " (* 2 (min 5 (:credit corp)))
                                         " [Credits] and take 2 tags")
                               :effect (effect (gain :tag 2 :credit (* 2 (min 5 (:credit corp))))
                                               (lose :corp :credit (min 5 (:credit corp))))}} card))}

   "Amped Up"
   {:effect (effect (gain :click 3) (damage :brain 1 {:unpreventable true :card card}))}

   "Blackmail"
   {:req (req (> (:bad-publicity corp) 0)) :prompt "Choose a server" :choices (req servers)
    :effect (effect (run target))}

   "Bribery"
   {:prompt "How many [Credits]?" :choices :credit
    :msg (msg "increase the rez cost of the 1st unrezzed ice approached by " target " [Credits]")
    :effect (effect (resolve-ability {:prompt "Choose a server" :choices (req servers)
                                      :effect (effect (run target nil card))} card nil))}


   "Calling in Favors"
   {:effect (effect (gain :credit (count (filter (fn [c] (has? c :subtype "Connection"))
                                                 (all-installed state :runner)))))}

   "Career Fair"
   {:prompt "Choose a Resource to install"
    :choices (req (filter #(#{"Resource"} (:type %)) (:hand runner)))
    :effect  (effect (install-cost-bonus -3) (runner-install target))}

   "Code Siphon"
   {:effect (effect (run :rd
                         {:replace-access
                          {:prompt "Choose a program to install"
                           :msg (msg "install " (:title target) " and take 1 tag")
                           :choices (req (filter #(has? % :type "Program") (:deck runner)))
                           :effect (effect (install-cost-bonus (* -3 (count (get-in corp [:servers :rd :ices]))))
                                           (runner-install target) (gain :tag 1) (shuffle! :deck))}} card))}

   "Day Job"
   {:additional-cost [:click 3] :effect (effect (gain :credit 10))}

   "Déjà Vu"
   {:prompt "Choose a card to add to Grip" :choices (req (:discard runner))
    :msg (msg "add " (:title target) " to his Grip")
    :effect (req (move state side target :hand)
                 (when (has? target :subtype "Virus")
                   (resolve-ability state side
                                    {:prompt "Choose a virus to add to Grip"
                                     :msg (msg "add " (:title target) " to his Grip")
                                     :choices (req (filter #(has? % :subtype "Virus") (:discard runner)))
                                     :effect (effect (move target :hand))} card nil)))}

   "Demolition Run"
   {:prompt "Choose a server" :choices ["HQ" "R&D"] :effect (effect (run target))}

   "Diesel"
   {:effect (effect (draw 3))}

   "Dirty Laundry"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (run target {:end-run {:req (req (:successful run)) :msg " gain 5 [Credits]"
                                           :effect (effect (gain :runner :credit 5))}} card))}

   "Drive By"
   {:choices {:req #(and (= (second (:zone %)) :remote)
                         (= (last (:zone %)) :content)
                         (not (:rezzed %)))}
    :msg (msg "expose " (:title target) (when (#{"Asset" "Upgrade"} (:type target)) " and trash it"))
    :effect (req (expose state side target)
                 (when (#{"Asset" "Upgrade"} (:type target))
                   (trash state side (assoc target :seen true))))}

   "Early Bird"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (gain :click 1) (run target))}

   "Easy Mark"
   {:effect (effect (gain :credit 3))}

   "Emergency Shutdown"
   {:req (req (some #{:hq} (:successful-run runner-reg))) :msg (msg "derez " (:title target))
    :choices {:req #(and (has? % :type "ICE") (:rezzed %))} :effect (effect (derez target))}

   "Escher"
   (let [ice-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :corp (:zone i))))))
         eshelp (fn es [] {:prompt "Select two pieces of ice to swap positions"
                           :choices {:req #(and (= (first (:zone %)) :servers) (= (:type %) "ICE")) :max 2}
                           :effect (req (if (= (count targets) 2)
                                          (let [fndx (ice-index state (first targets))
                                                sndx (ice-index state (second targets))
                                                fnew (assoc (first targets) :zone (:zone (second targets)))
                                                snew (assoc (second targets) :zone (:zone (first targets)))]
                                            (swap! state update-in (cons :corp (:zone (first targets)))
                                                   #(assoc % fndx snew))
                                            (swap! state update-in (cons :corp (:zone (second targets)))
                                                   #(assoc % sndx fnew))
                                            (update-ice-strength state side fnew)
                                            (update-ice-strength state side snew)
                                            (resolve-ability state side (es) card nil))
                                          (system-msg state side "has finished rearranging ice")))})]
     {:effect (effect (run :hq {:replace-access {:msg "rearrange installed ice"
                                                 :effect (effect (resolve-ability (eshelp) card nil))}} card))})

   "Eureka!"
   {:effect
    (req (let [topcard (first (:deck runner))
               caninst (some #(= % (:type topcard)) '("Hardware" "Resource" "Program"))
               cost (min 10 (:cost topcard))]
           (when caninst
             (do (gain state side :credit cost)
                 (runner-install state side topcard)))
           (when (get-card state topcard) ; only true if card was not installed
             (do (system-msg state side (str "reveals and trashes " (:title topcard)))
                 (trash state side topcard)
                 (when caninst (lose state side :credit cost))))))}

   "Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (map :title (:hand corp)))}

   "Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck runner)))
    :effect (effect (move target :hand) (shuffle! :deck))}

   "Feint"
   {:effect (effect (run :hq) (max-access 0))}

   "Fisk Investment Seminar"
   {:effect (effect (draw 3) (draw :corp 3))}

   "Forged Activation Orders"
   {:choices {:req #(and (has? % :type "ICE") (not (:rezzed %)))}
    :effect (req (let [ice target]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Rez " (:title ice) " or trash it?") :choices ["Rez" "Trash"]
                      :effect (effect (resolve-ability
                                        (if (and (= target "Rez") (<= (:cost ice) (:credit corp)))
                                          {:msg (msg "force the rez of " (:title ice))
                                           :effect (effect (rez :corp ice))}
                                          {:msg "trash the ICE" :effect (effect (trash :corp ice))})
                                        card nil))}
                     card nil)))}

   "Forked"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target))}

   "Frame Job"
   {:additional-cost [:forfeit] :effect (effect (gain :corp :bad-publicity 1))}

   "Freelance Coding Contract"
   {:choices {:max 5 :req #(and (has? % :type "Program") (= (:zone %) [:hand]))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain " (* 2 (count targets)) " [Credits]")
    :effect (effect (trash-cards targets) (gain :credit (* 2 (count targets))))}

   "Game Day"
   {:msg (msg "draw " (- (:max-hand-size runner) (count (:hand runner))) " cards")
    :effect (effect (draw (- (:max-hand-size runner) (count (:hand runner)))))}

   "Hacktivist Meeting"
   {:events {:rez {:req (req (not= (:type target) "ICE"))
                   :msg "force the Corp to trash 1 card from HQ at random"
                   :effect (effect (trash (first (shuffle (:hand corp)))))}}}

   "Hostage"
   {:prompt "Choose a Connection to install"
    :choices (req (filter #(and (has? % :subtype "Connection")
                                (<= (:cost %) (:credit runner))) (:deck runner)))
    :effect (effect (runner-install target) (shuffle! :deck))}

   "Ive Had Worse"
   {:effect (effect (draw 3))
    :trash-effect {:req (req (#{:meat :net} target))
                   :effect (effect (draw :runner 3)) :msg "draw 3 cards"}}

   "Indexing"
   {:effect (effect (run :rd {:replace-access
                              {:msg "rearrange the top 5 cards of R&D"
                               :effect (req (doseq [c (take 5 (:deck corp))]
                                              (move state side c :play-area)))}} card))}

   "Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (resolve-ability (if (= target "Expose a card")
                                       {:choices {:req #(= (first (:zone %)) :servers)}
                                        :effect (effect (expose target))
                                        :msg (msg "expose " (:title target))}
                                       {:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))})
                                     card nil))}

   "Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:runner :deck]))]
                   (if (= (:type c) "Program")
                     (do (trash state side c) (gain state side :credit 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}

   "Inside Job"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target))}

   "Knifed"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target))}

   "Kraken"
   {:req (req (:stole-agenda runner-reg)) :prompt "Choose a server" :choices (req servers)
    :msg (msg "force the Corp to trash an ICE protecting " target)}

   "Lawyer Up"
   {:effect (effect (draw 3) (lose :tag 2))}

   "Legwork"
   {:effect (effect (run :hq) (register-events (:events (card-def card))
                                               (assoc card :zone '(:discard))))
    :events {:successful-run {:effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Leverage"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :player :corp
    :prompt "Take 2 bad publicity?"
    :choices ["Yes" "No"]
    :effect (req (if (= target "Yes")
                   (do (gain state :corp :bad-publicity 2) (system-msg state :corp "takes 2 bad publicity"))
                   (do (register-events state side
                                        {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                      (damage-prevent :meat Integer/MAX_VALUE)
                                                                      (damage-prevent :brain Integer/MAX_VALUE))}
                                         :runner-turn-begins {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard)))
                       (system-msg state :runner "is immune to damage until the beginning of the runner's next turn"))))
    ; This :events is a hack so that the unregister-events above will fire.
    :events {:runner-turn-begins nil :pre-damage nil}}

   "Levy AR Lab Access"
   {:effect (effect (shuffle-into-deck :hand :discard) (draw 5)
                    (move (first (:play-area runner)) :rfg))}

   "Lucky Find"
   {:effect (effect (gain :credit 9))}

   "Mass Install"
   {:choices {:max 3 :req #(and (has? % :type "Program") (= (:zone %) [:hand]))}
    :msg (msg "install " (join ", " (map :title targets)))
    :effect (req (doseq [c targets] (runner-install state side c)))}

   "Modded"
   {:prompt "Choose a card to install"
    :choices (req (filter #(#{"Hardware" "Program"} (:type %)) (:hand runner)))
    :effect (effect (install-cost-bonus -3) (runner-install target))}

   "Net Celebrity"
   {:recurring 1}

   "Networking"
   {:effect (effect (lose :tag 1))
    :optional {:cost [:credit 1] :prompt "Pay 1 [Credits] to add Networking to Grip?"
               :msg "add it to his Grip" :effect (effect (move (last (:discard runner)) :hand))}}

   "Notoriety"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :effect (effect (gain-agenda-point 1) (move (first (:play-area runner)) :scored))}

   "Paper Tripping"
   {:effect (effect (lose :tag :all))}

   "Planned Assault"
   {:msg (msg "play " (:title target))
    :choices (req (filter #(and (has? % :subtype "Run")
                                (<= (:cost %) (:credit runner))) (:deck runner)))
    :prompt "Choose a Run event" :effect (effect (play-instant target {:no-additional-cost true}))}

   "Power Nap"
   {:effect (effect (gain :credit (+ 2 (count (filter (fn [c] (has? c :subtype "Double"))
                                                      (:discard runner))))))}

   "Push Your Luck"
   {:player :corp :prompt "Guess the amount the Runner will spend on Push Your Luck"
    :choices ["Even" "Odd"] :msg "make the Corp choose a guess"
    :effect (req (let [guess target]
                   (resolve-ability
                     state :runner
                     {:choices :credit :prompt "How many credits?"
                      :msg (msg "spend " target " [Credits]. The Corp guessed " guess)
                      :effect (req (when (or (and (= guess "Even") (odd? target))
                                             (and (= guess "Odd") (even? target)))
                                     (system-msg state :runner (str "gains " (* 2 target) " [Credits]"))
                                     (gain state :runner :credit (* 2 target))))} card nil)))}

   "Quality Time"
   {:effect (effect (draw 5))}

   "Queens Gambit"
   {:choices ["0", "1", "2", "3"] :prompt "How many advancement tokens?"
    :effect (req (let [c (Integer/parseInt target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(and (= (second (:zone %)) :remote)
                                           (= (last (:zone %)) :content)
                                           (not (:rezzed %)))}
                      :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                      :effect (effect (gain :credit (* 2 c)) (add-prop :corp target :advance-counter c))}
                     card nil)))}

   "Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :choices {:req #(= (first (:zone %)) :servers)} :msg (msg "access " (:title target))
    :effect (effect (handle-access targets))}

   "Recon"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target))}

   "Retrieval Run"
   {:effect (effect (run :archives
                         {:replace-access
                          {:prompt "Choose a program to install" :msg (msg "install " (:title target))
                           :choices (req (filter #(= (:type %) "Program") (:discard runner)))
                           :effect (effect (runner-install target {:no-cost true}))}} card))}

   "Running Interference"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (run target)
                    (register-events {:pre-rez
                                      {:req (req (= (:type target) "ICE"))
                                       :effect (effect (rez-cost-bonus (:cost target)))}
                                      :run-ends
                                      {:effect (effect (unregister-events card))}}
                                     (assoc card :zone '(:discard))))
    :events {:pre-rez nil :run-ends nil}}

   "Satellite Uplink"
   {:msg (msg "expose " (join ", " (map :title targets)))
    :choices {:max 2 :req #(= (first (:zone %)) :servers)}
    :effect (req (doseq [c targets] (expose state side c)))}

   "Scavenge"
   {:choices {:req #(= (:type %) "Program")}
    :effect (req (let [trashed target]
                   (trash state side trashed)
                   (resolve-ability
                     state side
                     {:prompt "Install a card from Grip or Heap?" :choices ["Grip" "Heap"]
                      :effect (req (let [fr target]
                                     (system-msg state side (str "trashes " (:title trashed) " to install a card from " fr))
                                     (resolve-ability
                                       state side
                                       {:prompt "Choose a program to install"
                                        :choices (req (filter #(and (= (:type %) "Program")
                                                                    (<= (:cost %) (+ (:credit runner) (:cost trashed))))
                                                              ((if (= fr "Grip") :hand :discard) runner)))
                                        :effect (effect (install-cost-bonus (- (:cost trashed)))
                                                        (runner-install target))} card nil)))} card nil)))}


   "Scrubbed"
   {:events (let [sc {:effect (req (update! state side (dissoc card :scrubbed-target)))}]
                 {:encounter-ice {:once :per-turn
                                  :effect (effect (update! (assoc card :scrubbed-target target)))}
                  :pre-ice-strength {:req (req (= (:cid target) (:cid (:scrubbed-target card))))
                                     :effect (effect (ice-strength-bonus -2))}
                  :pass-ice sc :run-ends sc})}

   "Singularity"
   {:prompt "Choose a server" :choices (req remotes)
    :effect (effect (run target
                      {:replace-access
                       {:msg "trash all cards in the server at no cost"
                        :effect (req (doseq [c (get-in (:servers corp) (conj (:server run) :content))]
                                       (trash state side c)))}} card))}

   "Special Order"
   {:prompt "Choose an Icebreaker"
    :effect (effect (system-msg (str "adds " (:title target) " to his Grip and shuffles his Stack"))
                    (move target :hand) (shuffle! :deck))
    :choices (req (filter #(has? % :subtype "Icebreaker") (:deck runner)))}

   "Spooned"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target))}

   "Stimhack"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (gain-run-credits 9)
                    (run target {:end-run
                                 {:msg " take 1 brain damage"
                                  :effect (effect (damage :brain 1 {:unpreventable true :card card}))}}
                      card))}

   "Sure Gamble"
   {:effect (effect (gain :credit 9))}

   "Surge"
   {:msg (msg "place 2 virus token on " (:title target))
    :choices {:req #(has? % :subtype "Virus")}
    :effect (effect (add-prop target :counter 2))}

   "Test Run"
   {:prompt "Install a card from Stack or Heap?" :choices ["Stack" "Heap"]
    :msg (msg "install a card from " target) :effect
    (effect (resolve-ability
             {:prompt "Choose a card to install"
              :choices (req (filter #(= (:type %) "Program")
                                    ((if (= target "Heap") :discard :deck) runner)))
              :effect (effect (runner-install (assoc-in target [:special :test-run] true) {:no-cost true}))
              :end-turn
              {:req (req (some #(when (and (= (:cid target) (:cid %)) (get-in % [:special :test-run])) %)
                               (get-in runner [:rig :program])))
               :msg (msg "move " (:title target) " on top of Stack")
               :effect (req (move state side (some #(when (= (:cid target) (:cid %)) %)
                                                   (get-in runner [:rig :program]))
                                  :deck {:front true}))}}
             card targets))}

   "The Makers Eye"
   {:effect (effect (run :rd) (register-events (:events (card-def card))
                                               (assoc card :zone '(:discard))))
    :events {:successful-run {:effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Three Steps Ahead"
   {:end-turn {:effect (effect (gain :credit (* 2 (count (:successful-run runner-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")}}

   "Tinkering"
   {:choices {:req #(and (has? % :type "ICE") (= (first (:zone %)) :servers))} 
    :effect (req (let [ice target]
                   (resolve-ability
                     state :runner
                     {:prompt (msg "Choose a type") 
                      :choices ["sentry" "code gate" "barrier"]
                      :msg (msg "give " (:title ice) " " target " until the end of turn")}
                      card nil)))}
    
   "Trade-In"
   {:prompt "Choose a hardware to trash" :choices {:req #(and (:installed %) (= (:type %) "Hardware"))}
    :msg (msg "trash " (:title target) " and gain " (quot (:cost target) 2) " [Credits]")
    :effect (effect (trash target) (gain [:credit (quot (:cost target) 2)])
                    (resolve-ability {:prompt "Choose a Hardware to add to Grip from Stack"
                                      :choices (req (filter #(= (:type %) "Hardware") (:deck runner)))
                                      :msg (msg "adds " (:title target) " to his Grip")
                                      :effect (effect (move target :hand))} card nil))}

   "Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:pre-advancement-cost
             {:effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored corp)))))}}}

   "Uninstall"
   {:choices {:req #(and (:installed %) (#{"Program" "Hardware"} (:type %)))}
    :msg (msg "move " (:title target) " to his or her grip")
    :effect (effect (move target :hand))}

   "Vamp"
   {:effect (effect (run :hq {:replace-access
                              {:prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                               :effect (effect (lose :corp :credit target) (gain :tag 1))}} card))}

   "Wanton Destruction"
   {:effect (effect
             (run :hq {:replace-access
                       {:msg (msg "Wanton Destruction to force the Corp to discard " target
                                  " cards from HQ at random")
                        :prompt "How many [Click] do you want to spend?"
                        :choices (req (map str (range 1 (inc (:click runner)))))
                        :effect (req (let [n (Integer/parseInt target)]
                                       (when (pay state :runner card :click n)
                                         (trash-cards state :corp (take n (shuffle (:hand corp)))))))}} card))}})
