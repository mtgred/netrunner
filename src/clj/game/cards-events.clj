(in-ns 'game.core)

(def cards-events
  {"Account Siphon"
   {:effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to lose " (min 5 (:credit corp))
                                         " [Credits], gain " (* 2 (min 5 (:credit corp)))
                                         " [Credits] and take 2 tags")
                               :effect (effect (tag-runner 2)
                                               (gain :runner :credit (* 2 (min 5 (:credit corp))))
                                               (lose :corp :credit (min 5 (:credit corp))))}} card))}

   "Amped Up"
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :effect (effect (gain :click 3) (damage :brain 1 {:unpreventable true :card card}))}

   "Apocalypse"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
                           ;; trash cards from right to left
                           ;; otherwise, auto-killing servers would move the cards to the next server
                           ;; so they could no longer be trashed in the same loop
    :msg "trash all installed Corp cards and turn all installed Runner cards facedown"
    :effect (req (doseq [c (->> (all-installed state :corp)
                                (sort-by #(vec (:zone %)))
                                (reverse))]
                   (trash state side c))
                 ;; do hosted cards first so they don't get trashed twice
                 (doseq [c (all-installed state :runner)]
                   (when (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone)))
                     (move state side c [:rig :facedown])
                     (if (:memoryunits c)
                       (gain state :runner :memory (:memoryunits c)))))
                 (doseq [c (all-installed state :runner)]
                   (when (not (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone))))
                     (move state side c [:rig :facedown])
                     (if (:memoryunits c)
                       (gain state :runner :memory (:memoryunits c))))))}

   "Blackmail"
   {:req (req (> (:bad-publicity corp) 0)) :prompt "Choose a server" :choices (req servers)
    :msg "prevent ICE from being rezzed during this run"
    :effect (effect (register-run-flag!
                      card
                      :can-rez
                      (fn [state side card]
                        (if (ice? card)
                          ((constantly false) (system-msg state side (str "is prevented from rezzing ICE on this run by Blackmail")))
                          true)))
                    (run target nil card))}

   "Bribery"
   {:prompt "How many [Credits]?" :choices :credit
    :msg (msg "increase the rez cost of the 1st unrezzed ICE approached by " target " [Credits]")
    :effect (effect (resolve-ability {:prompt "Choose a server" :choices (req servers)
                                      :effect (effect (run target nil card))} card nil))}

   "Calling in Favors"
   {:msg (msg "gain " (count (filter #(has-subtype? % "Connection") (all-installed state :runner)))
              " [Credits]")
    :effect (effect (gain :credit (count (filter #(has-subtype? % "Connection")
                                                 (all-installed state :runner)))))}

   "Career Fair"
   {:prompt "Choose a resource to install from your Grip"
    :choices {:req #(and (is-type? % "Resource")
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}

   "Code Siphon"
   {:effect (effect (run :rd
                         {:replace-access
                          {:prompt "Choose a program to install"
                           :msg (msg "install " (:title target) " and take 1 tag")
                           :choices (req (filter #(is-type? % "Program") (:deck runner)))
                           :effect (effect (install-cost-bonus [:credit (* -3 (count (get-in corp [:servers :rd :ices])))])
                                           (runner-install target) (tag-runner 1) (shuffle! :deck))}} card))}

   "Cyber Threat"
   {:prompt "Choose a server" :choices (req servers)
    :effect (req (let [serv target
                       runtgt [(last (server->zone state serv))]
                       ices (get-in @state (concat [:corp :servers] runtgt [:ices]))]
                   (resolve-ability
                     state :corp
                     {:optional
                      {:prompt (msg "Rez a piece of ICE protecting " serv "?")
                       :yes-ability {:prompt (msg "Choose a piece of " serv " ICE to rez") :player :corp
                                     :choices {:req #(and (not (:rezzed %))
                                                          (= (last (:zone %)) :ices))}
                                     :effect (req (rez state :corp target nil))}
                       :no-ability {:effect (req (swap! state assoc :per-run nil
                                                        :run {:server runtgt :position (count ices) :ices ices
                                                              :access-bonus 0 :run-effect nil})
                                                 (gain-run-credits state :runner (:bad-publicity corp))
                                                 (swap! state update-in [:runner :register :made-run] #(conj % (first runtgt)))
                                                 (trigger-event state :runner :run runtgt))
                                    :msg (msg "make a run on " serv " during which no ICE can be rezzed")}}}
                    card nil)))}

   "Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain :credit 10))}

   "Déjà Vu"
   {:prompt "Choose a card to add to Grip" :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "add " (:title target) " to their Grip")
    :effect (req (move state side target :hand)
                 (when (has-subtype? target "Virus")
                   (resolve-ability state side
                                    {:prompt "Choose a virus to add to Grip"
                                     :msg (msg "add " (:title target) " to their Grip")
                                     :choices (req (cancellable
                                                     (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                                     :effect (effect (move target :hand))} card nil)))}

   "Demolition Run"
   {:prompt "Choose a server" :choices ["HQ" "R&D"]
    :abilities [{:msg (msg "trash " (:title (:card (first (get-in @state [side :prompt])))) " at no cost")
                 :effect (effect (trash-no-cost))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Demolition Run in the Temporary Zone to trash a card being accessed at no cost") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                      (card-init state side c false)
                                      (register-events state side
                                                       {:run-ends {:effect (effect (trash c))}} c)))}
                     card nil))
    :events {:run-ends nil}}

   "Diesel"
   {:msg "draw 3 cards" :effect (effect (draw 3))}

   "Dirty Laundry"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (run target {:end-run {:req (req (:successful run)) :msg " gain 5 [Credits]"
                                           :effect (effect (gain :runner :credit 5))}} card))}

   "Drive By"
   {:choices {:req #(and (is-remote? (second (:zone %)))
                         (= (last (:zone %)) :content)
                         (not (:rezzed %)))}
    :msg (msg "expose " (:title target) (when (#{"Asset" "Upgrade"} (:type target)) " and trash it"))
    :effect (req (expose state side target)
                 (when (#{"Asset" "Upgrade"} (:type target))
                   (trash state side (assoc target :seen true))))}

   "Early Bird"
   {:prompt "Choose a server"
    :choices (req servers)
    :msg (msg "make a run on " target " and gain [Click]")
    :effect (effect (gain :click 1) (run target nil card))}

   "Easy Mark"
   {:msg "gain 3 [Credits]" :effect (effect (gain :credit 3))}

   "Emergency Shutdown"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :msg (msg "derez " (:title target))
    :choices {:req #(and (ice? %)
                         (rezzed? %))}
    :effect (effect (derez target))}

   "Employee Strike"
   {:msg "disable the Corp's identity"
    :effect (req (unregister-events state side (:identity corp)))
    :leave-play (req (when-let [events (:events (card-def (:identity corp)))]
                       (register-events state side events (:identity corp))))}

   "Escher"
   (letfn [(es [] {:prompt "Select two pieces of ICE to swap positions"
                   :choices {:req #(and (installed? %) (ice? %)) :max 2}
                   :effect (req (if (= (count targets) 2)
                                  (do (swap-ice state side (first targets) (second targets))
                                      (resolve-ability state side (es) card nil))
                                  (system-msg state side "has finished rearranging ICE")))})]
     {:effect (effect (run :hq {:replace-access
                                {:msg "rearrange installed ICE"
                                 :effect (effect (resolve-ability (es) card nil))}} card))})

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
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand corp))))}

   "Exploratory Romp"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (run target
                       {:replace-access
                        {:prompt "Advancements to remove from a card in or protecting this server?"
                         :choices ["0", "1", "2", "3"]
                         :effect (req (let [c (Integer/parseInt target)]
                                        (resolve-ability
                                          state side
                                          {:choices {:req #(and (contains? % :advance-counter)
                                                                (= (:server run) (vec (rest (butlast (:zone %))))))}
                                          :msg (msg "remove " c " advancements from "
                                                (card-str state target))
                                          :effect (req (add-prop state :corp target :advance-counter (- c))
                                                       (swap! state update-in [:runner :prompt] rest)
                                                       (handle-end-run state side))}
                                         card nil)))}} card))}

   "Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck runner)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand) (shuffle! :deck))}

   "Feint"
   {:effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:msg "access 0 cards"
                              :effect (effect (max-access 0))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :effect (effect (draw 3) (draw :corp 3))}

   "Forged Activation Orders"
   {:choices {:req #(and (ice? %)
                         (not (rezzed? %)))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       icepos (ice-index state ice)]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Rez " (:title ice) " at position " icepos
                                   " of " serv " or trash it?") :choices ["Rez" "Trash"]
                      :effect (effect (resolve-ability
                                        (if (and (= target "Rez") (<= (rez-cost state :corp ice) (:credit corp)))
                                          {:msg (msg "force the rez of " (:title ice))
                                           :effect (effect (rez :corp ice))}
                                          {:msg (msg "trash the ICE at position " icepos " of " serv)
                                           :effect (effect (trash :corp ice))})
                                        card nil))}
                     card nil)))}

   "Forked"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target nil card))}

   "Frame Job"
   {:prompt "Choose an agenda to forfeit"
    :choices (req (:scored runner))
    :effect (effect (forfeit target) (gain :corp :bad-publicity 1))
    :msg (msg "forfeit " (:title target) " and give the Corp 1 bad publicity")}

   "Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (is-type? % "Program")
                         (in-hand? %))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (effect (trash-cards targets) (gain :credit (* 2 (count targets))))}

   "Game Day"
   {:msg (msg "draw " (- (hand-size state :runner) (count (:hand runner))) " cards")
    :effect (effect (draw (- (hand-size state :runner) (count (:hand runner)))))}

   "Hacktivist Meeting"
   {:events {:rez {:req (req (and (not (ice? target)) (< 0 (count (:hand corp)))))
                   ;; FIXME the above condition is just a bandaid, proper fix would be preventing the rez altogether
                   :msg "force the Corp to trash 1 card from HQ at random"
                   :effect (effect (trash (first (shuffle (:hand corp)))))}}}

   "High-Stakes Job"
   {:prompt "Choose a server"
    :choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                        ok-servs (filter unrezzed-ice (get-in @state [:corp :servers]))]
                    (map (comp zone->name first) ok-servs)))
    :effect (effect (run target {:end-run {:req (req (:successful run)) :msg " gain 12 [Credits]"
                                           :effect (effect (gain :runner :credit 12))}} card))}

   "Hostage"
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck runner)) :sorted))
    :msg (msg "adds " (:title target) " to their Grip and shuffles their Stack")
    :effect (req (let [connection target]
                   (resolve-ability
                     state side
                     {:prompt (str "Install " (:title connection) "?")
                      :choices ["Yes" "No"]
                      :effect (req (let [d target]
                                     (resolve-ability state side
                                       {:effect (req (when (= "Yes" d)
                                                       (runner-install state side connection))
                                                     (shuffle! state side :deck)
                                                     (move state side connection :hand))} card nil)))}
                     card nil)))}

   "Ive Had Worse"
   {:effect (effect (draw 3))
    :trash-effect {:req (req (#{:meat :net} target))
                   :effect (effect (draw :runner 3)) :msg "draw 3 cards"}}

   "Immolation Script"
   {:effect (effect (run :archives nil card) (register-events (:events (card-def card))
                                                              (assoc card :zone '(:discard))))
    :events {:successful-run-ends
             {:req (req (= target :archives))
              :effect (effect (resolve-ability
                                {:prompt "Choose a piece of ICE in Archives"
                                 :choices (req (filter ice? (:discard corp)))
                                 :effect (req (let [icename (:title target)]
                                                (resolve-ability
                                                  state side
                                                  {:prompt (msg "Choose a rezzed copy of " icename " to trash")
                                                   :choices {:req #(and (ice? %)
                                                                        (rezzed? %)
                                                                        (= (:title %) icename))}
                                                   :msg (msg "trash " (card-str state target))
                                                   :effect (req (trash state :corp target))} card nil)))}
                               card nil)
                              (unregister-events card))}}}

   "Independent Thinking"
   (let [cards-to-draw (fn [ts] (* (count ts) (if (not-any? #(has-subtype? % "Directive") ts) 1 2)))]
     {:choices {:max 5 :req #(and (:installed %) (= (:side %) "Runner"))}
      :effect (effect (trash-cards targets) (draw :runner (cards-to-draw targets)))
      :msg (msg "trash " (count targets) " card" (when (not= 1(count targets)) "s") " and draw " (cards-to-draw targets) " cards")})

   "Indexing"
   {:effect (effect (run :rd {:replace-access
                              {:msg "rearrange the top 5 cards of R&D"
                               :effect (req (prompt! state side card
                                                     (str "Drag cards from the Temporary Zone back onto R&D") ["OK"] {})
                                            (doseq [c (take 5 (:deck corp))]
                                              (move state side c :play-area)))}} card))}

   "Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (resolve-ability (if (= target "Expose a card")
                                       {:choices {:req installed?}
                                        :effect (effect (expose target))
                                        :msg (msg "expose " (:title target))}
                                       {:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))})
                                     card nil))}

   "Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:runner :deck]))]
                   (if (is-type? c "Program")
                     (do (trash state side c) (gain state side :credit 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}

   "Inside Job"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target nil card))}

   "Itinerant Protesters"
   {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"
    :effect (req (lose state :corp :hand-size-modification (:bad-publicity corp))
                 (add-watch state :itin
                   (fn [k ref old new]
                     (let [bpnew (get-in new [:corp :bad-publicity])
                           bpold (get-in old [:corp :bad-publicity])]
                       (when (> bpnew bpold)
                         (lose state :corp :hand-size-modification (- bpnew bpold)))
                       (when (< bpnew bpold)
                         (gain state :corp :hand-size-modification (- bpold bpnew)))))))
    :leave-play (req (remove-watch state :itin)
                     (gain state :corp :hand-size-modification (:bad-publicity corp)))}

   "Knifed"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target nil card))}

   "Kraken"
   {:req (req (:stole-agenda runner-reg)) :prompt "Choose a server" :choices (req servers)
    :msg (msg "force the Corp to trash an ICE protecting " target)
    :effect (req (let [serv (next (server->zone state target))
                       servname target]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Choose a piece of ICE in " target " to trash")
                      :choices {:req #(and (= (last (:zone %)) :ices)
                                           (= serv (rest (butlast (:zone %)))))}
                      :effect (req (trash state :corp target)
                                   (system-msg state side (str "trashes "
                                    (card-str state target))))}
                    card nil)))}

   "Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose :tag 2))}

   "Legwork"
   {:effect (effect (run :hq nil card) (register-events (:events (card-def card))
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
                       (system-msg state :runner "is immune to damage until the beginning of the Runner's next turn"))))
    ; This :events is a hack so that the unregister-events above will fire.
    :events {:runner-turn-begins nil :pre-damage nil}}

   "Levy AR Lab Access"
   {:msg "shuffle their Grip and Heap into their Stack and draw 5 cards"
    :effect (effect (shuffle-into-deck :hand :discard) (draw 5)
                    (move (first (:play-area runner)) :rfg))}

   "Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain :credit 9))}

   "Mass Install"
   (let [mhelper (fn mi [n] {:prompt "Select a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (runner-install state side target)
                                            (when (< n 3)
                                              (resolve-ability state side (mi (inc n)) card nil)))})]
     {:effect (effect (resolve-ability (mhelper 1) card nil))})

   "Modded"
   {:prompt "Choose a program or piece of hardware to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}

   "Net Celebrity"
   {:recurring 1}

   "Networking"
   {:msg "remove 1 tag"
    :effect (effect (lose :tag 1))
    :optional {:prompt "Pay 1 [Credits] to add Networking to Grip?"
               :yes-ability {:cost [:credit 1]
                             :msg "add it to their Grip"
                             :effect (effect (move (last (:discard runner)) :hand))}}}

   "Notoriety"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :effect (effect (as-agenda :runner (first (:play-area runner)) 1))
    :msg "add it to their score area and gain 1 agenda point"}

   "Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose :tag :all))}

   "Planned Assault"
   {:msg (msg "play " (:title target))
    :choices (req (cancellable (filter #(and (has-subtype? % "Run")
                                             (<= (:cost %) (:credit runner))) (:deck runner)) :sorted))
    :prompt "Choose a Run event" :effect (effect (play-instant target {:no-additional-cost true}))}

   "Populist Rally"
   {:req (req (seq (filter #(has-subtype? % "Seedy") (all-installed state :runner))))
    :msg "give the Corp 1 fewer [Click] to spend on their next turn"
    :effect (effect (lose :corp :click-per-turn 1)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:corp-turn-ends {:effect (effect (gain :corp :click-per-turn 1))}}}

   "Power Nap"
   {:effect (effect (gain :credit (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard runner))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")}

   "Power to the People"
   {:effect (effect (register-events {:pre-steal-cost
                                      {:once :per-turn :effect (effect (gain :credit 7))
                                                       :msg "gain 7 [Credits]"}
                                      :runner-turn-ends
                                      {:effect (effect (unregister-events card))}}
                    (assoc card :zone '(:discard))))
    :events {:pre-steal-cost nil :runner-turn-ends nil}}

   "Prey"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target nil card))}

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
   {:msg "draw 5 cards" :effect (effect (draw 5))}

   "Queens Gambit"
   {:choices ["0", "1", "2", "3"] :prompt "How many advancement tokens?"
    :effect (req (let [c (Integer/parseInt target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(and (is-remote? (second (:zone %)))
                                           (= (last (:zone %)) :content)
                                           (not (:rezzed %)))}
                      :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                      :effect (effect (gain :credit (* 2 c)) (add-prop :corp target :advance-counter c {:placed true}))}
                     card nil)))}

   "Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :choices {:req installed?} :msg (msg "access " (:title target))
    :effect (effect (handle-access targets))}

   "Recon"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target nil card))}

   "Retrieval Run"
   {:effect (effect (run :archives
                      {:req (req (= target :archives))
                       :replace-access
                       {:prompt "Choose a program to install"
                        :msg (msg "install " (:title target))
                        :choices (req (filter #(is-type? % "Program") (:discard runner)))
                        :effect (effect (runner-install target {:no-cost true}))}} card))}

   "Run Amok"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (run target {:end-run {:msg " trash 1 piece of ICE that was rezzed during the run"}} card))}

   "Running Interference"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (run target nil card)
                    (register-events {:pre-rez
                                      {:req (req (ice? target))
                                       :effect (effect (rez-cost-bonus (:cost target)))}
                                      :run-ends
                                      {:effect (effect (unregister-events card))}}
                                     (assoc card :zone '(:discard))))
    :events {:pre-rez nil :run-ends nil}}

   "Satellite Uplink"
   {:msg (msg "expose " (join ", " (map :title targets)))
    :choices {:max 2 :req installed?}
    :effect (req (doseq [c targets] (expose state side c)))}

   "Scavenge"
   {:req (req (pos? (count (filter #(is-type? % "Program") (all-installed state :runner)))))
    :prompt "Choose an installed program to trash"
    :choices {:req #(and (is-type? % "Program")
                         (installed? %))}
    :effect (req (let [trashed target tcost (- (:cost trashed)) st state si side]
                   (trash state side trashed)
                   (resolve-ability
                     state side
                     {:prompt "Choose a program to install from your grip or heap"
                      :show-discard true
                      :choices {:req #(and (is-type? % "Program")
                                           (#{[:hand] [:discard]} (:zone %))
                                           (can-pay? st si nil (modified-install-cost st si % [:credit tcost])))}
                      :effect (effect (install-cost-bonus [:credit (- (:cost trashed))])
                                      (runner-install target))
                      :msg (msg "trash " (:title trashed) " and install " (:title target))} card nil)))}


   "Scrubbed"
   {:events (let [sc {:effect (req (update! state side (dissoc card :scrubbed-target)))}]
                 {:encounter-ice {:once :per-turn
                                  :effect (effect (update! (assoc card :scrubbed-target target)))}
                  :pre-ice-strength {:req (req (= (:cid target) (:cid (:scrubbed-target card))))
                                     :effect (effect (ice-strength-bonus -2 target))}
                  :pass-ice sc :run-ends sc})}

   "Showing Off"
   {:effect (effect (run :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :effect (req (swap! state assoc-in [:corp :deck]
                                            (rseq (into [] (get-in @state [:corp :deck]))))
                                     (do-access state side (:server run))
                                     (swap! state assoc-in [:corp :deck]
                                            (rseq (into [] (get-in @state [:corp :deck])))))}} card))}

   "Singularity"
   {:prompt "Choose a server" :choices (req remotes)
    :effect (effect (run target
                      {:req (req (is-remote? target))
                       :replace-access
                       {:msg "trash all cards in the server at no cost"
                        :effect (req (doseq [c (get-in (:servers corp) (conj (:server run) :content))]
                                       (trash state side c)))}} card))}

   "Social Engineering"
   {:prompt "Choose an unrezzed piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (not (rezzed? %)) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))]
              (resolve-ability
                 state :runner
                 {:msg (msg "choose the ICE at position " (ice-index state ice) " of " serv)
                  :effect (effect (register-events {:pre-rez-cost
                                                    {:req (req (= target ice))
                                                     :effect (req (let [cost (rez-cost state side (get-card state target))]
                                                                    (gain state :runner :credit cost)))
                                                     :msg (msg "gain " (rez-cost state side (get-card state target)) " [Credits]")}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:pre-rez-cost nil}
    :end-turn {:effect (effect (unregister-events card))}}

   "Special Order"
   {:prompt "Choose an Icebreaker"
    :effect (effect (system-msg (str "adds " (:title target) " to their Grip and shuffles their Stack"))
                    (move target :hand) (shuffle! :deck))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))}

   "Spooned"
   {:prompt "Choose a server" :choices (req servers) :effect (effect (run target nil card))}

   "Stimhack"
   {:prompt "Choose a server" :choices (req servers)
    :effect (effect (gain-run-credits 9)
                    (run target {:end-run
                                 {:msg " take 1 brain damage"
                                  :effect (effect (damage :brain 1 {:unpreventable true :card card}))}}
                      card))}

   "Sure Gamble"
   {:msg "gain 9 [Credits]" :effect (effect (gain :credit 9))}

   "Surge"
   {:msg (msg "place 2 virus tokens on " (:title target))
    :choices {:req #(and (has-subtype? % "Virus") (:added-virus-counter %))}
    :effect (req (add-prop state :runner target :counter 2))}

   "Test Run"
   {:prompt "Install a program from Stack or Heap?"
    :choices (cancellable ["Stack" "Heap"])
    :msg (msg "install a program from " target)
    :effect (effect (resolve-ability
                     {:prompt "Choose a program to install"
                      :choices (req (cancellable
                                     (filter #(is-type? % "Program")
                                             ((if (= target "Heap") :discard :deck) runner))))
                      :effect (effect (runner-install (assoc-in target [:special :test-run] true) {:no-cost true}))
                      :end-turn
                      {:req (req (some #(when (and (= (:cid target) (:cid %))
                                                   (get-in % [:special :test-run])) %)
                                       (get-in runner [:rig :program])))
                       :msg (msg "move " (:title target) " on top of their Stack")
                       :effect (req (move state side (some #(when (= (:cid target) (:cid %)) %)
                                                           (get-in runner [:rig :program]))
                                          :deck {:front true}))}}
                     card targets))}

   "The Makers Eye"
   {:effect (effect (run :rd nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Three Steps Ahead"
   {:end-turn {:effect (effect (gain :credit (* 2 (count (:successful-run runner-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")}}

   "Tinkering"
   {:prompt "Choose a piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       stypes (:subtype ice)]
              (resolve-ability
                 state :runner
                 {:msg (msg "make " (card-str state ice) " gain sentry, code gate, and barrier until the end of the turn")
                  :effect (effect (update! (assoc ice :subtype
                                                      (->> (vec (.split (:subtype ice) " - "))
                                                           (concat ["Sentry" "Code Gate" "Barrier"])
                                                           distinct
                                                           (join " - "))))
                                  (update-ice-strength (get-card state ice))
                                  (register-events {:runner-turn-ends
                                                    {:effect (effect (update! (assoc (get-card state ice) :subtype stypes)))}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:runner-turn-ends nil}}

   "Trade-In"
   {:prompt "Choose a hardware to trash"
    :choices {:req #(and (installed? %)
                         (is-type? % "Hardware"))}
    :msg (msg "trash " (:title target) " and gain " (quot (:cost target) 2) " [Credits]")
    :effect (effect (trash target) (gain [:credit (quot (:cost target) 2)])
                    (resolve-ability {:prompt "Choose a Hardware to add to Grip from Stack"
                                      :choices (req (filter #(is-type? % "Hardware")
                                                            (:deck runner)))
                                      :msg (msg "adds " (:title target) " to their Grip")
                                      :effect (effect (move target :hand))} card nil))}

   "Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:pre-advancement-cost
             {:effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored corp)))))}}}

   "Uninstall"
   {:choices {:req #(and (installed? %)
                         (#{"Program" "Hardware"} (:type %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}

   "Vamp"
   {:effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                               :effect (effect (lose :corp :credit target) (tag-runner 1))}} card))}

   "Wanton Destruction"
   {:effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to discard " target " cards from HQ at random")
                               :prompt "How many [Click] do you want to spend?"
                               :choices (req (map str (range 1 (inc (:click runner)))))
                               :effect (req (let [n (Integer/parseInt target)]
                                              (when (pay state :runner card :click n)
                                                (trash-cards state :corp (take n (shuffle (:hand corp)))))))}} card))}

   "Windfall"
   {:effect (req (shuffle! state :runner :deck)
                 (let [topcard (first (:deck runner))
                       cost (:cost topcard)]
                   (if (is-type? topcard "Event")
                     (do (trash state side topcard)
                         (system-msg state side (str "shuffles their Stack and trashes "
                          (:title topcard) " to gain 0 [Credits]")))
                     (do (gain state side :credit cost)
                         (trash state side topcard)
                         (system-msg state side (str "shuffles their Stack and trashes "
                          (:title topcard) " to gain " cost " [Credits]"))))))}})
