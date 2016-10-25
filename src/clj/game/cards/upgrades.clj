(in-ns 'game.core)
(declare expose-prevent)

(def cards-upgrades
  {"Akitaro Watanabe"
   {:events {:pre-rez-cost {:req (req (and (ice? target)
                                           (= (card->server state card) (card->server state target))))
                            :effect (effect (rez-cost-bonus -2))}}}

   "Amazon Industrial Zone"
   {:events
     {:corp-install  {:optional {:req (req (and (ice? target)
                                                (= (card->server state card) (card->server state target))))
                                 :prompt "Rez ICE with rez cost lowered by 3?" :priority 2
                                 :yes-ability {:effect (effect (rez-cost-bonus -3) (rez target))}}}}}

   "Ash 2X3ZB9CY"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
                              :trace {:base 4
                                      :effect (req (max-access state side 0)
                                                   (when-not (:replace-access (get-in @state [:run :run-effect]))
                                                     (let [ash card]
                                                       (swap! state update-in [:run :run-effect]
                                                              #(assoc % :replace-access
                                                                        {:mandatory true
                                                                         :effect (effect (handle-access [ash])) :card ash})))))
                                      :msg "prevent the Runner from accessing cards other than Ash 2X3ZB9CY"}}}}

   "Awakening Center"
   {:abilities [{:label "Host a piece of Bioroid ICE"
                 :cost [:click 1]
                 :prompt "Select a piece of Bioroid ICE to host on Awakening Center"
                 :choices {:req #(and (ice? %)
                                      (has-subtype? % "Bioroid")
                                      (in-hand? %))}
                 :msg "host a piece of Bioroid ICE"
                 :effect (effect (trigger-event :corp-install target)
                                 (host card target {:facedown true}))}
                {:req (req (and this-server (= (get-in @state [:run :position]) 0)))
                 :label "Rez a hosted piece of Bioroid ICE"
                 :prompt "Choose a piece of Bioroid ICE to rez" :choices (req (:hosted card))
                 :msg (msg "lower the rez cost of " (:title target) " by 7 [Credits] and force the Runner to encounter it")
                 :effect (effect (rez-cost-bonus -7) (rez target)
                                 (update! (dissoc (get-card state target) :facedown))
                                 (register-events {:run-ends
                                                    {:effect (req (doseq [c (:hosted card)]
                                                                    (when (:rezzed c)
                                                                      (trash state side c)))
                                                                  (unregister-events state side card))}} card))}]
    :events {:run-ends nil}}

   "Bernice Mai"
   {:events {:successful-run {:req (req this-server)
                              :trace {:base 5 :msg "give the Runner 1 tag"
                                      :effect (effect (tag-runner :runner 1))
                                      :unsuccessful {:effect (effect (system-msg "trashes Bernice Mai from the unsuccessful trace")
                                                                     (trash card))}}}}}

   "Breaker Bay Grid"
   {:events {:pre-rez-cost {:req (req (and (is-remote? (second (:zone card)))
                                           (or (= (:zone card) (:zone target))
                                               (= (:zone card) (:zone (get-card state (:host target)))))))
                            :effect (effect (rez-cost-bonus -5))}}}

   "Caprice Nisei"
   {:events {:pass-ice {:req (req (and this-server
                                       (= (:position run) 1))) ; trigger when last ice passed
                        :msg "start a Psi game"
                        :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}
             :run {:req (req (and this-server
                                  (= (:position run) 0))) ; trigger on unprotected server
                   :msg "start a Psi game"
                   :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}}
    :abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}]}

   "ChiLo City Grid"
   {:events {:successful-trace {:req (req this-server)
                                :effect (effect (tag-runner :runner 1))
                                :msg "give the Runner 1 tag"}}}

   "Corporate Troubleshooter"
   {:abilities [{:label "[Trash]: Add strength to a rezzed ICE protecting this server" :choices :credit
                 :prompt "How many credits?"
                 :effect (req (let [boost target]
                                (resolve-ability
                                  state side
                                  {:choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "add " boost " strength to " (:title target))
                                   :effect (req (update! state side (assoc card :troubleshooter-target target
                                                                                :troubleshooter-amount boost))
                                                (trash state side (get-card state card))
                                                (update-ice-strength state side target))} card nil)))}]
    :events {:pre-ice-strength nil :runner-turn-ends nil :corp-turn-ends nil}
    :trash-effect
               {:effect (req (register-events
                               state side
                               (let [ct {:effect (req (unregister-events state side card)
                                                      (update! state side (dissoc card :troubleshooter-target))
                                                      (update-ice-strength state side (:troubleshooter-target card)))}]
                                 {:pre-ice-strength
                                                    {:req (req (= (:cid target) (:cid (:troubleshooter-target card))))
                                                     :effect (effect (ice-strength-bonus (:troubleshooter-amount card) target))}
                                  :runner-turn-ends ct :corp-turn-ends ct}) card))}}

   "Crisium Grid"
   (let [suppress-event {:req (req (and this-server (not= (:cid target) (:cid card))))}]
     {:suppress {:pre-successful-run suppress-event
                 :successful-run suppress-event}
      :events {:pre-successful-run {:silent (req true)
                                    :req (req this-server)
                                    :effect (req (swap! state update-in [:run :run-effect] dissoc :replace-access)
                                                 (swap! state update-in [:run] dissoc :successful)
                                                 (swap! state update-in [:runner :register :successful-run] #(rest %)))}}})

   "Cyberdex Virus Suite"
   {:access {:delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Cyberdex Virus Suite")
                             (continue-ability
                               {:optional {:prompt "Purge virus counters with Cyberdex Virus Suite?"
                                           :yes-ability {:msg (msg "purge virus counters")
                                                         :effect (effect (clear-wait-prompt :runner)
                                                                         (purge))}
                                           :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil))}
    :abilities [{:label "[Trash]: Purge virus counters"
                 :msg "purge virus counters" :effect (effect (trash card) (purge))}]}

   "Dedicated Technician Team"
   {:recurring 2}

   "Disposable HQ"
   (letfn [(dhq [n i]
             {:req (req (pos? i))
              :prompt "Select a card in HQ to add to the bottom of R&D"
              :choices {:req #(and (= (:side %) "Corp")
                                   (in-hand? %))}
              :msg "add a card to the bottom of R&D"
              :effect (req (move state side target :deck)
                           (when (< n i)
                             (resolve-ability state side (dhq (inc n) i) card nil)))})]
     {:access {:effect (req (let [n (count (:hand corp))]
                              (show-wait-prompt state :runner "Corp to finish using Disposable HQ")
                              (resolve-ability state side
                                {:optional
                                 {:prompt "Use Disposable HQ to add cards to the bottom of R&D?"
                                  :yes-ability {:msg "add cards in HQ to the bottom of R&D"
                                                :effect (effect (resolve-ability (dhq 1 n) card nil))}
                                  :end-effect (effect (clear-wait-prompt :runner))}}
                               card nil)))}})

   "Drone Screen"
   {:events {:run {:req (req (and this-server tagged))
                   :delayed-completion true
                   :trace {:base 3
                           :msg "do 1 meat damage"
                           :effect (effect (damage eid :meat 1 {:card card :unpreventable true}))}}}}

   "Experiential Data"
   {:effect (req (update-ice-in-server state side (card->server state card)))
    :events {:pre-ice-strength {:req (req (= (card->server state card) (card->server state target)))
                                :effect (effect (ice-strength-bonus 1 target))}}
    :derez-effect {:effect (req (update-ice-in-server state side (card->server state card)))}
    :trash-effect {:effect (req (update-all-ice state side))}}

   "Expo Grid"
   (let [ability {:req (req (some #(and (is-type? % "Asset")
                                        (rezzed? %))
                                  (get-in corp (:zone card))))
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :label "Gain 1 [Credits] (start of turn)"
                  :effect (effect (gain :credit 1))}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})

   "Georgia Emelyov"
   {:events {:unsuccessful-run {:req (req (= (first (:server target)) (second (:zone card))))
                                :delayed-completion true
                                :msg "do 1 net damage"
                                :effect (effect (damage eid :net 1 {:card card}))}}
    :abilities [{:cost [:credit 2]
                 :label "Move to another server"
                 :delayed-completion true
                 :effect (effect (continue-ability
                                   {:prompt "Choose a server"
                                    :choices (butlast (server-list state side))
                                    :msg (msg "move to " target)
                                    :effect (req (let [c (move state side card
                                                               (conj (server->zone state target) :content))]
                                                   (unregister-events state side card)
                                                   (register-events state side (:events (card-def c)) c)))}
                                   card nil))}]}

   "Heinlein Grid"
   {:abilities [{:req (req this-server)
                 :label "Force the Runner to lose all [Credits] from spending or losing a [Click]"
                 :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]") :once :per-run
                 :effect (effect (lose :runner :credit :all))}]}

   "Hokusai Grid"
   {:events {:successful-run {:req (req this-server) :msg "do 1 net damage"
                              :delayed-completion true
                              :effect (effect (damage eid :net 1 {:card card}))}}}

   "Keegan Lane"
   {:abilities [{:label "[Trash], remove a tag: Trash a program"
                 :req (req (and this-server
                                (pos? (get-in @state [:runner :tag]))
                                (not (empty? (filter #(is-type? % "Program")
                                                     (all-installed state :runner))))))
                 :msg (msg "remove 1 tag")
                 :effect (req (resolve-ability state side trash-program card nil)
                              (trash state side card {:cause :ability-cost})
                              (lose state :runner :tag 1))}]}

   "Marcus Batty"
   {:abilities [{:req (req this-server)
                 :label "[Trash]: Start a Psi game"
                 :msg "start a Psi game"
                 :psi {:not-equal {:prompt "Select a rezzed piece of ICE to resolve one of its subroutines"
                                   :choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "resolve a subroutine on " (:title target))}}
                 :effect (effect (trash card))}]}

   "Midori"
   {:abilities
    [{:req (req this-server)
      :label "Swap the ICE being approached with a piece of ICE from HQ"
      :prompt "Select a piece of ICE"
      :choices {:req #(and (ice? %)
                           (in-hand? %))}
      :once :per-run
      :msg (msg "swap " (card-str state current-ice) " with a piece of ICE from HQ")
      :effect (req (let [hqice target
                         c current-ice]
                     (resolve-ability state side
                       {:effect (req (let [newice (assoc hqice :zone (:zone c))
                                           cndx (ice-index state c)
                                           ices (get-in @state (cons :corp (:zone c)))
                                           newices (apply conj (subvec ices 0 cndx) newice (subvec ices cndx))]
                                       (swap! state assoc-in (cons :corp (:zone c)) newices)
                                       (swap! state update-in [:corp :hand]
                                              (fn [coll] (remove-once #(not= (:cid %) (:cid hqice)) coll)))
                                       (trigger-event state side :corp-install newice)
                                       (move state side c :hand)))} card nil)))}]}

   "Mumbad City Grid"
   {:abilities [{:req (req this-server)
                 :label "Swap the ICE just passed with another piece of ICE protecting this server"
                 :effect (req (let [passed-ice (nth (get-in @state (vec (concat [:corp :servers] (:server run) [:ices])))
                                                                                (:position run))
                                    ice-zone (:zone passed-ice)]
                                 (resolve-ability state :corp
                                   {:prompt (msg "Select a piece of ICE to swap with " (:title passed-ice))
                                    :choices {:req #(and (= ice-zone (:zone %)) (ice? %))}
                                    :effect (req (let [fndx (ice-index state passed-ice)
                                                       sndx (ice-index state target)
                                                       fnew (assoc passed-ice :zone (:zone target))
                                                       snew (assoc target :zone (:zone passed-ice))]
                                                   (swap! state update-in (cons :corp ice-zone)
                                                          #(assoc % fndx snew))
                                                   (swap! state update-in (cons :corp ice-zone)
                                                          #(assoc % sndx fnew))
                                                   (update-ice-strength state side fnew)
                                                   (update-ice-strength state side snew)))} card nil)
                                 (system-msg state side (str "uses Mumbad City Grid to swap " (card-str state passed-ice)
                                                             " with " (card-str state target)))))}]}

   "Mumbad Virtual Tour"
   {:access {:req (req installed)
             :effect (req (let [trash-cost (trash-cost state side card)
                                slow-trash (any-flag-fn? state :runner :slow-trash true)]
                            (if (and (can-pay? state :runner nil :credit trash-cost)
                                     (not slow-trash))
                              (do (toast state :runner "You have been forced to trash Mumbad Virtual Tour" "info")
                                  (swap! state assoc-in [:runner :register :force-trash] true))
                              (toast state :runner
                                     (str "You must trash Mumbad Virtual Tour, if able, using any available means "
                                          "(Whizzard, Imp, Ghost Runner, Net Celebrity...)")))))}
    :trash-effect {:when-unrezzed true
                   :effect (req (swap! state assoc-in [:runner :register :force-trash] false))}}

   "NeoTokyo Grid"
   (let [ng {:req (req (and (= (second (:zone target)) (second (:zone card)))
                            (#{:content} (last (:zone target)))
                            (is-remote? (second (:zone card))))) :once :per-turn
             :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}]
     {:events {:advance ng :advancement-placed ng}})

   "Oaktown Grid"
   {:events {:pre-trash {:req (req (and (is-remote? (second (:zone card)))
                                           (or (= (:zone card) (:zone target))
                                               (= (:zone card) (:zone (get-card state (:host target)))))))
                         :effect (effect (trash-cost-bonus 3))}}}

   "Off the Grid"
   {:effect (req (prevent-run-on-server state card (second (:zone card))))
    :events {:runner-turn-begins {:effect (req (prevent-run-on-server state card (second (:zone card))))}
             :successful-run {:req (req (= target :hq))
                              :effect (req (trash state :corp card)
                                           (enable-run-on-server state card
                                                                 (second (:zone card)))
                                           (system-msg state :corp (str "trashes Off the Grid")))}}
    :leave-play (req (enable-run-on-server state card (second (:zone card))))}

   "Old Hollywood Grid"
   (let [ohg {:req (req this-server)
              :effect (effect (register-run-flag!
                                card :can-steal
                                (fn [state side card]
                                  (if-not (some #(= (:title %) (:title card)) (:scored runner))
                                    ((constantly false)
                                     (toast state :runner "Cannot steal due to Old Hollywood Grid." "warning"))
                                    true))))}]
     (assoc-in ohg [:events :run] ohg))

   "Panic Button"
   {:init {:root "HQ"} :abilities [{:cost [:credit 1] :label "Draw 1 card" :effect (effect (draw))
                                    :req (req (and run (= (first (:server run)) :hq)))}]}

   "Port Anson Grid"
   {:msg "prevent the Runner from jacking out unless they trash an installed program"
    :effect (req (when this-server
                   (prevent-jack-out state side)))
    :events {:run {:req (req this-server)
                   :msg "prevent the Runner from jacking out unless they trash an installed program"
                   :effect (effect (prevent-jack-out))}
             :runner-trash {:req (req (and this-server (is-type? target "Program")))
                            :effect (req (swap! state update-in [:run] dissoc :cannot-jack-out))}}}

   "Prisec"
   {:access {:req (req (installed? card))
             :delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Prisec")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 2 [Credits] to use Prisec ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:cost [:credit 2]
                                               :msg "do 1 meat damage and give the Runner 1 tag"
                                               :delayed-completion true
                                               :effect (req (when-completed (damage state side :meat 1 {:card card})
                                                                            (do (tag-runner state :runner 1)
                                                                                ;; TO-DO: extend effect-completed to tag prevention
                                                                                (effect-completed state side eid))))}}}
                               card nil))}}

   "Product Placement"
   {:access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain :corp :credit 2))}}

   "Red Herrings"
   (let [ab {:req (req (or (= (:zone card) (:zone target)) (= (central->zone (:zone target)) (butlast (:zone card)))))
             :effect (effect (steal-cost-bonus [:credit 5]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends nil}})

   "Research Station"
   {:init {:root "HQ"}
    :in-play [:hand-size-modification 2]}

   "Ruhr Valley"
   {:events {:run {:req (req this-server)
                   :effect (effect (lose :runner :click 1))
                   :msg "force the Runner to spend an additional [Click]"}
             :runner-turn-begins {:req (req (> (:click-per-turn runner) 1))
                                  :effect (req (enable-run-on-server state card (second (:zone card))))}
             :runner-spent-click {:req (req (<= 1 (:click runner)))
                                  :effect (req (prevent-run-on-server state card (second (:zone card))))}
             :leave-play (req (enable-run-on-server state card (second (:zone card))))}}

   "Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}

   "Ryon Knight"
   {:abilities [{:label "[Trash]: Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-server (zero? (:click runner))))
                 :delayed-completion true
                 :effect (effect (trash card) (damage eid :brain 1 {:card card}))}]}

   "SanSan City Grid"
   {:effect (req (when-let [agenda (some #(when (is-type? % "Agenda") %)
                                         (:content (card->server state card)))]
                   (update-advancement-cost state side agenda)))
    :events {:corp-install {:req (req (and (is-type? target "Agenda")
                                           (= (:zone card) (:zone target))))
                            :effect (effect (update-advancement-cost target))}
             :pre-advancement-cost {:req (req (= (:zone card) (:zone target)))
                                    :effect (effect (advancement-cost-bonus -1))}}}

   "Satellite Grid"
   {:effect (req (doseq [c (:ices (card->server state card))]
                   (set-prop state side c :extra-advance-counter 1))
                 (update-all-ice state side))
    :events {:corp-install {:req (req (and (ice? target)
                                           (= (card->server state target) (card->server state card))))
                            :effect (effect (set-prop target :extra-advance-counter 1))}}
    :leave-play (req (doseq [c (:ices (card->server state card))]
                       (update! state side (dissoc c :extra-advance-counter)))
                     (update-all-ice state side))}

   "Self-destruct"
   {:abilities [{:req (req this-server)
                 :label "[Trash]: Trace X - Do 3 net damage"
                 :effect (req (let [serv (card->server state card)
                                    cards (concat (:ices serv) (:content serv))]
                                (trash state side card)
                                (doseq [c cards] (trash state side c))
                                (resolve-ability
                                  state side
                                  {:trace {:base (req (dec (count cards)))
                                           :effect (effect (damage eid :net 3 {:card card}))
                                           :msg "do 3 net damage"}} card nil)))}]}

   "Shell Corporation"
   {:abilities
    [{:cost [:click 1]
      :msg "store 3 [Credits]" :once :per-turn
      :effect (effect (add-counter card :credit 3))}
     {:cost [:click 1]
      :msg (msg "gain " (get-in card [:counter :credit] 0) " [Credits]") :once :per-turn
      :label "Take all credits"
      :effect (effect (gain :credit (get-in card [:counter :credit] 0))
                      (set-prop card :counter {:credit 0}))}]}

   "Simone Diego"
   {:recurring 2}

   "Strongbox"
   (let [ab {:req (req (or (= (:zone card) (:zone target)) (= (central->zone (:zone target)) (butlast (:zone card)))))
             :effect (effect (steal-cost-bonus [:click 1]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends nil}})

   "Surat City Grid"
   {:events
    {:rez {:req (req (and (= (second (:zone target)) (second (:zone card)))
                          (not (and (is-type? target "Upgrade")
                                    (is-central? (second (:zone target)))))
                          (not= (:cid target) (:cid card))
                          (seq (filter #(and (not (rezzed? %))
                                             (not (is-type? % "Agenda"))) (all-installed state :corp)))))
           :effect (effect (resolve-ability
                             {:optional
                              {:prompt (msg "Rez another card with Surat City Grid?")
                               :yes-ability {:prompt "Select a card to rez"
                                             :choices {:req #(and (not (rezzed? %))
                                                                  (not (is-type? % "Agenda")))}
                                             :msg (msg "rez " (:title target) ", lowering the rez cost by 2 [Credits]")
                                             :effect (effect (rez-cost-bonus -2)
                                                             (rez target))}}}
                            card nil))}}}

   "The Twins"
   {:abilities [{:label "Reveal and trash a copy of the ICE just passed from HQ"
                 :req (req (and this-server
                                (> (count (get-run-ices state)) (:position run))
                                (:rezzed (get-in (:ices (card->server state card)) [(:position run)]))))
                 :effect (req (let [icename (:title (get-in (:ices (card->server state card)) [(:position run)]))]
                                (resolve-ability
                                  state side
                                  {:prompt "Select a copy of the ICE just passed"
                                   :choices {:req #(and (in-hand? %)
                                                        (ice? %)
                                                        (= (:title %) icename))}
                                   :effect (req (trash state side (assoc target :seen true))
                                                (swap! state update-in [:run]
                                                       #(assoc % :position (inc (:position run)))))
                                   :msg (msg "trash a copy of " (:title target) " from HQ and force the Runner to encounter it again")}
                                 card nil)))}]}

   "Tori Hanzō"
   {:events
    {:pre-resolve-damage
     {:once :per-run
      :req (req (and this-server (= target :net) (> (last targets) 0) (can-pay? state :corp nil [:credit 2])))
      :effect (req (swap! state assoc-in [:damage :damage-replace] true)
                   (damage-defer state side :net (last targets))
                   (show-wait-prompt state :runner "Corp to use Tori Hanzō")
                   (resolve-ability state side
                     {:optional {:prompt (str "Pay 2 [Credits] to do 1 brain damage with Tori Hanzō?") :player :corp
                                 :yes-ability {:msg "do 1 brain damage instead of net damage"
                                               :effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                                            (clear-wait-prompt state :runner)
                                                            (pay state :corp card :credit 2)
                                                            (damage state side eid :brain 1 {:card card}))}
                                 :no-ability {:effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                                           (clear-wait-prompt state :runner)
                                                           (damage state side eid :net (get-defer-damage state side :net nil)
                                                                   {:card card}))}}} card nil))}
     :prevented-damage {:req (req (and this-server (= target :net) (> (last targets) 0)))
                        :effect (req (swap! state assoc-in [:per-run (:cid card)] true))}}}

   "Tyrs Hand"
   {:abilities [{:label "[Trash]: Prevent a subroutine on a piece of Bioroid ice from being broken"
                 :req (req (and (= (butlast (:zone current-ice)) (butlast (:zone card)))
                                (has-subtype? current-ice "Bioroid")))
                 :effect (effect (trash card))
                 :msg (msg "prevent a subroutine on " (:title current-ice) " from being broken")}]}

   "Underway Grid"
   {:events {:pre-expose {:req (req (= (take 2 (:zone target)) (take 2 (:zone card))))
                          :msg "prevent 1 card from being exposed"
                          :effect (effect (expose-prevent 1))}}}

   "Valley Grid"
   {:abilities [{:req (req this-server)
                 :label "Reduce Runner's maximum hand size by 1 until start of next Corp turn"
                 :msg "reduce the Runner's maximum hand size by 1 until the start of the next Corp turn"
                 :effect (req (update! state side (assoc card :times-used (inc (get card :times-used 0))))
                              (lose state :runner :hand-size-modification 1))}]
    :trash-effect {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
                   :effect (req (when-let [n (:times-used card)]
                                  (register-events state side
                                                   {:corp-turn-begins
                                                    {:msg (msg "increase the Runner's maximum hand size by " n)
                                                     :effect (effect (gain :runner :hand-size-modification n)
                                                                     (unregister-events card)
                                                                     (update! (dissoc card :times-used)))}}
                                                   (assoc card :zone '(:discard)))))}
    :events {:corp-turn-begins {:req (req (:times-used card))
                                :msg (msg "increase the Runner's maximum hand size by "
                                          (:times-used card))
                                :effect (effect (gain :runner :hand-size-modification
                                                      (:times-used card))
                                                (update! (dissoc card :times-used)))}}}

   "Will-o-the-Wisp"
   {:abilities [{:label "[Trash]: Add an icebreaker to the bottom of Stack"
                 :choices {:req #(has-subtype? % "Icebreaker")}
                 :msg (msg "add " (:title target) " to the bottom of Stack")
                 :effect (effect (trash card) (move :runner target :deck))}]}})
