(ns game.cards.upgrades
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.eid :refer [effect-completed]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer :all]))

(defn- counter-based-extra-cost
  "Cold Site Server and Reduced Service. Modify cost to run current server whenever counters are added or removed.
  Unit-Costs is a vec like [:credit 1 :click 1] saying how much extra cost 1 counter adds.
  NOTE: If card has an :effect or :leave-play, see function source for the things they need to do to ensure tracking works."
  [unit-costs cdef]
  (let [store-key [:special :current-added-cost]]
       (letfn [(reset-cost [state card amt]
                 (swap! state update-in [:corp :servers (second (:zone card)) :additional-cost]
                        #(merge-costs (concat % (vec (flatten (map (fn [x] [(first x) (* amt (second x))]) (partition 2 unit-costs))))))))
               (recompute-cost [state card]
                 (let [change ((fnil - 0 0) (get-counters card :power) (get-in card store-key))]
                   (reset-cost state card change)
                   (update! state :corp (assoc-in card store-key (get-counters card :power)))))
               (clear-cost [state card]
                 (reset-cost state card (- (get-in card store-key 0)))
                 (update! state :corp (assoc-in card store-key 0)))]
         (merge cdef
                {:events (merge {:counter-added {:req (req (same-card? target card))
                                                 :effect (req (recompute-cost state card))}}
                                (:events cdef))
                 :effect (if (:effect cdef) (:effect cdef)
                             (req (add-counter state side (get-card state card) :power 0)))
                 :leave-play (if (:leave-play cdef) (:leave-play cdef)
                                 (req (clear-cost state card)))}))))

;; Card definitions
(def card-definitions
  {"Akitaro Watanabe"
   {:events {:pre-rez-cost {:req (req (and (ice? target)
                                           (= (card->server state card) (card->server state target))))
                            :effect (effect (rez-cost-bonus -2))}}}

   "Amazon Industrial Zone"
   {:events
    {:corp-install  {:optional {:req (req (and (ice? target)
                                               (protecting-same-server? card target)))
                                :prompt "Rez ICE with rez cost lowered by 3?" :priority 2
                                :yes-ability {:effect (effect (rez-cost-bonus -3) (rez target))}}}}}

   "Arella Salvatore"
   (let [select-ability
         {:prompt "Select a card to install with Arella Salvatore"
          :choices {:req #(and (corp-installable-type? %)
                               (in-hand? %)
                               (corp? %))}
          :async true
          :cancel-effect (req (effect-completed state side eid))
          :effect (req (wait-for (corp-install state :corp target nil {:ignore-all-cost true :display-message false})
                                 (let [inst-target (find-latest state target)]
                                   (add-prop state :corp inst-target :advance-counter 1 {:placed true})
                                   (system-msg state :corp
                                               (str "uses Arella Salvatore to install and place a counter on "
                                                    (card-str state inst-target) ", ignoring all costs"))
                                   (effect-completed state side eid))))}]
     {:events
      {:agenda-scored
       {:req (req (= (:previous-zone target) (:zone card)))
        :interactive (req (some corp-installable-type? (:hand corp)))
        :silent (req (not-any? corp-installable-type? (:hand corp)))
        :async true
        :effect (req (if (some corp-installable-type? (:hand corp))
                       (continue-ability state side select-ability card nil)
                       (effect-completed state side eid)))}}})

   "Ash 2X3ZB9CY"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
                              :trace {:base 4
                                      :successful
                                      {:msg "prevent the Runner from accessing cards other than Ash 2X3ZB9CY"
                                       :effect (req (max-access state side 0)
                                                    (when-not (:replace-access (get-in @state [:run :run-effect]))
                                                      (let [ash card]
                                                        (swap! state update-in [:run :run-effect]
                                                               #(assoc % :replace-access
                                                                       {:mandatory true
                                                                        :async true
                                                                        :effect (req (wait-for (access-card state :runner ash)
                                                                                               (effect-completed state side eid)))
                                                                        :card ash})))))}}}}}

   "Awakening Center"
   {:can-host (req (ice? target))
    :abilities [{:label "Host a piece of Bioroid ICE"
                 :cost [:click 1]
                 :prompt "Select a piece of Bioroid ICE to host on Awakening Center"
                 :choices {:req #(and (ice? %)
                                      (has-subtype? % "Bioroid")
                                      (in-hand? %))}
                 :msg "host a piece of Bioroid ICE"
                 :effect (req (corp-install state side target card {:ignore-all-cost true}))}
                {:req (req (and this-server
                                (zero? (get-in @state [:run :position]))))
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

   "Bamboo Dome"
   (letfn [(dome [dcard]
             {:prompt "Select a card to add to HQ"
              :async true
              :choices {:req #(and (corp? %)
                                   (in-play-area? %))}
              :msg "move a card to HQ"
              :effect (effect (move target :hand)
                              (continue-ability (put dcard) dcard nil))})
           (put [dcard]
             {:prompt "Select first card to put back onto R&D"
              :async true
              :choices {:req #(and (corp? %)
                                   (in-play-area? %))}
              :msg "move remaining cards back to R&D"
              :effect (effect (move target :deck {:front true})
                              (move (first (get-in @state [:corp :play-area])) :deck {:front true})
                              (clear-wait-prompt :runner)
                              (effect-completed eid))})]
     {:init {:root "R&D"}
      :install-req (req (filter #{"R&D"} targets))
      :abilities [{:cost [:click 1]
                   :req (req (>= (count (:deck corp)) 3))
                   :async true
                   :msg (msg (str "reveal " (join ", " (map :title (take 3 (:deck corp)))) " from R&D"))
                   :label "Reveal the top 3 cards of R&D. Secretly choose 1 to add to HQ. Return the others to the top of R&D, in any order."
                   :effect (req (reveal state side (take 3 (:deck corp)))
                                (doseq [c (take 3 (:deck corp))]
                                  (move state side c :play-area))
                                (show-wait-prompt state :runner "Corp to use Bamboo Dome")
                                (continue-ability state side (dome card) card nil))}]})

   "Ben Musashi"
   (let [bm {:req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:net 2]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc bm :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost bm :run-ends nil}})

   "Bernice Mai"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
                              :trace {:base 5
                                      :successful {:msg "give the Runner 1 tag"
                                                   :async true
                                                   :effect (effect (gain-tags :corp eid 1))}
                                      :unsuccessful
                                      {:effect (effect (system-msg "trashes Bernice Mai from the unsuccessful trace")
                                                       (trash card))}}}}}

   "Bio Vault"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :advanceable :always
    :abilities [{:label "End the run"
                 :advance-counter-cost 2
                 :req (req (:run @state))
                 :msg "end the run"
                 :async true
                 :cost [:trash]
                 :effect (effect (end-run eid card))}]}

   "Black Level Clearance"
   {:events {:successful-run
             {:interactive (req true)
              :req (req this-server)
              :async true
              :effect (effect (continue-ability
                                {:prompt "Take 1 brain damage or jack out?"
                                 :player :runner
                                 :choices ["Take 1 brain damage" "Jack out"]
                                 :effect (req (if (= target "Take 1 brain damage")
                                                (damage state side eid :brain 1 {:card card})
                                                (do (jack-out state side nil)
                                                    (swap! state update-in [:runner :prompt] rest)
                                                    (close-access-prompt state side)
                                                    (handle-end-run state side)
                                                    (gain-credits state :corp 5)
                                                    (draw state :corp)
                                                    (system-msg state :corp (str "gains 5 [Credits] and draws 1 card. Black Level Clearance is trashed"))
                                                    (trash state side card)
                                                    (effect-completed state side eid))))}
                                card nil))}}}

   "Breaker Bay Grid"
   {:events {:pre-rez-cost {:req (req (in-same-server? card target))
                            :effect (effect (rez-cost-bonus -5))}}}

   "Bryan Stinson"
   {:abilities [{:cost [:click 1]
                 :req (req (and (< (:credit runner) 6)
                                (pos? (count (filter #(and (operation? %)
                                                           (has-subtype? % "Transaction")) (:discard corp))))))
                 :label "Play a transaction operation from Archives, ignoring all costs, and remove it from the game"
                 :prompt "Choose a transaction operation to play"
                 :msg (msg "play " (:title target) " from Archives, ignoring all costs, and removes it from the game")
                 :choices (req (cancellable (filter #(and (operation? %)
                                                          (has-subtype? % "Transaction")) (:discard corp)) :sorted))
                 :effect (effect (play-instant nil (assoc-in target [:special :rfg-when-trashed] true) {:ignore-cost true})
                                 (move target :rfg))}]}

   "Calibration Testing"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:label "Place 1 advancement token on a card in this server"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Select a card in this server"
                                    :choices {:req #(in-same-server? % card)}
                                    :async true
                                    :msg (msg "place an advancement token on " (card-str state target))
                                    :cost [:trash]
                                    :effect (effect (add-prop target :advance-counter 1 {:placed true}))}
                                   card nil))}]}

   "Caprice Nisei"
   (let [ability {:req (req (and this-server
                                 (zero? (:position run))))
                  :psi {:not-equal {:msg "end the run"
                                    :effect (effect (end-run eid card))}}}]
     {:events {:approach-server ability}
      :abilities [ability]})

   "ChiLo City Grid"
   {:events {:successful-trace {:req (req this-server)
                                :async true
                                :effect (effect (gain-tags :corp eid 1))
                                :msg "give the Runner 1 tag"}}}

   "Code Replicator"
   {:abilities [{:label "Force the runner to approach the passed piece of ice again"
                 :req (req (and this-server
                                (> (count (get-run-ices state)) (:position run))
                                (:rezzed (get-in (:ices (card->server state card)) [(:position run)]))))
                 :effect (req (let [icename (:title (get-in (:ices (card->server state card)) [(:position run)]))]
                                (trash state :corp (get-card state card))
                                (swap! state update-in [:run] #(assoc % :position (inc (:position run))))
                                (system-msg state :corp (str "trashes Code Replicator to make the runner approach "
                                                             icename " again"))))}]}

   "Cold Site Server"
   (counter-based-extra-cost
    [:credit 1 :click 1]
    {:events {:corp-turn-begins {:req (req (pos? (get-counters card :power)))
                                 :msg " uses Cold Site Server to remove all hosted power counters"
                                 :effect (effect (add-counter card :power (- (get-counters card :power))))}}
     :abilities [{:cost [:click 1]
                  :msg "place 1 power counter on Cold Site Server"
                  :effect (effect (add-counter card :power 1))}]})

   "Corporate Troubleshooter"
   {:abilities [{:label "Add strength to a rezzed ICE protecting this server" :choices :credit
                 :prompt "How many credits?"
                 :effect (req (let [boost target]
                                (resolve-ability
                                  state side
                                  {:choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "add " boost " strength to " (:title target))
                                   :effect (req (update! state side (assoc card
                                                                           :troubleshooter-target target
                                                                           :troubleshooter-amount boost))
                                                (trash state side (get-card state card))
                                                (update-ice-strength state side target))} card nil)))}]
    :events {:pre-ice-strength nil
             :runner-turn-ends nil
             :corp-turn-ends nil}
    :trash-effect
    {:effect (req (register-events
                    state side
                    (let [ct {:effect (req (unregister-events state side card)
                                           (update! state side (dissoc card :troubleshooter-target))
                                           (update-ice-strength state side (:troubleshooter-target card)))}]
                      {:pre-ice-strength
                       {:req (req (same-card? target (:troubleshooter-target card)))
                        :effect (effect (ice-strength-bonus (:troubleshooter-amount card) target))}
                       :runner-turn-ends ct
                       :corp-turn-ends ct})
                    card))}}

   "Crisium Grid"
   (let [suppress-event {:req (req (and this-server (not (same-card? target card))))}]
     {:suppress {:pre-successful-run suppress-event
                 :successful-run suppress-event}
      :events {:pre-successful-run
               {:silent (req true)
                :req (req this-server)
                :effect (req (swap! state update-in [:run :run-effect] dissoc :replace-access)
                             (swap! state update-in [:run] dissoc :successful)
                             (swap! state update-in [:runner :register :successful-run] #(next %)))}}})

   "Cyberdex Virus Suite"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (effect (show-wait-prompt :runner "Corp to use Cyberdex Virus Suite")
                             (continue-ability
                               {:optional {:prompt "Purge virus counters with Cyberdex Virus Suite?"
                                           :yes-ability {:msg (msg "purge virus counters")
                                                         :effect (effect (clear-wait-prompt :runner)
                                                                         (purge))}
                                           :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil))}
    :abilities [{:label "Purge virus counters"
                 :msg "purge virus counters" :effect (effect (trash card) (purge))}]}

   "Daruma"
   (letfn [(choose-swap [to-swap]
             {:prompt (str "Select a card to swap with " (:title to-swap))
              :choices {:not-self true
                        :req #(and (corp? %)
                                   (#{"Asset" "Agenda" "Upgrade"} (:type %))
                                   (or (in-hand? %) ; agenda, asset or upgrade from HQ
                                       (and (installed? %) ; card installed in a server
                                            ;; central upgrades are not in a server
                                            (not (#{:hq :rd :archives} (first (:zone %)))))))}
              :effect (req (wait-for (trash state :corp card nil)
                                     (move state :corp to-swap (:zone target) {:keep-server-alive true})
                                     (move state :corp target (:zone to-swap) {:keep-server-alive true})
                                     (system-msg state :corp
                                                 (str "uses Daruma to swap " (card-str state to-swap)
                                                      " with " (card-str state target)))
                                     (clear-wait-prompt state :runner)))
              :cancel-effect (effect (clear-wait-prompt :runner))})
           (ability [card]
             {:optional {:prompt "Trash Daruma to swap a card in this server?"
                         :yes-ability {:async true
                                       :prompt "Select a card in this server to swap"
                                       :choices {:req #(and (installed? %)
                                                            (in-same-server? card %))
                                                 :not-self true}
                                       :effect (effect (continue-ability (choose-swap target) card nil))}
                         :no-ability {:effect (effect (clear-wait-prompt :runner))}}})]
     {:events {:approach-server {:async true
                                 :effect (effect (show-wait-prompt :runner "Corp to use Daruma")
                                                 (continue-ability :corp (ability card) card nil))}}})

   "Dedicated Technician Team"
   {:recurring 2
    :interactions {:pay-credits {:req (req (and (= :corp-install (:source-type eid))
                                                (= (second (:zone card))
                                                   (second (server->zone state (:source eid))))))
                                 :type :recurring}}}

   "Defense Construct"
   {:advanceable :always
    :abilities [{:label "Add 1 facedown card from Archives to HQ for each advancement token"
                 :req (req (and run
                                (= (:server run) [:archives])
                                (pos? (get-counters card :advancement))))
                 :effect (effect (resolve-ability
                                   {:show-discard true
                                    :choices {:max (get-counters card :advancement)
                                              :req #(and (corp? %)
                                                         (not (:seen %))
                                                         (in-discard? %))}
                                    :msg (msg "add " (count targets) " facedown cards in Archives to HQ")
                                    :effect (req (doseq [c targets]
                                                   (move state side c :hand)))}
                                   card nil)
                                 (trash card))}]}

   "Disposable HQ"
   (letfn [(dhq [n i]
             {:req (req (pos? i))
              :prompt "Select a card in HQ to add to the bottom of R&D"
              :choices {:req #(and (corp? %)
                                   (in-hand? %))}
              :async true
              :msg "add a card to the bottom of R&D"
              :effect (req (move state side target :deck)
                           (if (< n i)
                             (continue-ability state side (dhq (inc n) i) card nil)
                             (do
                               (clear-wait-prompt state :runner)
                               (effect-completed state side eid))))
              :cancel-effect (effect (clear-wait-prompt :runner))})]
     {:flags {:rd-reveal (req true)}
      :access {:async true
               :effect (req (let [n (count (:hand corp))]
                              (show-wait-prompt state :runner "Corp to finish using Disposable HQ")
                              (continue-ability
                                state side
                                {:optional
                                 {:prompt "Use Disposable HQ to add cards to the bottom of R&D?"
                                  :yes-ability {:async true
                                                :msg "add cards in HQ to the bottom of R&D"
                                                :effect (effect (continue-ability (dhq 1 n) card nil))}
                                  :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                                card nil)))}})

   "Drone Screen"
   {:events {:run {:req (req (and this-server tagged))
                   :async true
                   :trace {:base 3
                           :successful
                           {:msg "do 1 meat damage"
                            :effect (effect (damage eid :meat 1 {:card card
                                                                 :unpreventable true}))}}}}}

   "Embolus"
   (let [maybe-gain-counter {:once :per-turn
                             :label "Place a power counter on Embolus"
                             :effect (effect
                                       (continue-ability
                                         {:optional
                                          {:prompt "Pay 1 [Credit] to place a power counter on Embolus?"
                                           :yes-ability {:effect (effect (add-counter card :power 1))
                                                         :cost [:credit 1]
                                                         :msg "pay 1 [Credit] to place a power counter on Embolus"}}}
                                         card nil))}
         etr {:req (req this-server)
              :counter-cost [:power 1]
              :msg "end the run"
              :effect (effect (end-run eid card))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins maybe-gain-counter
               :successful-run {:req (req (pos? (get-counters card :power)))
                                :msg "remove 1 power counter from Embolus"
                                :effect (effect (add-counter card :power -1))}}
      :abilities [maybe-gain-counter etr]})

   "Experiential Data"
   {:effect (req (update-ice-in-server state side (card->server state card)))
    :events {:pre-ice-strength {:req (req (protecting-same-server? card target))
                                :effect (effect (ice-strength-bonus 1 target))}}
    :derez-effect {:effect (req (update-ice-in-server state side (card->server state card)))}
    :trash-effect {:effect (req (update-all-ice state side))}}

   "Expo Grid"
   (let [ability {:req (req (some #(and (asset? %)
                                        (rezzed? %))
                                  (get-in corp (:zone card))))
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :label "Gain 1 [Credits] (start of turn)"
                  :effect (effect (gain-credits 1))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Forced Connection"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 3
                     :successful {:msg "give the Runner 2 tags"
                                  :async true
                                  :effect (effect (gain-tags :corp eid 2))}}}}

   "Fractal Threat Matrix"
   {:implementation "Manual trigger each time all subs are broken"
    :abilities [{:label "Trash the top 2 cards from the Stack"
                 :msg (msg (let [deck (:deck runner)]
                             (if (pos? (count deck))
                               (str "trash " (join ", " (map :title (take 2 deck))) " from the Stack")
                               "trash the top 2 cards from their Stack - but the Stack is empty")))
                 :effect (effect (mill :corp :runner 2))}]}

   "Georgia Emelyov"
   {:events {:unsuccessful-run {:req (req (= (first (:server target))
                                             (second (:zone card))))
                                :async true
                                :msg "do 1 net damage"
                                :effect (effect (damage eid :net 1 {:card card}))}}
    :abilities [{:cost [:credit 2]
                 :label "Move to another server"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Choose a server"
                                    :choices (server-list state)
                                    :msg (msg "move to " target)
                                    :effect (req (let [c (move state side card
                                                               (conj (server->zone state target) :content))]
                                                   (unregister-events state side card)
                                                   (register-events state side (:events (card-def c)) c)))}
                                   card nil))}]}

   "Giordano Memorial Field"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req this-server)
      :msg "force the Runner to pay or end the run"
      :effect (req (let [credits (:credit runner)
                         cost (* 2 (count (:scored runner)))
                         pay-str (str "pay " cost " [Credits]")
                         c-pay-str (capitalize pay-str)]
                     (show-wait-prompt state :corp (str "Runner to " pay-str " or end the run"))
                     (continue-ability
                       state :runner
                       {:player :runner
                        :async true
                        :prompt (msg "You must " pay-str " or end the run")
                        :choices (concat (when (>= credits cost)
                                           [c-pay-str])
                                         ["End the run"])
                        :effect (req (clear-wait-prompt state :corp)
                                     (if (= c-pay-str target)
                                       (do (pay state :runner card :credit cost)
                                           (system-msg state :runner (str "pays " cost " [Credits]")))
                                       (do (end-run state side eid card)
                                           (system-msg state :corp "ends the run")))
                                     (effect-completed state side eid))}
                       card nil)))}}}

   "Heinlein Grid"
   {:abilities [{:req (req this-server)
                 :label "Force the Runner to lose all [Credits] from spending or losing a [Click]"
                 :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
                 :once :per-run
                 :effect (effect (lose-credits :runner :all)
                                 (lose :runner :run-credit :all))}]}

   "Helheim Servers"
   {:abilities [{:label "All ice protecting this server has +2 strength until the end of the run"
                 :req (req (and this-server
                                (pos? (count run-ices))
                                (pos? (count (:hand corp)))))
                 :async true
                 :cost [:trash-from-hand 1]
                 :effect (req (register-events
                                state side
                                {:pre-ice-strength {:req (req (= (card->server state card)
                                                                 (card->server state target)))
                                                    :effect (effect (ice-strength-bonus 2 target))}
                                 :run-ends {:effect (effect (unregister-events card))}}
                                card)
                              (continue-ability
                                state side
                                {:effect (req (update-ice-in-server
                                                state side (card->server state card)))}
                                card nil))}]
    :events {:pre-ice-strength nil}}

   "Henry Phillips"
   {:implementation "Manually triggered by Corp"
    :abilities [{:req (req (and this-server tagged))
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain-credits 2))}]}

   "Hired Help"
   (let [prompt-to-trash-agenda-or-etr
         {:prompt "Choose one"
          :player :runner
          :choices ["Trash 1 scored agenda" "End the run"]
          :effect (req (if (= target "End the run")
                         (do (system-msg state :runner (str "declines to pay the additional cost from Hired Help"))
                             (end-run state side eid card))
                         (if (seq (:scored runner))
                           (continue-ability state :runner
                                             {:prompt "Choose an Agenda to trash"
                                              :async true
                                              :choices {:max 1
                                                        :req #(is-scored? state side %)}
                                              :effect (req (wait-for (trash state side target {:unpreventable true})
                                                                     (system-msg state :runner (str "trashes " (:title target)
                                                                                                    " as an additional cost to initiate a run"))
                                                                     (effect-completed state side eid)))}
                                             card nil)
                           (do (system-msg state :runner (str "wants to pay the additional cost from Hired Help but has no scored agenda to trash"))
                               (end-run state side eid card)))))}]
     {:events {:run {:req (req (and this-server
                                    (empty? (filter #(= :hq %) (:successful-run runner-reg)))))
                     :effect (req (continue-ability state :runner prompt-to-trash-agenda-or-etr card nil))}}})

   "Hokusai Grid"
   {:events {:successful-run {:req (req this-server)
                              :msg "do 1 net damage"
                              :async true
                              :effect (effect (damage eid :net 1 {:card card}))}}}

   "Increased Drop Rates"
   {:flags {:rd-reveal (req true)}
    :access {:interactive (req true)
             :effect (effect (show-wait-prompt :corp "Runner to decide if they will take 1 tag")
                             (continue-ability
                               {:optional
                                {:player :runner
                                 :prompt "Take 1 tag to prevent Corp from removing 1 bad publicity?"
                                 :yes-ability {:async true
                                               :effect (req (gain-tags state :runner eid 1 {:unpreventable true})
                                                            (system-msg
                                                              state :runner
                                                              "takes 1 tag to prevent Corp from removing 1 bad publicity"))}
                                 :no-ability {:msg "remove 1 bad publicity"
                                              :effect (effect (lose-bad-publicity :corp 1))}
                                 :end-effect (effect (clear-wait-prompt :corp)
                                                     (effect-completed eid))}}
                               card nil))}}

   "Intake"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 4
                     :label "add an installed program or virtual resource to the Grip"
                     :successful
                     {:async true
                      :effect (effect (show-wait-prompt :runner "Corp to resolve Intake")
                                      (continue-ability
                                        {:prompt "Select a program or virtual resource"
                                         :player :corp
                                         :choices {:req #(and (installed? %)
                                                              (or (program? %)
                                                                  (and (resource? %)
                                                                       (has-subtype? % "Virtual"))))}
                                         :msg (msg "move " (:title target) " to the Grip")
                                         :effect (effect (move :runner target :hand))
                                         :end-effect (req (clear-wait-prompt state :runner)
                                                          (effect-completed state side eid))}
                                        card nil))}}}}

   "Jinja City Grid"
   (letfn [(install-ice [ice ices grids server]
             (let [remaining (remove-once #(same-card? % ice) ices)]
               {:async true
                :effect (req (if (= "None" server)
                               (continue-ability state side (choose-ice remaining grids) card nil)
                               (do (reveal state side ice)
                                   (system-msg state side (str "reveals that they drew " (:title ice)))
                                   (wait-for (corp-install state side ice server {:extra-cost [:credit -4]})
                                             (if (= 1 (count ices))
                                               (effect-completed state side eid)
                                               (continue-ability state side (choose-ice remaining grids)
                                                                 card nil))))))}))
           (choose-grid [ice ices grids]
             (if (= 1 (count grids))
               (install-ice ice ices grids (-> (first grids) :zone second zone->name))
               {:async true
                :prompt (str "Choose a server to install " (:title ice))
                :choices (conj (mapv #(-> % :zone second zone->name) grids) "None")
                :effect (effect (continue-ability (install-ice ice ices grids target) card nil))}))
           (choose-ice [ices grids]
             (when (seq ices)
               {:async true
                :prompt "Choose an ice to reveal and install, or None to decline"
                :choices (conj (mapv :title ices) "None")
                :effect (req (if (= "None" target)
                               (effect-completed state side eid)
                               (continue-ability state side
                                                 (choose-grid (some #(when (= target (:title %)) %) ices)
                                                              ices grids)
                                                 card nil)))}))]
     {:events {:corp-draw {;; THIS IS A HACK: it prevents multiple Jinja from showing the "choose a server to install into" sequence
                           :once :per-turn
                           :once-key :jinja-city-grid-draw
                           :async true
                           :effect (req (cond
                                          ;; If ice were drawn, do the full routine.
                                          (some ice? (:most-recent-drawn corp-reg))
                                          (let [ices (filter #(and (ice? %)
                                                                   (get-card state %))
                                                             (:most-recent-drawn corp-reg))
                                                grids (filterv #(= "Jinja City Grid" (:title %))
                                                               (all-active-installed state :corp))]
                                            (when (= :runner (:active-player @state))
                                              (show-wait-prompt state :runner "Corp to resolve Jinja City Grid"))
                                            (if (not-empty ices)
                                              (continue-ability state side (choose-ice ices grids) card nil)
                                              (effect-completed state side eid)))
                                          ;; else, if it's the runner's turn, show a fake prompt so the runner can't infer that ice weren't drawn
                                          (= :runner (:active-player @state))
                                          (continue-ability
                                            state :corp
                                            {:prompt "You did not draw any ice to use with Jinja City Grid"
                                             :choices ["Carry on!"]
                                             :prompt-type :bogus
                                             :effect nil}
                                            card nil)
                                          ;; otherwise, we done
                                          :else
                                          (effect-completed state side eid)))}
               :post-corp-draw {:effect (req (swap! state dissoc-in [:per-turn :jinja-city-grid-draw])
                                             (when (= :runner (:active-player @state))
                                               (clear-wait-prompt state :runner)))}}})

   "K. P. Lynn"
   (let [abi {:prompt "Choose one"
              :player :runner
              :choices ["Take 1 tag" "End the run"]
              :effect (req (if (= target "Take 1 tag")
                             (do (gain-tags state :runner 1)
                                 (system-msg state :corp (str "uses K. P. Lynn. Runner chooses to take 1 tag")))
                             (do (end-run state side eid card)
                                 (system-msg state :corp (str "uses K. P. Lynn. Runner chooses to end the run")))))}]
     {:events {:pass-ice {:req (req (and this-server (= (:position run) 1))) ; trigger when last ice passed
                          :async true
                          :effect (req (continue-ability state :runner abi card nil))}
               :run {:req (req (and this-server
                                    (zero? (:position run)))) ; trigger on unprotected server
                     :async true
                     :effect (req (continue-ability state :runner abi card nil))}}})

   "Letheia Nisei"
   (let [ability {:label "Force runner to re-approach outer ice"
                  :once :per-turn
                  :req (req (and this-server
                                 (zero? (:position run))))
                  :psi {:not-equal
                        {:effect
                         (effect
                           (show-wait-prompt :runner "Corp to use Letheia Nisei")
                           (continue-ability
                             :corp
                             {:optional
                              {:prompt "Trash to force re-approach outer ice?"
                               :autoresolve (get-autoresolve :auto-fire)
                               :yes-ability
                               {:msg "force the Runner to approach outermost piece of ice"
                                :cost [:trash]
                                :effect (req (swap! state assoc-in [:run :position] (count run-ices)))}
                               :end-effect (effect (clear-wait-prompt :runner))}}
                             card nil))}}}]
     {:events {:approach-server ability}
      :abilities [ability
                  (set-autoresolve :auto-fire "Fire Letheia Nisei?")]})

   "Keegan Lane"
   {:abilities [{:req (req (and this-server
                                (some? (first (filter program? (all-active-installed state :runner))))))
                 :prompt "Select a program to trash"
                 :label "Trash a program"
                 :msg (msg "trash " (:title target))
                 :choices {:req #(and (installed? %)
                                      (program? %))}
                 :cost [:trash :tag 1]
                 :effect (effect (trash eid target nil))}]}



   "Khondi Plaza"
   {:recurring (effect (set-prop card :rec-counter (count (get-remotes state))))
    :effect (effect (set-prop card :rec-counter (count (get-remotes state))))
    :interactions {:pay-credits {:req (req (and (= :rez (:source-type eid))
                                                (ice? target)
                                                (= (card->server state card) (card->server state target))))
                                 :type :recurring}}}

   "Manta Grid"
   {:events {:successful-run-ends
             {:msg "gain a [Click] next turn"
              :req (req (and (= (first (:server target)) (second (:zone card)))
                             (or (< (:credit runner) 6) (zero? (:click runner)))))
              :effect (req (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}}}

   "Marcus Batty"
   {:abilities [{:req (req this-server)
                 :label "Start a Psi game"
                 :psi {:not-equal {:prompt "Select a rezzed piece of ICE to resolve one of its subroutines"
                                   :choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "resolve a subroutine on " (:title target))}}
                 :cost [:trash]}]}

   "Mason Bellamy"
   {:implementation "Manually triggered by Corp"
    :abilities [{:label "Force the Runner to lose [Click] after an encounter where they broke a subroutine"
                 :req (req this-server)
                 :msg "force the Runner to lose [Click]"
                 :effect (effect (lose :runner :click 1))}]}

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
                     (resolve-ability
                       state side
                       {:effect (req (let [newice (assoc hqice :zone (:zone c))
                                           cndx (ice-index state c)
                                           ices (get-in @state (cons :corp (:zone c)))
                                           newices (apply conj (subvec ices 0 cndx) newice (subvec ices cndx))]
                                       (swap! state assoc-in (cons :corp (:zone c)) newices)
                                       (swap! state update-in [:corp :hand]
                                              (fn [coll] (remove-once #(same-card? % hqice) coll)))
                                       (trigger-event state side :corp-install newice)
                                       (move state side c :hand)))} card nil)))}]}

   "Mumbad City Grid"
   {:abilities [{:req (req (let [num-ice (count run-ices)]
                             (and this-server
                                  (>= num-ice 2)
                                  (< (:position run 0) num-ice))))
                 :label "Swap the ICE just passed with another piece of ICE protecting this server"
                 :effect (req (let [passed-ice (nth (get-in @state (vec (concat [:corp :servers] (:server run) [:ices])))
                                                    (:position run))
                                    ice-zone (:zone passed-ice)]
                                (resolve-ability
                                  state :corp
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
                                                  (update-ice-strength state side snew)
                                                  (system-msg state side (str "uses Mumbad City Grid to swap "
                                                                              (card-str state passed-ice)
                                                                              " with " (card-str state target)))))}
                                  card nil)))}]}

   "Mumbad Virtual Tour"
   {:flags {:must-trash (req (when installed true))}}

   "Mwanza City Grid"
   (let [gain-creds-and-clear {:req (req (= (:from-server target)
                                            (second (:zone card))))
                               :silent (req true)
                               :effect (req (let [cnt (total-cards-accessed run)
                                                  total (* 2 cnt)]
                                              (when cnt
                                                (gain-credits state :corp total)
                                                (system-msg state :corp
                                                            (str "gains " total " [Credits] from Mwanza City Grid")))))}
         boost-access-by-3 {:req (req (= target (second (:zone card))))
                            :msg "force the Runner to access 3 additional cards"
                            :effect (req (access-bonus state :runner (-> card :zone second) 3))}]
     {:install-req (req (filter #{"HQ" "R&D"} targets))
      :events {:pre-access boost-access-by-3
               :end-access-phase gain-creds-and-clear}
      ;; TODO: as written, this may fail if mwanza is trashed outside of a run on its server
      ;; (e.g. mwanza on R&D, run HQ, use polop to trash mwanza mid-run, shiro fires to cause RD
      :trash-effect                     ; if there is a run, mark mwanza effects to remain active until the end of the run
      {:req (req (:run @state))
       :effect (effect (register-events {:pre-access (assoc boost-access-by-3 :req (req (= target (second (:previous-zone card)))))
                                         :end-access-phase (assoc gain-creds-and-clear :req (req (= (:from-server target) (second (:previous-zone card)))))
                                         :unsuccessful-run-ends {:effect (effect (unregister-events card))}
                                         :successful-run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}})

   "NeoTokyo Grid"
   (let [ng {:req (req (in-same-server? card target))
             :once :per-turn
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits 1))}]
     {:events {:advance ng
               :advancement-placed ng}})

   "Nihongai Grid"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req (and this-server
                     (or (< (:credit runner) 6)
                         (< (count (:hand runner)) 2))
                     (not-empty (:hand corp))))
      :effect (req (show-wait-prompt state :runner "Corp to use Nihongai Grid")
                   (let [top5 (take 5 (:deck corp))]
                     (if (pos? (count top5))
                       (continue-ability
                         state side
                         {:optional
                          {:prompt "Use Nihongai Grid to look at top 5 cards of R&D and swap one with a card from HQ?"
                           :yes-ability
                           {:async true
                            :prompt "Choose a card to swap with a card from HQ"
                            :choices top5
                            :effect (req (let [rdc target]
                                           (continue-ability
                                             state side
                                             {:async true
                                              :prompt (msg "Choose a card in HQ to swap for " (:title rdc))
                                              :choices {:req in-hand?}
                                              :msg "swap a card from the top 5 of R&D with a card in HQ"
                                              :effect (req (let [hqc target
                                                                 newrdc (assoc hqc :zone [:deck])
                                                                 deck (vec (get-in @state [:corp :deck]))
                                                                 rdcndx (first (keep-indexed #(when (same-card? %2 rdc) %1) deck))
                                                                 newdeck (seq (apply conj (subvec deck 0 rdcndx) target (subvec deck rdcndx)))]
                                                             (swap! state assoc-in [:corp :deck] newdeck)
                                                             (swap! state update-in [:corp :hand]
                                                                    (fn [coll] (remove-once #(same-card? % hqc) coll)))
                                                             (move state side rdc :hand)
                                                             (clear-wait-prompt state :runner)
                                                             (effect-completed state side eid)))}
                                             card nil)))}
                           :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                     (effect-completed state side eid))}}}
                         card nil)
                       (do (clear-wait-prompt state :runner)
                           (effect-completed state side eid)))))}}}

   "Oaktown Grid"
   {:events {:pre-trash {:req (req (in-same-server? card target))
                         :effect (effect (trash-cost-bonus 3))}}}

   "Oberth Protocol"
   {:additional-cost [:forfeit]
    :events {:advance {:req (req (and (same-server? card target)
                                      (= 1 (count (filter #(= (second (:zone %)) (second (:zone card)))
                                                          (map first (turn-events state side :advance)))))))
                       :msg (msg "place an additional advancement token on " (card-str state target))
                       :effect (effect (add-prop :corp target :advance-counter 1 {:placed true}))}}}

   "Off the Grid"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :effect (req (prevent-run-on-server state card (second (:zone card))))
    :events {:runner-turn-begins {:effect (req (prevent-run-on-server state card (second (:zone card))))}
             :successful-run {:req (req (= target :hq))
                              :effect (req (trash state :corp card)
                                           (enable-run-on-server state card
                                                                 (second (:zone card)))
                                           (system-msg state :corp (str "trashes Off the Grid")))}}
    :leave-play (req (enable-run-on-server state card (second (:zone card))))}

   "Old Hollywood Grid"
   (let [ohg {:req (req (or (in-same-server? card target)
                            (from-same-server? card target)))
              :effect (req (register-persistent-flag!
                             state side
                             card :can-steal
                             (fn [state _ card]
                               (if-not (some #(= (:title %) (:title card)) (:scored runner))
                                 ((constantly false)
                                  (toast state :runner "Cannot steal due to Old Hollywood Grid." "warning"))
                                 true))))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (req (register-events
                      state side
                      {:pre-steal-cost (assoc ohg :req (req (or (= (:zone (get-nested-host target))
                                                                   (:previous-zone card))
                                                                (= (central->zone (:zone target))
                                                                   (butlast (:previous-zone card))))))
                       :run-ends {:effect (req (unregister-events state side (find-latest state card))
                                               (clear-persistent-flag! state side card :can-steal))}}
                      (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ohg
               :access {:effect (req (clear-persistent-flag! state side card :can-steal))}
               :run-ends nil}})

   "Overseer Matrix"
   (let [om {:req (req (in-same-server? card target))
             :async true
             :effect (effect (show-wait-prompt :runner "Corp to use Overseer Matrix")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 1 [Credits] to use Overseer Matrix ability?"
                                 :player :corp
                                 :end-effect (effect (clear-wait-prompt :runner)
                                                     (effect-completed eid))
                                 :yes-ability {:cost [:credit 1]
                                               :msg "give the Runner 1 tag"
                                               :async true
                                               :effect (req (gain-tags state :corp eid 1))}}}
                               card nil))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card)))
                      (:run @state)))
       :effect (effect (register-events {:runner-trash (assoc om :req (req (or (= (:zone (get-nested-host target))
                                                                                  (:previous-zone card))
                                                                               (= (central->zone (:zone target))
                                                                                  (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:run-ends nil
               :runner-trash om}})

   "Panic Button"
   {:init {:root "HQ"}
    :install-req (req (filter #{"HQ"} targets))
    :abilities [{:cost [:credit 1] :label "Draw 1 card" :effect (effect (draw))
                 :req (req (and run (= (first (:server run)) :hq)))}]}

   "Port Anson Grid"
   {:msg "prevent the Runner from jacking out unless they trash an installed program"
    :effect (req (when this-server
                   (prevent-jack-out state side)))
    :events {:run {:req (req this-server)
                   :msg "prevent the Runner from jacking out unless they trash an installed program"
                   :effect (effect (prevent-jack-out))}
             :runner-trash {:req (req (and this-server (program? target)))
                            :effect (req (swap! state update-in [:run] dissoc :cannot-jack-out))}}}

   "Prisec"
   {:access {:req (req (installed? card))
             :async true
             :effect (effect (show-wait-prompt :runner "Corp to use Prisec")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 2 [Credits] to use Prisec ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:cost [:credit 2]
                                               :msg "do 1 meat damage and give the Runner 1 tag"
                                               :async true
                                               :effect (req (wait-for (damage state side :meat 1 {:card card})
                                                                      (gain-tags state :corp eid 1)))}}}
                               card nil))}}

   "Product Placement"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain-credits :corp 2))}}

   "Red Herrings"
   (let [ab {:req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:credit 5]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends nil}})

   "Reduced Service"
   (counter-based-extra-cost
    [:credit 2]
    {:events {:successful-run {:req (req (and (pos? (get-counters card :power))
                                              (is-central? (:server run))))
                               :msg "remove a hosted power counter"
                               :effect (effect (add-counter card :power -1))}}
    :effect (req (show-wait-prompt state :runner "Corp to place credits on Reduced Service")
                 (continue-ability state side {:choices (req (map str (range (inc (min 4 (get-in @state [:corp :credit]))))))
                                               :prompt "How many credits to spend?"
                                               :effect (req (clear-wait-prompt state :runner)
                                                            (let [spent (str->int target)]
                                                              (deduct state :corp [:credit spent])
                                                              (add-counter state :corp card :power spent)
                                                              (system-msg state :corp (str "places " (quantify spent "power counter") " on Reduced Service"))
                                                              (effect-completed state side eid)))}
                                   card nil))})

   "Research Station"
   {:init {:root "HQ"}
    :install-req (req (filter #{"HQ"} targets))
    :in-play [:hand-size 2]}

   "Ruhr Valley"
   (letfn [(change-click-cost [n]
             (req (let [server (second (:zone card))]
                    (swap! state update-in [:corp :servers server :additional-cost]
                           #(merge-costs (concat % [:click n]))))))]
     {:effect (change-click-cost 1)
      :leave-play (change-click-cost -1)})

   "Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}

   "Ryon Knight"
   {:abilities [{:label "Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-server (zero? (:click runner))))
                 :async true
                 :effect (effect (trash card) (damage eid :brain 1 {:card card}))}]}

   "SanSan City Grid"
   {:effect (req (when-let [agenda (some #(when (agenda? %) %)
                                         (:content (card->server state card)))]
                   (update-advancement-cost state side agenda)))
    :events {:corp-install {:req (req (and (agenda? target)
                                           (in-same-server? card target)))
                            :effect (effect (update-advancement-cost target))}
             :pre-advancement-cost {:req (req (in-same-server? card target))
                                    :effect (effect (advancement-cost-bonus -1))}}}

   "Satellite Grid"
   {:effect (req (doseq [c (:ices (card->server state card))]
                   (set-prop state side c :extra-advance-counter 1))
                 (update-all-ice state side))
    :events {:corp-install {:req (req (and (ice? target)
                                           (protecting-same-server? card target)))
                            :effect (effect (set-prop target :extra-advance-counter 1))}}
    :leave-play (req (doseq [c (:ices (card->server state card))]
                       (update! state side (dissoc c :extra-advance-counter)))
                     (update-all-ice state side))}

   "Self-destruct"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:req (req this-server)
                 :label "Trace X - Do 3 net damage"
                 :effect (req (let [serv (card->server state card)
                                    cards (concat (:ices serv) (:content serv))]
                                (trash state side card)
                                (doseq [c cards]
                                  (trash state side c))
                                (resolve-ability
                                  state side
                                  {:trace {:base (req (dec (count cards)))
                                           :successful {:msg "do 3 net damage"
                                                        :effect (effect (damage eid :net 3 {:card card}))}}}
                                  card nil)))}]}

   "Shell Corporation"
   {:abilities
    [{:cost [:click 1]
      :msg "store 3 [Credits]"
      :once :per-turn
      :effect (effect (add-counter card :credit 3))}
     {:cost [:click 1]
      :msg (msg "gain " (get-counters card :credit) " [Credits]")
      :once :per-turn
      :label "Take all credits"
      :effect (effect (gain-credits (get-counters card :credit))
                      (set-prop card :counter {:credit 0}))}]}

   "Signal Jamming"
   {:abilities [{:label "Cards cannot be installed until the end of the run"
                 :msg (msg "prevent cards being installed until the end of the run")
                 :req (req this-server)
                 :cost [:trash]}]
    :trash-effect {:effect (effect (register-run-flag! card :corp-lock-install (constantly true))
                                   (register-run-flag! card :runner-lock-install (constantly true))
                                   (toast :runner "Cannot install until the end of the run")
                                   (toast :corp "Cannot install until the end of the run"))}
    :events {:run-ends nil}}

   "Simone Diego"
   {:recurring 2
    :interactions {:pay-credits {:req (req (and (= :advance (:source-type eid))
                                                (same-server? card target)))
                                 :type :recurring}}}

   "Strongbox"
   (let [ab {:req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:click 1]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab
               :run-ends nil}})

   "Surat City Grid"
   {:events
    {:rez {:req (req (and (same-server? card target)
                          (not (and (upgrade? target)
                                    (is-central? (second (:zone target)))))
                          (not (same-card? target card))
                          (seq (filter #(and (not (rezzed? %))
                                             (not (agenda? %))) (all-installed state :corp)))))
           :effect (effect (resolve-ability
                             {:optional
                              {:prompt (msg "Rez another card with Surat City Grid?")
                               :yes-ability {:prompt "Select a card to rez"
                                             :choices {:req #(and (not (rezzed? %))
                                                                  (not (agenda? %)))}
                                             :msg (msg "rez " (:title target) ", lowering the rez cost by 2 [Credits]")
                                             :effect (effect (rez-cost-bonus -2)
                                                             (rez target))}}}
                             card nil))}}}

   "Tempus"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :effect (req (when (= (first (:zone card)) :deck)
                            (system-msg state :runner (str "accesses Tempus"))))
             :trace {:base 3
                     :successful
                     {:msg "make the Runner choose between losing [Click][Click] or suffering 1 brain damage"
                      :async true
                      :effect (req (let [tempus card]
                                     (if (< (:click runner) 2)
                                       (do
                                         (system-msg state side "suffers 1 brain damage")
                                         (damage state side eid :brain 1 {:card tempus}))
                                       (do
                                         (show-wait-prompt state :corp "Runner to resolve Tempus")
                                         (continue-ability
                                           state :runner
                                           {:prompt "Lose [Click][Click] or take 1 brain damage?"
                                            :player :runner
                                            :choices ["Lose [Click][Click]" "Take 1 brain damage"]
                                            :async true
                                            :effect
                                            (req (clear-wait-prompt state :corp)
                                                 (if (.startsWith target "Take")
                                                   (do
                                                     (system-msg state side (str "chooses to take 1 brain damage"))
                                                     (damage state side eid :brain 1 {:card tempus}))
                                                   (do
                                                     (system-msg state side "chooses to lose [Click][Click]")
                                                     (lose state :runner :click 2)
                                                     (effect-completed state side eid))))}
                                           card nil)))))}}}}

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
                                   :effect (req (reveal state side target)
                                                (trash state side (assoc target :seen true))
                                                (swap! state update-in [:run]
                                                       #(assoc % :position (inc (:position run)))))
                                   :msg (msg "trash a copy of " (:title target) " from HQ and force the Runner to encounter it again")}
                                  card nil)))}]}

   "Tori Hanzō"
   {:events
    {:pre-resolve-damage
     {:once :per-run
      :async true
      :req (req (and this-server
                     (= target :net)
                     (pos? (last targets))
                     (can-pay? state :corp eid card nil [:credit 2])))
      :effect (req (swap! state assoc-in [:damage :damage-replace] true)
                   (damage-defer state side :net (last targets))
                   (show-wait-prompt state :runner "Corp to use Tori Hanzō")
                   (continue-ability
                     state side
                     {:optional
                      {:prompt (str "Pay 2 [Credits] to do 1 brain damage with Tori Hanzō?")
                       :player :corp
                       :yes-ability {:async true
                                     :msg "do 1 brain damage instead of net damage"
                                     :effect (req (swap! state update-in [:damage] dissoc :damage-replace :defer-damage)
                                                  (clear-wait-prompt state :runner)
                                                  (pay state :corp card :credit 2)
                                                  (wait-for (damage state side :brain 1 {:card card})
                                                            (do (swap! state assoc-in [:damage :damage-replace] true)
                                                                (effect-completed state side eid))))}
                       :no-ability {:async true
                                    :effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                                 (clear-wait-prompt state :runner)
                                                 (effect-completed state side eid))}}}
                     card nil))}
     :prevented-damage {:req (req (and this-server
                                       (= target :net)
                                       (pos? (last targets))))
                        :effect (req (swap! state assoc-in [:per-run (:cid card)] true))}}}

   "Traffic Analyzer"
   {:events {:rez {:req (req (and (protecting-same-server? card target)
                                  (ice? target)))
                   :interactive (req true)
                   :trace {:base 2
                           :successful {:msg "gain 1 [Credits]"
                                        :effect (effect (gain-credits 1))}}}}}

   "Tyr's Hand"
   {:abilities [{:label "Prevent a subroutine on a piece of Bioroid ICE from being broken"
                 :req (req (and (= (butlast (:zone current-ice)) (butlast (:zone card)))
                                (has-subtype? current-ice "Bioroid")))
                 :effect (effect (trash card))
                 :msg (msg "prevent a subroutine on " (:title current-ice) " from being broken")}]}

   "Underway Grid"
   {:implementation "Bypass prevention is not implemented"
    :events {:pre-expose {:req (req (same-server? card target))
                          :msg "prevent 1 card from being exposed"
                          :effect (effect (expose-prevent 1))}}}

   "Valley Grid"
   {:implementation "Activation is manual"
    :abilities [{:req (req this-server)
                 :label "Reduce Runner's maximum hand size by 1 until start of next Corp turn"
                 :msg "reduce the Runner's maximum hand size by 1 until the start of the next Corp turn"
                 :effect (req (update! state side (assoc card :times-used (inc (get card :times-used 0))))
                              (change-hand-size state :runner -1))}]
    :trash-effect {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
                   :effect (req (when-let [n (:times-used card)]
                                  (register-events state side
                                                   {:corp-turn-begins
                                                    {:msg (msg "increase the Runner's maximum hand size by " n)
                                                     :effect (effect (change-hand-size :runner n)
                                                                     (unregister-events card)
                                                                     (update! (dissoc card :times-used)))}}
                                                   (assoc card :zone '(:discard)))))}
    :events {:corp-turn-begins {:req (req (:times-used card))
                                :msg (msg "increase the Runner's maximum hand size by "
                                          (:times-used card))
                                :effect (effect (change-hand-size :runner (:times-used card))
                                                (update! (dissoc card :times-used)))}}}

   "Warroid Tracker"
   (letfn [(wt [card n t]
             {:prompt "Choose an installed card to trash due to Warroid Tracker"
              :async true
              :player :runner
              :priority 2
              :choices {:req #(and (installed? %) (runner? %))}
              :effect (req (system-msg state side (str "trashes " (card-str state target) " due to Warroid Tracker"))
                           (trash state side target {:unpreventable true})
                           (if (> n t)
                             (continue-ability state side (wt card n (inc t)) card nil)
                             (do (clear-wait-prompt state :corp)
                                 (effect-completed state side eid)))
                           ;; this ends-the-run if WT is the only card and is trashed, and trashes at least one runner card
                           (when (zero? (count (cards-to-access state side (get-in @state [:run :server]))))
                             (handle-end-run state side)))})]
     {:implementation "Does not handle UFAQ interaction with Singularity"
      :events {:runner-trash
               {:async true
                :req (req (let [target-zone (:zone target)
                                target-zone (or (central->zone target-zone) target-zone)
                                warroid-zone (:zone card)]
                            (= (second warroid-zone)
                               (second target-zone))))
                :trace {:base 4
                        :successful
                        {:effect
                         (req (let [n (count (all-installed state :runner))
                                    n (if (> n 2) 2 n)]
                                (if (pos? n)
                                  (do (system-msg
                                        state side
                                        (str "uses Warroid Tracker to force the runner to trash "
                                             (quantify n " installed card")))
                                      (show-wait-prompt state :corp "Runner to choose cards to trash")
                                      (resolve-ability state side (wt card n 1) card nil))
                                  (system-msg
                                    state side
                                    "uses Warroid Tracker but there are no installed cards to trash"))))}}}}})

   "Will-o'-the-Wisp"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req (and this-server
                     (some #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
      :effect (effect (show-wait-prompt :runner "Corp to use Will-o'-the-Wisp")
                      (continue-ability {:optional
                                         {:prompt "Trash Will-o'-the-Wisp?"
                                          :choices {:req #(has-subtype? % "Icebreaker")}
                                          :yes-ability {:async true
                                                        :prompt "Choose an icebreaker used to break at least 1 subroutine during this run"
                                                        :choices {:req #(has-subtype? % "Icebreaker")}
                                                        :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
                                                        :effect (effect (trash card)
                                                                        (move :runner target :deck)
                                                                        (clear-wait-prompt :runner)
                                                                        (effect-completed eid))}
                                          :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                                       (effect-completed eid))}}}
                                        card nil))}}}})
