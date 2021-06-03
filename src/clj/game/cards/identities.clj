(ns game.cards.identities
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.string :as string]))

;;; Helper functions for Draft cards
(def draft-points-target
  "Set each side's agenda points target at 6, per draft format rules"
  (req (swap! state assoc-in [:runner :agenda-point-req] 6)
       (swap! state assoc-in [:corp :agenda-point-req] 6)))

(defn- has-most-faction?
  "Checks if the faction has a plurality of rezzed / installed cards"
  [state side fc]
  (let [card-list (all-active-installed state side)
        faction-freq (frequencies (map :faction card-list))
        reducer (fn [{:keys [max-count] :as acc} faction count]
                  (cond
                    ;; Has plurality update best-faction
                    (> count max-count)
                    {:max-count count :max-faction faction}
                    ;; Lost plurality
                    (= count max-count)
                    (dissoc acc :max-faction)
                    ;; Count is not more, do not change the accumulator map
                    :default
                    acc))
        best-faction (:max-faction (reduce-kv reducer {:max-count 0 :max-faction nil} faction-freq))]
    (= fc best-faction)))

;; Card definitions

(defcard "419: Amoral Scammer"
  {:events [{:event :corp-install
             :async true
             :req (req (and (first-event? state :corp :corp-install)
                            (pos? (:turn @state))
                            (not (rezzed? (:card context)))
                            (not (#{:rezzed-no-cost :rezzed-no-rez-cost :rezzed :face-up} (:install-state context)))))
             :waiting-prompt "Runner to choose an option"
             :effect
             (effect
               (continue-ability
                 {:optional
                  {:prompt "Expose installed card unless Corp pays 1 [Credits]?"
                   :player :runner
                   :autoresolve (get-autoresolve :auto-419)
                   :no-ability {:effect (req (clear-wait-prompt state :corp))}
                   :yes-ability
                   {:async true
                    :effect (req (if (not (can-pay? state :corp (assoc eid :source card :source-type :ability) card nil :credit 1))
                                   (do
                                     (toast state :corp "Cannot afford to pay 1 [Credit] to block card exposure" "info")
                                     (expose state :runner eid (:card context)))
                                   (continue-ability
                                     state side
                                     {:optional
                                      {:waiting-prompt "Corp to choose an option"
                                       :prompt "Pay 1 [Credits] to prevent exposure of installed card?"
                                       :player :corp
                                       :no-ability
                                       {:async true
                                        :effect (effect (expose :runner eid (:card context)))}
                                       :yes-ability
                                       {:async true
                                        :effect
                                        (req (wait-for
                                               (pay state :corp card [:credit 1])
                                               (system-msg state :corp
                                                           (str (:msg async-result)
                                                                " to prevent "
                                                                " card from being exposed"))
                                               (effect-completed state side eid)))}}}
                                     card targets)))}}}
                 card targets))}]
   :abilities [(set-autoresolve :auto-419 "419")]})

(defcard "Acme Consulting: The Truth You Need"
  (letfn [(outermost? [run-position run-ices]
            (and run-position
                 (pos? run-position)
                 (= run-position (count run-ices))))]
    {:constant-effects [{:type :tags
                         :req (req (and (rezzed? current-ice)
                                        (= :encounter-ice (:phase run))
                                        (outermost? run-position run-ices)))
                         :value 1}]}))

(defcard "Adam: Compulsive Hacker"
  {:events [{:event :pre-start-game
             :req (req (= side :runner))
             :async true
             :waiting-prompt "Runner to make a decision"
             :effect (req (let [directives (->> (server-cards)
                                                (filter #(has-subtype? % "Directive"))
                                                (map make-card)
                                                (map #(assoc % :zone [:play-area]))
                                                (into []))]
                            ;; Add directives to :play-area - assumed to be empty
                            (swap! state assoc-in [:runner :play-area] directives)
                            (continue-ability
                              state side
                              {:prompt (str "Choose 3 starting directives")
                               :choices {:max 3
                                         :all true
                                         :card #(and (runner? %)
                                                     (in-play-area? %))}
                               :effect (req (doseq [c targets]
                                              (runner-install
                                                state side
                                                (make-eid state eid) c
                                                {:ignore-all-cost true
                                                 :custom-message (fn [_] (str "starts with " (:title c) " in play"))}))
                                            (swap! state assoc-in [:runner :play-area] []))}
                              card nil)))}]})

(defcard "AgInfusion: New Miracles for a New World"
  {:abilities [{:label "Trash a piece of ice to choose another server- the runner is now running that server"
                :once :per-turn
                :async true
                :req (req (and run
                               (= :approach-ice (:phase run))
                               (not (rezzed? current-ice))
                               (can-rez? state side current-ice {:ignore-unique true})))
                :prompt "Choose another server and redirect the run to its outermost position"
                :choices (req (cancellable (remove #{(-> @state :run :server central->name)} servers)))
                :msg (msg "trash the approached piece of ice. The Runner is now running on " target)
                :effect (req (let [dest (server->zone state target)
                                   ice (count (get-in corp (conj dest :ices)))
                                   phase (if (pos? ice) :encounter-ice :approach-server)]
                               (when (zero? ice)
                                 (swap! state assoc-in [:run :jack-out] true))
                               (redirect-run state side target phase)
                               (start-next-phase state side nil)
                               (trash state side eid current-ice {:unpreventable true})))}]})

(defcard "Akiko Nisei: Head Case"
  {:events [{:event :pre-access
             :interactive (req true)
             :psi {:req (req (= target :rd))
                   :player :runner
                   :equal {:msg "access 1 additional card"
                           :effect (effect (access-bonus :rd 1)
                                           (effect-completed eid))}}}]})

(defcard "Alice Merchant: Clan Agitator"
  {:events [{:event :successful-run
             :interactive (req true)
             :req (req (and (= :archives (target-server context))
                            (first-successful-run-on-server? state :archives)
                            (not-empty (:hand corp))))
             :waiting-prompt "Corp to make a decision"
             :prompt "Choose a card in HQ to discard"
             :player :corp
             :choices {:all true
                       :card #(and (in-hand? %)
                                   (corp? %))}
             :msg "force the Corp to trash 1 card from HQ"
             :async true
             :effect (effect (trash :corp eid target nil))}]})

(defcard "Andromeda: Dispossessed Ristie"
  {:events [{:event :pre-start-game
             :req (req (= side :runner))
             :effect (effect (draw 4 {:suppress-event true}))}]
   :mulligan (effect (draw 4 {:suppress-event true}))})

(defcard "Apex: Invasive Predator"
  (let [ability {:prompt "Select a card to install facedown"
                 :label "Install a card facedown (start of turn)"
                 :once :per-turn
                 :choices {:max 1
                           :card #(and (runner? %)
                                       (in-hand? %))}
                 :req (req (and (pos? (count (:hand runner)))
                                (:runner-phase-12 @state)))
                 :async true
                 :msg "install a card facedown"
                 :effect (effect
                           (runner-install
                             (assoc eid :source card :source-type :runner-install)
                             target
                             {:facedown true :no-msg true}))}]
    {:implementation "Install restriction not enforced"
     :events [(assoc ability :event :runner-turn-begins)]
     :flags {:runner-phase-12 (req true)}
     :abilities [ability]}))

(defcard "Argus Security: Protection Guaranteed"
  {:events [{:event :agenda-stolen
             :prompt "Take 1 tag or suffer 2 meat damage?"
             :async true
             :choices ["1 tag" "2 meat damage"]
             :player :runner
             :msg "make the Runner take 1 tag or suffer 2 meat damage"
             :effect (req (if (= target "1 tag")
                            (do (system-msg state side "chooses to take 1 tag")
                                (gain-tags state :runner eid 1))
                            (do (system-msg state side "chooses to suffer 2 meat damage")
                                (damage state :runner eid :meat 2 {:unboostable true :card card}))))}]})

(defcard "Armand \"Geist\" Walker: Tech Lord"
  {:events [{:event :runner-trash
             :async true
             :interactive (req true)
             :req (req (and (= side :runner) (= :ability-cost (:cause target))))
             :msg "draw a card"
             :effect (effect (draw eid 1 nil))}]})

(defcard "Asa Group: Security Through Vigilance"
  {:events [{:event :corp-install
             :async true
             :req (req (first-event? state :corp :corp-install))
             :effect (req (let [installed-card (:card context)
                                z (butlast (get-zone installed-card))]
                            (continue-ability
                              state side
                              {:prompt (str "Select a "
                                            (if (is-remote? z)
                                              "non-agenda"
                                              "piece of ice")
                                            " in HQ to install")
                               :choices {:card #(and (in-hand? %)
                                                     (corp? %)
                                                     (corp-installable-type? %)
                                                     (not (agenda? %))
                                                     (or (is-remote? z)
                                                         (ice? %)))}
                               :async true
                               :effect (effect (corp-install eid target (zone->name z) nil))}
                              card nil)))}]})

(defcard "Ayla \"Bios\" Rahim: Simulant Specialist"
  {:abilities [{:label "Add 1 hosted card to your grip"
                :cost [:click 1]
                :async true
                :prompt "Choose a hosted card"
                :choices (req (cancellable (:hosted card)))
                :msg "move a hosted card to their Grip"
                :effect (effect (move target :hand)
                                (effect-completed eid))}]
   :events [{:event :pre-start-game
             :req (req (= side :runner))
             :async true
             :waiting-prompt "Runner to make a decision"
             :effect (req (doseq [c (take 6 (:deck runner))]
                            (move state side c :play-area))
                          (continue-ability
                            state side
                            {:prompt "Select 4 cards to be hosted"
                             :choices {:max 4
                                       :all true
                                       :card #(and (runner? %)
                                                   (in-play-area? %))}
                             :effect (req (doseq [c targets]
                                            (host state side (get-card state card) c {:facedown true}))
                                          (doseq [c (get-in @state [:runner :play-area])]
                                            (move state side c :deck))
                                          (shuffle! state side :deck))}
                            card nil))}]})

(defcard "Az McCaffrey: Mechanical Prodigy"
  ;; Effect marks Az's ability as "used" if it has already met it's trigger condition this turn
  (letfn [(az-type? [card] (or (hardware? card)
                               (and (resource? card)
                                    (or (has-subtype? card "Job")
                                        (has-subtype? card "Connection")))))
          (not-triggered? [state] (no-event? state :runner :runner-install #(az-type? (:card (first %)))))]
    {:constant-effects [{:type :install-cost
                         :req (req (and (az-type? target)
                                        (not-triggered? state)))
                         :value -1}]
     :events [{:event :runner-install
               :req (req (and (az-type? (:card context))
                              (not-triggered? state)))
               :silent (req true)
               :msg (msg "reduce the install cost of " (:title (:card context)) " by 1 [Credits]")}]}))

(defcard "Azmari EdTech: Shaping the Future"
  {:events [{:event :corp-turn-ends
             :prompt "Name a Runner card type"
             :choices ["Event" "Resource" "Program" "Hardware"]
             :effect (effect (update! (assoc card :az-target target))
                             (system-msg (str "uses Azmari EdTech: Shaping the Future to name " target)))}
            {:event :runner-install
             :req (req (and (is-type? (:card context) (:az-target card))
                            (not (:facedown context))))
             :async true
             :effect (effect (gain-credits :corp eid 2))
             :once :per-turn
             :msg (msg "gain 2 [Credits] from " (:az-target card))}
            {:event :play-event
             :req (req (is-type? (:card context) (:az-target card)))
             :async true
             :effect (effect (gain-credits :corp eid 2))
             :once :per-turn
             :msg (msg "gain 2 [Credits] from " (:az-target card))}]})

(defcard "Blue Sun: Powering the Future"
  {:flags {:corp-phase-12 (req (and (not (:disabled card))
                                    (some rezzed? (all-installed state :corp))))}
   :abilities [{:choices {:card rezzed?}
                :label "Add 1 rezzed card to HQ and gain credits equal to its rez cost"
                :msg (msg "add " (:title target) " to HQ and gain " (rez-cost state side target) " [Credits]")
                :async true
                :effect (effect (move target :hand)
                                (gain-credits eid (rez-cost state side target)))}]})

(defcard "Boris \"Syfr\" Kovac: Crafty Veteran"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-turn-begins
             :req (req (and (has-most-faction? state :runner "Criminal")
                            (pos? (get-in runner [:tag :base]))))
             :msg "remove 1 tag"
             :async true
             :effect (effect (lose-tags eid 1))}]})

(defcard "Cerebral Imaging: Infinite Frontiers"
  {:constant-effects [(corp-hand-size+ (req (:credit corp)))]
   :effect (req (swap! state assoc-in [:corp :hand-size :base] 0))
   :leave-play (req (swap! state assoc-in [:corp :hand-size :base] 5))})

(defcard "Chaos Theory: Wünderkind"
  {:constant-effects [(mu+ 1)]})

(defcard "Chronos Protocol: Selective Mind-mapping"
  {:req (req (empty? (filter #(= :net (first %)) (turn-events state :runner :damage))))
   :effect (effect (enable-corp-damage-choice))
   :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-corp))
   :events [{:event :corp-phase-12
             :effect (effect (enable-corp-damage-choice))}
            {:event :runner-phase-12
             :effect (effect (enable-corp-damage-choice))}
            {:event :pre-resolve-damage
             :optional
             {:player :corp
              :req (req (and (= target :net)
                             (corp-can-choose-damage? state)
                             (pos? (last targets))
                             (empty? (filter #(= :net (first %)) (turn-events state :runner :damage)))
                             (pos? (count (:hand runner)))))
              :waiting-prompt "Corp to make a decision"
              :prompt "Use Chronos Protocol to select the first card trashed?"
              :yes-ability
              {:async true
               :msg (msg "look at the Runner's Grip ( "
                         (string/join ", " (map :title (sort-by :title (:hand runner))))
                         " ) and select the card that is trashed")
               :effect
               (effect (continue-ability
                         {:prompt "Select a card to trash"
                          :choices (req (:hand runner))
                          :not-distinct true
                          :msg (msg "choose " (:title target) " to trash")
                          :effect (req (chosen-damage state :corp target))}
                         card nil))}
              :no-ability
              {:effect (req (system-msg state :corp "doesn't use Chronos Protocol to select the first card trashed"))}}}]})

(defcard "Cybernetics Division: Humanity Upgraded"
  {:constant-effects [(hand-size+ -1)]})

(defcard "Earth Station: SEA Headquarters"
  (let [flip-effect (effect (update! (if (:flipped card)
                                       (do (system-msg state :corp "flipped their identity to Earth Station: SEA Headquarters")
                                           (assoc card
                                                  :flipped false
                                                  :face :front
                                                  :code (subs (:code card) 0 5)))
                                       (assoc card
                                              :flipped true
                                              :face :back
                                              :code (str (subs (:code card) 0 5) "flip")))))]
    {:events [{:event :pre-first-turn
               :req (req (= side :corp))
               :effect (effect (update! (assoc card :flipped false :face :front)))}
              {:event :successful-run
               :req (req (and (= :hq (target-server context))
                              (:flipped card)))
               :effect flip-effect}]
     :constant-effects [{:type :run-additional-cost
                         :req (req (or
                                     (and (not (:flipped card))
                                          (= :hq (:server (second targets))))
                                     (and (:flipped card)
                                          (in-coll? (keys (get-remotes state)) (:server (second targets))))))
                         :value (req [:credit (if (:flipped card) 6 1)])}]
     :async true
     ; This effect will be resolved when the ID is reenabled after Strike / Direct Access
     :effect (effect
               (continue-ability
                 {:req (req (< 1 (count (get-remotes state))))
                  :prompt "Select a server to be saved from the rules apocalypse"
                  :choices (req (get-remote-names state))
                  :async true
                  :effect (req (let [to-be-trashed (remove #(in-coll? ["Archives" "R&D" "HQ" target] (zone->name (second (get-zone %))))
                                                           (all-installed state :corp))]
                                 (system-msg state side (str "chooses " target
                                                             " to be saved from the rules apocalypse and trashes "
                                                             (quantify (count to-be-trashed) "card")))
                                 ; these cards get trashed by the game and not by players
                                 (trash-cards state side eid to-be-trashed {:unpreventable true :game-trash true})))}
                 card nil))
     :abilities [{:label "Flip identity to Earth Station: Ascending to Orbit"
                  :req (req (not (:flipped card)))
                  :cost [:click 1]
                  :msg "flip their identity to Earth Station: Ascending to Orbit"
                  :effect flip-effect}
                 {:label "Manually flip identity to Earth Station: SEA Headquarters"
                  :req (req (:flipped card))
                  :effect flip-effect}]}))

(defcard "Edward Kim: Humanity's Hammer"
  {:events [{:event :access
             :once :per-turn
             :req (req (and (operation? target)
                            (first-event? state side :access #(operation? (first %)))))
             :async true
             :effect (req (if (in-discard? target)
                            (effect-completed state side eid)
                            (do (when run
                                  (swap! state assoc-in [:run :did-trash] true))
                                (swap! state assoc-in [:runner :register :trashed-card] true)
                                (system-msg state :runner
                                            (str "uses Edward Kim: Humanity's Hammer to"
                                                 " trash " (:title target)
                                                 " at no cost"))
                                (trash state side eid target nil))))}]})

(defcard "Ele \"Smoke\" Scovak: Cynosure of the Net"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Exile: Streethawk"
  {:flags {:runner-install-draw true}
   :events [{:event :runner-install
             :async true
             :req (req (and (program? (:card context))
                            (some #{:discard} (:previous-zone (:card context)))))
             :msg "draw a card"
             :effect (effect (draw eid 1 nil))}]})

(defcard "Freedom Khumalo: Crypto-Anarchist"
  {:interactions
   {:access-ability
    {:async true
     :once :per-turn
     :label "Trash card"
     :req (req (and (not (:disabled card))
                    (not (agenda? target))
                    (<= (play-cost state side target)
                        (number-of-runner-virus-counters state))))
     :waiting-prompt "Runner to make a decision"
     :effect (req (let [accessed-card target
                        play-or-rez (:cost target)]
                    (if (zero? play-or-rez)
                      (continue-ability
                        state side
                        {:async true
                         :msg (msg "trash " (:title accessed-card) " at no cost")
                         :effect (effect (trash eid (assoc accessed-card :seen true) {:accessed true}))}
                        card nil)
                      (wait-for (resolve-ability state side (pick-virus-counters-to-spend play-or-rez) card nil)
                                (if-let [msg (:msg async-result)]
                                  (do (system-msg state :runner
                                                  (str "uses Freedom Khumalo: Crypto-Anarchist to"
                                                       " trash " (:title accessed-card)
                                                       " at no cost, spending " msg))
                                      (trash state side eid (assoc accessed-card :seen true) {:accessed true}))
                                  ;; Player cancelled ability
                                  (do (swap! state dissoc-in [:per-turn (:cid card)])
                                      (access-non-agenda state side eid accessed-card :skip-trigger-event true)))))))}}})

(defcard "Fringe Applications: Tomorrow, Today"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-turn-begins
             :player :corp
             :req (req (and (not (:disabled card))
                            (has-most-faction? state :corp "Weyland Consortium")
                            (some ice? (all-installed state side))))
             :prompt "Select a piece of ice to place 1 advancement token on"
             :choices {:card #(and (installed? %)
                                   (ice? %))}
             :msg (msg "place 1 advancement token on " (card-str state target))
             :effect (req (add-prop state :corp target :advance-counter 1 {:placed true}))}]})

(defcard "Gabriel Santiago: Consummate Professional"
  {:events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            (first-successful-run-on-server? state :hq)))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits eid 2))}]})

(defcard "Gagarin Deep Space: Expanding the Horizon"
  {:events [{:event :pre-access-card
             :req (req (is-remote? (second (get-zone target))))
             :effect (effect (access-cost-bonus [:credit 1]))
             :msg "make the Runner spend 1 [Credits] to access"}]})

(defcard "GameNET: Where Dreams are Real"
  {:implementation "Credit gain not implemented. You can use shortcut ability."
   :abilities [{:msg "gain 1 [Credits] (shortcut)"
                :async true
                :effect (effect (gain-credits :corp eid 1))}]})

(defcard "GRNDL: Power Unleashed"
  {:events [{:event :pre-start-game
             :req (req (= :corp side))
             :async true
             :effect (req (wait-for (gain-credits state :corp 5)
                                    (if (zero? (count-bad-pub state))
                                      (gain-bad-publicity state :corp eid 1)
                                      (effect-completed state side eid))))}]})

(defcard "Haarpsichord Studios: Entertainment Unleashed"
  {:constant-effects [{:type :cannot-steal
                       :value (req (pos? (event-count state side :agenda-stolen)))}]
   :events [{:event :access
             :req (req (and (agenda? target)
                            (pos? (event-count state side :agenda-stolen))))
             :effect (effect (toast :runner "Cannot steal due to Haarpsichord Studios." "warning"))}]})

(defcard "Haas-Bioroid: Architects of Tomorrow"
  {:events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (has-subtype? (:ice context) "Bioroid")
                            (first-event? state :runner :pass-ice
                                          (fn [targets]
                                            (let [context (first targets)
                                                  ice  (:ice context)]
                                              (and (rezzed? ice)
                                                   (installed? ice)
                                                   (has-subtype? ice "Bioroid")))))))
             :waiting-prompt "Corp to make a decision"
             :prompt "Select a Bioroid to rez"
             :player :corp
             :choices
             {:req (req (and (has-subtype? target "Bioroid")
                             (not (rezzed? target))
                             (can-pay? state side (assoc eid :source card :source-type :rez) target nil
                                       [:credit (rez-cost state side target {:cost-bonus -4})])))}
             :msg (msg "rez " (:title target))
             :async true
             :effect (effect (rez eid target {:cost-bonus -4}))}]})

(defcard "Haas-Bioroid: Engineering the Future"
  {:events [{:event :corp-install
             :req (req (first-event? state corp :corp-install))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Haas-Bioroid: Precision Design"
  {:constant-effects [(corp-hand-size+ 1)]
   :events [{:event :agenda-scored
             :interactive (req true)
             :optional {:prompt "Add card from Archives to HQ?"
                        :autoresolve (get-autoresolve :auto-precision-design)
                        :yes-ability (corp-recur)}}]
   :abilities [(set-autoresolve :auto-precision-design "add card from Archives to HQ")]})

(defcard "Haas-Bioroid: Stronger Together"
  {:constant-effects [{:type :ice-strength
                       :req (req (has-subtype? target "Bioroid"))
                       :value 1}]
   :leave-play (effect (update-all-ice))
   :effect (effect (update-all-ice))})

(defcard "Harishchandra Ent.: Where You're the Star"
  {:events [{:event :tags-changed
             :effect (req (if (is-tagged? state)
                            (when-not (get-in @state [:runner :openhand])
                              (system-msg state :corp "reveals the Runner's hand")
                              (reveal-hand state :runner))
                            (when (get-in @state [:runner :openhand])
                              (system-msg state :corp "hides the Runner's hand")
                              (conceal-hand state :runner))))}]
   :effect (req (when (is-tagged? state)
                  (reveal-hand state :runner)))
   :leave-play (req (when (is-tagged? state)
                      (conceal-hand state :runner)))})

(defcard "Harmony Medtech: Biomedical Pioneer"
  {:effect (effect (lose :agenda-point-req 1)
                   (lose :runner :agenda-point-req 1))
   :leave-play (effect (gain :agenda-point-req 1)
                       (gain :runner :agenda-point-req 1))})

(defcard "Hayley Kaplan: Universal Scholar"
  {:events [{:event :runner-install
             :silent (req (not (and (first-event? state side :runner-install)
                                    (some #(is-type? % (:type (:card context))) (:hand runner)))))
             :req (req (and (first-event? state side :runner-install)
                            (not (:facedown context))))
             :once :per-turn
             :async true
             :waiting-prompt "Runner to make a decision"
             :effect
             (effect (continue-ability
                       (let [itarget (:card context)
                             card-type (:type itarget)]
                         (if (some #(is-type? % (:type itarget)) (:hand runner))
                           {:optional
                            {:prompt (str "Install another " card-type " from your Grip?")
                             :yes-ability
                             {:prompt (str "Select another " card-type " to install from your Grip")
                              :choices {:card #(and (is-type? % card-type)
                                                    (in-hand? %))}
                              :msg (msg "install " (:title target))
                              :async true
                              :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}}}
                           {:prompt (str "You have no " card-type "s in hand")
                            :choices ["Carry on!"]
                            :prompt-type :bogus}))
                       card nil))}]})

(defcard "Hoshiko Shiro: Untold Protagonist"
  (let [flip-effect (req (update! state side (if (:flipped card)
                                               (assoc card
                                                      :flipped false
                                                      :face :front
                                                      :code (subs (:code card) 0 5)
                                                      :subtype "Natural")
                                               (assoc card
                                                      :flipped true
                                                      :face :back
                                                      :code (str (subs (:code card) 0 5) "flip")
                                                      :subtype "Digital")))
                         (update-link state))]
    {:constant-effects [(link+ (req (:flipped card)) 1)
                        {:type :gain-subtype
                         :req (req (and (same-card? card target) (:flipped card)))
                         :value "Digital"}
                        {:type :lose-subtype
                         :req (req (and (same-card? card target) (:flipped card)))
                         :value "Natural"}]
     :events [{:event :pre-first-turn
               :req (req (= side :runner))
               :effect (effect (update! (assoc card :flipped false :face :front)))}
              {:event :runner-turn-ends
               :interactive (req true)
               :async true
               :effect (req (cond
                              (and (:flipped card)
                                   (not (:accessed-cards runner-reg)))
                              (do (system-msg state :runner "flips their identity to Hoshiko Shiro: Untold Protagonist")
                                  (continue-ability state :runner {:effect flip-effect} card nil))

                              (and (not (:flipped card))
                                   (:accessed-cards runner-reg))
                              (wait-for (gain-credits state :runner 2)
                                        (system-msg state :runner "gains 2 [Credits] and flips their identity to Hoshiko Shiro: Mahou Shoujo")
                                        (continue-ability state :runner {:effect flip-effect} card nil))

                              :else
                              (effect-completed state side eid)))}
              {:event :runner-turn-begins
               :req (req (:flipped card))
               :async true
               :effect (req (wait-for (draw state :runner 1 nil)
                                      (wait-for (lose-credits state :runner 1)
                                                (system-msg state :runner "uses Hoshiko Shiro: Mahou Shoujo to draw 1 card and lose 1 [Credits]")
                                                (effect-completed state side eid))))}]
     :abilities [{:label "flip ID"
                  :msg "flip their ID manually"
                  :effect flip-effect}]}))

(defcard "Hyoubu Institute: Absolute Clarity"
  {:events [{:event :corp-reveal
             :once :per-turn
             :req (req (first-event? state side :corp-reveal))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]
   :abilities [{:cost [:click 1]
                :label "Reveal the top card of the Stack"
                :async true
                :effect (req (if-let [revealed-card (-> runner :deck first)]
                               (do (system-msg state side (str "uses Hyoubu Institute: Absolute Clarity to reveal "
                                                               (:title revealed-card) " from the top of the Stack"))
                                   (reveal state side eid revealed-card))
                               (effect-completed state side eid)))}
               {:cost [:click 1]
                :label "Reveal a random card from the Grip"
                :async true
                :effect (req (if-let [revealed-card (-> runner :hand shuffle first)]
                               (do (system-msg state side (str "uses Hyoubu Institute: Absolute Clarity to reveal "
                                                               (:title revealed-card) " from the Grip"))
                                   (reveal state side eid revealed-card))
                               (effect-completed state side eid)))}]})

(defcard "Iain Stirling: Retired Spook"
  (let [ability {:req (req (> (:agenda-point corp) (:agenda-point runner)))
                 :once :per-turn
                 :msg "gain 2 [Credits]"
                 :async true
                 :effect (effect (gain-credits eid 2))}]
    {:flags {:drip-economy true}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Industrial Genomics: Growing Solutions"
  {:constant-effects [{:type :trash-cost
                       :value (req (count (remove :seen (:discard corp))))}]})

(defcard "Information Dynamics: All You Need To Know"
  {:events (let [inf {:req (req (and (not (:disabled card))
                                     (has-most-faction? state :corp "NBN")))
                      :msg "give the Runner 1 tag"
                      :async true
                      :effect (effect (gain-tags :corp eid 1))}]
             [{:event :pre-start-game
               :effect draft-points-target}
              (assoc inf :event :agenda-scored)
              (assoc inf :event :agenda-stolen)])})

(defcard "Jamie \"Bzzz\" Micken: Techno Savant"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-install
             :req (req (and (has-most-faction? state :runner "Shaper")
                            (first-event? state side :runner-install)))
             :msg "draw 1 card"
             :once :per-turn
             :async true
             :effect (effect (draw eid 1 nil))}]})

(defcard "Jemison Astronautics: Sacrifice. Audacity. Success."
  {:events [{:event :corp-forfeit-agenda
             :async true
             :waiting-prompt "Corp to make a decision"
             :effect
             (effect
               (continue-ability
                 (let [p (inc (get-agenda-points target))]
                   {:prompt (str "Select a card to place advancement tokens on with " (:title card))
                    :choices {:card #(and (installed? %)
                                          (corp? %))}
                    :msg (msg "place " (quantify p "advancement token")
                              " on " (card-str state target))
                    :effect (effect (add-prop :corp target :advance-counter p {:placed true}))})
                 card nil))}]})

(defcard "Jesminder Sareen: Girl Behind the Curtain"
  {:events [{:event :pre-tag
             :async true
             :once :per-run
             :req (req (:run @state))
             :msg "avoid the first tag during this run"
             :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "Jinteki Biotech: Life Imagined"
  {:events [{:event :pre-first-turn
             :req (req (= side :corp))
             :prompt "Choose a copy of Jinteki Biotech to use this game"
             :choices ["The Brewery" "The Tank" "The Greenhouse"]
             :effect (effect (update! (assoc card :biotech-target target :face :front))
                             (system-msg (str "has chosen a copy of Jinteki Biotech for this game")))}]
   :abilities [{:label "Check chosen flip identity"
                :effect (req (case (:biotech-target card)
                               "The Brewery"
                               (toast state :corp "Flip to: The Brewery (Do 2 net damage)" "info")
                               "The Tank"
                               (toast state :corp "Flip to: The Tank (Shuffle Archives into R&D)" "info")
                               "The Greenhouse"
                               (toast state :corp "Flip to: The Greenhouse (Place 4 advancement tokens on a card)" "info")
                               ;; default case
                               (toast state :corp "No flip identity specified" "info")))}
               {:cost [:click 3]
                :req (req (not (:biotech-used card)))
                :label "Flip this identity"
                :async true
                :effect (req (let [flip (:biotech-target card)]
                               (update! state side (assoc (get-card state card) :biotech-used true))
                               (case flip
                                 "The Brewery"
                                 (do (system-msg state side "uses The Brewery to do 2 net damage")
                                     (update! state side (assoc card :code "brewery" :face :brewery))
                                     (damage state side eid :net 2 {:card card}))
                                 "The Tank"
                                 (do (system-msg state side "uses The Tank to shuffle Archives into R&D")
                                     (shuffle-into-deck state side :discard)
                                     (update! state side (assoc card :code "tank" :face :tank))
                                     (effect-completed state side eid))
                                 "The Greenhouse"
                                 (do (system-msg state side (str "uses The Greenhouse to place 4 advancement tokens "
                                                                 "on a card that can be advanced"))
                                     (update! state side (assoc card :code "greenhouse" :face :greenhouse))
                                     (continue-ability
                                       state side
                                       {:prompt "Select a card that can be advanced"
                                        :choices {:card can-be-advanced?}
                                        :effect (effect (add-prop target :advance-counter 4 {:placed true}))}
                                       card nil))
                                 (toast state :corp (str "Unknown Jinteki Biotech: Life Imagined card: " flip) "error"))))}]})

(defcard "Jinteki: Personal Evolution"
  (let [ability {:async true
                 :req (req (not (:winner @state)))
                 :msg "do 1 net damage"
                 :effect (effect (damage eid :net 1 {:card card}))}]
    {:events [(assoc ability
                     :event :agenda-scored
                     :interactive (req true))
              (assoc ability :event :agenda-stolen)]}))

(defcard "Jinteki: Potential Unleashed"
  {:events [{:async true
             :event :pre-resolve-damage
             :req (req (and (-> @state :corp :disable-id not)
                            (= target :net)
                            (pos? (last targets))))
             :effect (req (let [c (first (get-in @state [:runner :deck]))]
                            (system-msg state :corp (str "uses Jinteki: Potential Unleashed to trash " (:title c)
                                                         " from the top of the Runner's Stack"))
                            (mill state :corp eid :runner 1)))}]})

(defcard "Jinteki: Replicating Perfection"
  {:events [{:event :runner-phase-12
             :effect (req (apply prevent-run-on-server
                                 state card (map first (get-remotes state))))}
            {:event :run
             :once :per-turn
             :req (req (is-central? (:server target)))
             :effect (req (apply enable-run-on-server
                                 state card (map first (get-remotes state))))}]
   :req (req (empty? (let [successes (turn-events state side :successful-run)]
                       (filter is-central? (map :server successes)))))
   :effect (req (apply prevent-run-on-server state card (map first (get-remotes state))))
   :leave-play (req (apply enable-run-on-server state card (map first (get-remotes state))))})

(defcard "Jinteki: Restoring Humanity"
  {:events [{:event :corp-turn-ends
             :req (req (pos? (count (remove :seen (:discard corp)))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Kabonesa Wu: Netspace Thrillseeker"
  {:abilities [{:label "Install a non-virus program from your stack, lowering the cost by 1 [Credit]"
                :cost [:click 1]
                :prompt "Choose a program"
                :choices (req (cancellable
                                (filter #(and (program? %)
                                              (not (has-subtype? % "Virus"))
                                              (can-pay? state :runner (assoc eid :source card :source-type :runner-install) % nil
                                                        [:credit (install-cost state side % {:cost-bonus -1})]))
                                        (:deck runner))))
                :msg (msg "install " (:title target) " from the stack, lowering the cost by 1 [Credit]")
                :async true
                :effect (effect (trigger-event :searched-stack nil)
                                (shuffle! :deck)
                                (register-events
                                  card
                                  [{:event :runner-turn-ends
                                    :interactive (req true)
                                    :duration :end-of-turn
                                    :req (req (some #(get-in % [:special :kabonesa]) (all-installed state :runner)))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (req (doseq [program (filter #(get-in % [:special :kabonesa]) (all-installed state :runner))]
                                                   (move state side program :rfg)))}])
                                (runner-install (assoc eid :source card :source-type :runner-install)
                                                (assoc-in target [:special :kabonesa] true)
                                                {:cost-bonus -1}))}]})

(defcard "Kate \"Mac\" McCaffrey: Digital Tinker"
  ;; Effect marks Kate's ability as "used" if it has already met it's trigger condition this turn
  (letfn [(kate-type? [card] (or (hardware? card)
                                 (program? card)))
          (not-triggered? [state card] (no-event? state :runner :runner-install #(kate-type? (:card (first %)))))]
    {:constant-effects [{:type :install-cost
                         :req (req (and (kate-type? target)
                                        (not-triggered? state card)))
                         :value -1}]
     :events [{:event :runner-install
               :req (req (and (kate-type? (:card context))
                              (not-triggered? state card)))
               :silent (req true)
               :msg (msg "reduce the install cost of " (:title (:card context)) " by 1 [Credits]")}]}))

(defcard "Ken \"Express\" Tenma: Disappeared Clone"
  {:events [{:event :play-event
             :req (req (and (has-subtype? (:card context) "Run")
                            (first-event? state :runner :play-event #(has-subtype? (:card (first %)) "Run"))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Khan: Savvy Skiptracer"
  {:events [{:event :pass-ice
             :req (req (first-event? state :runner :pass-ice))
             :async true
             :interactive (req true)
             :effect (effect
                       (continue-ability
                         (when (some #(and (has-subtype? % "Icebreaker")
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [:credit (install-cost state side % {:cost-bonus -1})]))
                                     (:hand runner))
                           {:prompt "Select an icebreaker to install from your Grip"
                            :choices
                            {:req (req (and (in-hand? target)
                                            (has-subtype? target "Icebreaker")
                                            (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                      [:credit (install-cost state side target {:cost-bonus -1})])))}
                            :async true
                            :msg (msg "install " (:title target) ", lowering the cost by 1 [Credits]")
                            :effect (effect (runner-install eid target {:cost-bonus -1}))})
                         card nil))}]})

(defcard "Laramy Fisk: Savvy Investor"
  {:events [{:event :successful-run
             :async true
             :interactive (get-autoresolve :auto-fisk (complement never?))
             :silent (get-autoresolve :auto-fisk never?)
             :optional
             {:req (req (and (is-central? (:server context))
                             (first-event? state side :successful-run
                                           (fn [targets]
                                             (let [context (first targets)]
                                               (is-central? (:server context)))))))
              :autoresolve (get-autoresolve :auto-fisk)
              :prompt "Force the Corp to draw a card?"
              :yes-ability {:msg "force the Corp to draw 1 card"
                            :async true
                            :effect (effect (draw :corp eid 1 nil))}
              :no-ability {:effect (effect (system-msg "declines to use Laramy Fisk: Savvy Investor"))}}}]
   :abilities [(set-autoresolve :auto-fisk "force Corp draw")]})

(defcard "Lat: Ethical Freelancer"
  {:events [{:event :runner-turn-ends
             :optional {:req (req (= (count (:hand runner)) (count (:hand corp))))
                        :autoresolve (get-autoresolve :auto-lat)
                        :prompt "Draw 1 card?"
                        :yes-ability {:async true
                                      :msg "draw 1 card"
                                      :effect (effect (draw :runner eid 1 nil))}
                        :no-ability {:effect (effect (system-msg "declines to use Lat: Ethical Freelancer"))}}}]
   :abilities [(set-autoresolve :auto-lat "Lat: Ethical Freelancer")]})

(defcard "Leela Patel: Trained Pragmatist"
  (let [leela {:interactive (req true)
               :prompt "Select an unrezzed card to return to HQ"
               :choices {:card #(and (not (rezzed? %))
                                     (installed? %)
                                     (corp? %))}
               :msg (msg "add " (card-str state target) " to HQ")
               :effect (effect (move :corp target :hand))}]
    {:events [(assoc leela :event :agenda-scored)
              (assoc leela :event :agenda-stolen)]}))

(defcard "Liza Talking Thunder: Prominent Legislator"
  {:events [{:event :successful-run
             :async true
             :interactive (req true)
             :msg "draw 2 cards and take 1 tag"
             :req (req (and (is-central? (:server context))
                            (first-event? state side :successful-run
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (is-central? (:server context)))))))
             :effect (req (wait-for (gain-tags state :runner 1)
                                    (draw state :runner eid 2 nil)))}]})

(defcard "Los: Data Hijacker"
  {:events [{:event :rez
             :once :per-turn
             :req (req (ice? (:card context)))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 2))}]})

(defcard "MaxX: Maximum Punk Rock"
  (let [ability {:msg (msg (let [deck (:deck runner)]
                             (if (pos? (count deck))
                               (str "trash " (string/join ", " (map :title (take 2 deck))) " from their Stack and draw 1 card")
                               "trash the top 2 cards from their Stack and draw 1 card - but their Stack is empty")))
                 :label "trash and draw cards"
                 :once :per-turn
                 :async true
                 :effect (req (wait-for (mill state :runner :runner 2)
                                        (draw state :runner eid 1 nil)))}]
    {:flags {:runner-turn-draw true
             :runner-phase-12 (req (and (not (:disabled card))
                                        (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner))))}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "MirrorMorph: Endless Iteration"
  (let [mm-ability {:prompt "Gain [Click] or gain 1 [Credits]"
                    :choices ["Gain [Click]" "Gain 1 [Credits]"]
                    :msg (msg (decapitalize target))
                    :once :per-turn
                    :label "Manually trigger ability"
                    :async true
                    :effect (req (if (= "Gain [Click]" target)
                                   (do (gain state side :click 1)
                                       (update! state side (assoc-in (get-card state card) [:special :mm-click] true))
                                       (effect-completed state side eid))
                                   (gain-credits state side eid 1)))}]
    {:implementation "Does not work with terminal Operations"
     :abilities [mm-ability]
     :events [{:event :corp-spent-click
               :async true
               :effect (req (let [cid (first target)
                                  ability-idx (:ability-idx (:source-info eid))
                                  bac-cid (get-in @state [:corp :basic-action-card :cid])
                                  cause (if (keyword? (first target))
                                          (case (first target)
                                            :play-instant [bac-cid 3]
                                            :corp-click-install [bac-cid 2]
                                            (first target)) ; in clojure there's: (= [1 2 3] '(1 2 3))
                                          [cid ability-idx])
                                  prev-actions (get-in card [:special :mm-actions] [])
                                  actions (conj prev-actions cause)]
                              (update! state side (assoc-in card [:special :mm-actions] actions))
                              (update! state side (assoc-in (get-card state card) [:special :mm-click] false))
                              (if (and (= 3 (count actions))
                                       (= 3 (count (distinct actions))))
                                (continue-ability state side mm-ability (get-card state card) nil)
                                (effect-completed state side eid))))}
              {:event :corp-turn-ends
               :effect (effect (update! (assoc-in card [:special :mm-actions] [])))}]
     :constant-effects [{:type :prevent-ability
                         :req (req (and (get-in card [:special :mm-click])
                                        (let [cid (:cid target)
                                              ability-idx (nth targets 2 nil)
                                              cause [cid ability-idx]
                                              prev-actions (get-in card [:special :mm-actions] [])
                                              actions (conj prev-actions cause)]
                                          (not (and (= 4 (count actions))
                                                    (= 4 (count (distinct actions))))))))
                         :value true}]}))

(defcard "Mti Mwekundu: Life Improved"
  {:events [{:event :approach-server
             :optional
             {:req (req (some ice? (:hand corp)))
              :prompt "Install a piece of ice?"
              :once :per-turn
              :yes-ability
              {:prompt "Choose a piece of ice to install from HQ"
               :choices {:card #(and (ice? %)
                                     (in-hand? %))}
               :async true
               :msg "install ice at the innermost position of this server. Runner is now approaching that piece of ice"
               :effect (req (wait-for (corp-install state side target (zone->name (target-server run))
                                                    {:ignore-all-cost true
                                                     :front true})
                                      (swap! state assoc-in [:run :position] 1)
                                      (set-next-phase state :approach-ice)
                                      (update-all-ice state side)
                                      (update-all-icebreakers state side)
                                      (effect-completed state side eid)
                                      (start-next-phase state side nil)))}}}]})

(defcard "Nasir Meidan: Cyber Explorer"
  {:events [{:event :approach-ice
             :req (req (not (rezzed? (:ice context))))
             :effect (effect
                       (register-events
                         card
                         (let [ice (:ice context)
                               cost (rez-cost state side ice)]
                           [{:event :encounter-ice
                             :duration :end-of-encounter
                             :unregister-once-resolved true
                             :req (req (same-card? (:ice context) ice))
                             :msg (msg "lose all credits and gain " cost
                                       " [Credits] from the rez of " (:title ice))
                             :async true
                             :effect (req (wait-for (lose-credits state :runner :all)
                                                    (gain-credits state :runner eid cost)))}])))}]})

(defcard "Nathaniel \"Gnat\" Hall: One-of-a-Kind"
  (let [ability {:label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :interactive (req true)
                 :async true
                 :effect (req (if (and (> 3 (count (:hand runner)))
                                       (:runner-phase-12 @state))
                                (do (system-msg state :runner (str "uses " (:title card) " to gain 1 [Credits]"))
                                    (gain-credits state :runner eid 1))
                                (effect-completed state side eid)))}]
    {:flags {:drip-economy true
             :runner-phase-12 (req (and (not (:disabled card))
                                        (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner))))}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)]}))

(defcard "NBN: Controlling the Message"
  {:events [{:event :runner-trash
             :interactive (req true)
             :once-per-instance true
             :optional
             {:player :corp
              :req (req (and (some #(and (corp? (:card %))
                                         (installed? (:card %)))
                                   targets)
                             (first-event? state side :runner-trash
                                           (fn [targets]
                                             (some #(and (installed? (:card %))
                                                         (corp? (:card %)))
                                                   targets)))))
              :waiting-prompt "Corp to make a decision"
              :prompt "Trace the Runner with NBN: Controlling the Message?"
              :autoresolve (get-autoresolve :auto-ctm)
              :yes-ability
              {:trace {:base 4
                       :successful
                       {:msg "give the Runner 1 tag"
                        :async true
                        :effect (effect (gain-tags :corp eid 1 {:unpreventable true}))}}}}}]
   :abilities [(set-autoresolve :auto-ctm "CtM")]})

(defcard "NBN: Making News"
  {:recurring 2
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "NBN: Reality Plus"
  {:events [{:event :runner-gain-tag
             :req (req (first-event? state :runner :runner-gain-tag))
             :player :corp
             :async true
             :waiting-prompt "Corp to choose an option"
             :prompt "Select option"
             :choices ["Gain 2 [Credits]" "Draw 2 cards"]
             :msg (msg (decapitalize target))
             :effect (req
                       (if (= target "Gain 2 [Credits]")
                            (gain-credits state :corp eid 2)
                            (draw state :corp eid 2 nil)))}]})

(defcard "NBN: The World is Yours*"
  {:constant-effects [(corp-hand-size+ 1)]})

(defcard "Near-Earth Hub: Broadcast Center"
  {:events [{:event :server-created
             :req (req (first-event? state :corp :server-created))
             :msg "draw 1 card"
             :async true
             :effect (effect (draw :corp eid 1 nil))}]})

(defcard "Nero Severn: Information Broker"
  {:events [{:event :encounter-ice
             :optional
             {:req (req (has-subtype? (:ice context) "Sentry"))
              :prompt "Do you want to jack out?"
              :once :per-turn
              :yes-ability {:async true
                            :msg "jack out"
                            :effect (effect (jack-out eid))}}}]})

(defcard "New Angeles Sol: Your News"
  (let [nasol {:optional
               {:prompt "Play a Current?"
                :player :corp
                :req (req (some #(has-subtype? % "Current") (concat (:hand corp) (:discard corp) (:current corp))))
                :yes-ability {:prompt "Select a Current to play from HQ or Archives"
                              :show-discard true
                              :async true
                              :choices {:card #(and (has-subtype? % "Current")
                                                    (corp? %)
                                                    (or (in-hand? %)
                                                        (in-discard? %)))}
                              :msg (msg "play a current from " (name-zone "Corp" (get-zone target)))
                              :effect (effect (play-instant eid target))}}}]
    {:events [(assoc nasol :event :agenda-scored)
              (assoc nasol :event :agenda-stolen)]}))

(defcard "NEXT Design: Guarding the Net"
  (let [ndhelper (fn nd [n] {:prompt (str "When finished, click NEXT Design: Guarding the Net to draw back up to 5 cards in HQ. "
                                          "Select a piece of ice in HQ to install:")
                             :choices {:card #(and (corp? %)
                                                   (ice? %)
                                                   (in-hand? %))}
                             :effect (req (wait-for (corp-install state side target nil nil)
                                                    (continue-ability state side (when (< n 3) (nd (inc n))) card nil)))})]
    {:events [{:event :pre-first-turn
               :req (req (= side :corp))
               :msg "install up to 3 pieces of ice and draw back up to 5 cards"
               :async true
               :effect (req (wait-for (resolve-ability state side (ndhelper 1) card nil)
                                      (update! state side (assoc card :fill-hq true))
                                      (effect-completed state side eid)))}]
     :abilities [{:req (req (:fill-hq card))
                  :label "draw remaining cards"
                  :msg (msg "draw " (- 5 (count (:hand corp))) " cards")
                  :effect (req (draw state side (- 5 (count (:hand corp))))
                               (update! state side (dissoc card :fill-hq))
                               (swap! state assoc :turn-events nil))}]}))

(defcard "Nisei Division: The Next Generation"
  {:events [{:event :reveal-spent-credits
             :req (req (and (some? (first targets))
                            (some? (second targets))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Noise: Hacker Extraordinaire"
  {:events [{:async true
             :event :runner-install
             :req (req (has-subtype? (:card context) "Virus"))
             :msg "force the Corp to trash the top card of R&D"
             :effect (effect (mill :corp eid :corp 1))}]})

(defcard "Null: Whistleblower"
  {:events [{:event :encounter-ice
             :optional
             {:req (req (pos? (count (:hand runner))))
              :prompt "Trash a card in grip to lower ice strength by 2?"
              :once :per-turn
              :yes-ability
              {:prompt "Select a card in your Grip to trash"
               :choices {:card in-hand?}
               :msg (msg "trash " (:title target)
                         " and reduce the strength of " (:title current-ice)
                         " by 2 for the remainder of the run")
               :async true
               :effect (effect (register-floating-effect
                                 card
                                 (let [ice current-ice]
                                   {:type :ice-strength
                                    :duration :end-of-run
                                    :req (req (same-card? target ice))
                                    :value -2}))
                               (update-all-ice)
                               (trash eid target {:unpreventable true}))}}}]})

(defcard "Omar Keung: Conspiracy Theorist"
  {:abilities [{:cost [:click 1]
                :msg "make a run on Archives"
                :once :per-turn
                :makes-run true
                :async true
                :effect (effect (update! (assoc-in card [:special :omar-run] true))
                                (make-run eid :archives (get-card state card)))}]
   :events [{:event :pre-successful-run
             :interactive (req true)
             :req (req (and (get-in card [:special :omar-run])
                            (= :archives (-> run :server first))))
             :prompt "Treat as a successful run on which server?"
             :choices ["HQ" "R&D"]
             :effect (req (let [target-server (if (= target "HQ") :hq :rd)]
                            (swap! state assoc-in [:run :server] [target-server])
                            (trigger-event state :corp :no-action)
                            (system-msg state side (str "uses Omar Keung: Conspiracy Theorist to make a successful run on " target))))}
            {:event :run-ends
             :effect (effect (update! (dissoc-in card [:special :omar-run])))}]})

(defcard "Pālanā Foods: Sustainable Growth"
  {:events [{:event :runner-draw
             :req (req (and (first-event? state :corp :runner-draw)
                            (pos? target)))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Quetzal: Free Spirit"
  {:abilities [(assoc (break-sub nil 1 "Barrier" {:repeatable false}) :once :per-turn)]})

(defcard "Reina Roja: Freedom Fighter"
  (letfn [(not-triggered? [state]
            (no-event? state :runner :rez #(ice? (:card (first %)))))]
    {:constant-effects [{:type :rez-cost
                         :req (req (and (ice? target)
                                        (not (rezzed? target))
                                        (not-triggered? state)))
                         :value 1}]
     :events [{:event :rez
               :req (req (and (ice? (:card context))
                              (not-triggered? state)))
               :msg (msg "increased the rez cost of " (:title (:card context)) " by 1 [Credits]")}]}))

(defcard "René \"Loup\" Arcemont: Party Animal"
  {:events [{:event :runner-trash
             :req (req (and (:accessed context)
                            (first-event? state side :runner-trash
                                          (fn [targets]
                                            (some #(:accessed %) targets)))))
             :async true
             :msg "gain 1 [Credits] and draw 1 card"
             :effect (req (wait-for (draw state :runner 1 nil)
                                    (gain-credits state :runner eid 1)))}]})

(defcard "Rielle \"Kit\" Peddler: Transhuman"
  {:events [{:event :encounter-ice
             :once :per-turn
             :msg (msg "make " (:title (:ice context))
                       " gain Code Gate until the end of the run")
             :effect (effect (register-floating-effect
                               card
                               (let [ice (:ice context)]
                                 {:type :gain-subtype
                                  :duration :end-of-run
                                  :req (req (same-card? ice target))
                                  :value "Code Gate"})))}]})

(defcard "Saraswati Mnemonics: Endless Exploration"
  (letfn [(install-card [chosen]
            {:prompt "Select a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (req (let [tgtcid (:cid chosen)]
                            (register-turn-flag!
                              state side
                              card :can-rez
                              (fn [state side card]
                                (if (= (:cid card) tgtcid)
                                  ((constantly false)
                                   (toast state :corp "Cannot rez due to Saraswati Mnemonics: Endless Exploration." "warning"))
                                  true)))
                            (register-turn-flag!
                              state side
                              card :can-score
                              (fn [state side card]
                                (if (and (= (:cid card) tgtcid)
                                         (<= (get-advancement-requirement card) (get-counters card :advancement)))
                                  ((constantly false)
                                   (toast state :corp "Cannot score due to Saraswati Mnemonics: Endless Exploration." "warning"))
                                  true))))
                          (wait-for (corp-install state side chosen target nil)
                                    (add-prop state :corp (find-latest state chosen) :advance-counter 1 {:placed true})
                                    (effect-completed state side eid)))})]
    {:abilities [{:async true
                  :label "Install a card from HQ"
                  :cost [:click 1 :credit 1]
                  :prompt "Select a card to install from HQ"
                  :choices {:card #(and (or (asset? %) (agenda? %) (upgrade? %))
                                     (corp? %)
                                     (in-hand? %))}
                  :msg (msg "install a card in a remote server and place 1 advancement token on it")
                  :effect (effect (continue-ability (install-card target) card nil))}]}))

(defcard "Seidr Laboratories: Destiny Defined"
  {:implementation "Manually triggered"
   :abilities [{:req (req (and run (seq (:discard corp))))
                :label "add card from Archives to R&D during a run"
                :once :per-turn
                :prompt "Select a card to add to the top of R&D"
                :show-discard true
                :choices {:card #(and (corp? %)
                                      (in-discard? %))}
                :effect (effect (move target :deck {:front true}))
                :msg (msg "add " (if (:seen target) (:title target) "a card") " to the top of R&D")}]})

(defcard "Silhouette: Stealth Operative"
  {:events [{:event :successful-run
             :interactive (req (some #(not (rezzed? %)) (all-installed state :corp)))
             :async true
             :req (req (and (= :hq (target-server context))
                            (first-successful-run-on-server? state :hq)))
             :choices {:card #(and (installed? %)
                                   (not (rezzed? %)))}
             :msg "expose 1 card"
             :effect (effect (expose eid target))}]})

(defcard "Skorpios Defense Systems: Persuasive Power"
  {:implementation "Manually triggered, no restriction on which cards in Heap can be targeted. Cannot use on in progress run event"
   :abilities [{:label "Remove a card in the Heap that was just trashed from the game"
                :waiting-prompt "Corp to make a decision"
                :prompt "Choose a card in the Runner's Heap that was just trashed"
                :once :per-turn
                :choices (req (cancellable (:discard runner)))
                :msg (msg "remove " (:title target) " from the game")
                :effect (req (move state :runner target :rfg))}]})

(defcard "Spark Agency: Worldswide Reach"
  {:events [{:event :rez
             :req (req (and (has-subtype? (:card context) "Advertisement")
                            (first-event? state :corp :rez #(has-subtype? (:card (first %)) "Advertisement"))))
             :async true
             :effect (effect (lose-credits :runner eid 1))
             :msg (msg "make the Runner lose 1 [Credits] by rezzing an Advertisement")}]})

(defcard "Sportsmetal: Go Big or Go Home"
  (let [ab {:prompt "Gain 2 [Credits] or draw 2 cards?"
            :player :corp
            :choices ["Gain 2 [Credits]" "Draw 2 cards"]
            :msg (msg (if (= target "Gain 2 [Credits]")
                        "gain 2 [Credits]"
                        "draw 2 cards"))
            :async true
            :interactive (req true)
            :effect (req (if (= target "Gain 2 [Credits]")
                           (gain-credits state :corp eid 2)
                           (draw state :corp eid 2 nil)))}]
    {:events [(assoc ab :event :agenda-scored)
              (assoc ab :event :agenda-stolen)]}))

(defcard "SSO Industries: Fueling Innovation"
  (letfn [(installed-faceup-agendas [state]
            (->> (all-installed state :corp)
                 (filter agenda?)
                 (filter faceup?)))
          (selectable-ice? [card]
            (and
              (ice? card)
              (installed? card)
              (zero? (get-counters card :advancement))))
          (ice-with-no-advancement-tokens [state]
            (->> (all-installed state :corp)
                 (filter selectable-ice?)))]
    {:events [{:event :corp-turn-ends
               :optional
               {:req (req (and (not-empty (installed-faceup-agendas state))
                               (not-empty (ice-with-no-advancement-tokens state))))
                :waiting-prompt "Corp to make a decision"
                :prompt "Place advancement tokens?"
                :autoresolve (get-autoresolve :auto-sso)
                :yes-ability
                {:async true
                 :effect (req (let [agendas (installed-faceup-agendas state)
                                    agenda-points (->> agendas
                                                       (map :agendapoints)
                                                       (reduce +))
                                    ice (ice-with-no-advancement-tokens state)]
                                (continue-ability
                                  state side
                                  {:prompt (str "Select a piece of ice with no advancement tokens to place "
                                                (quantify agenda-points "advancement token") " on")
                                   :choices {:card #(selectable-ice? %)}
                                   :msg (msg "place " (quantify agenda-points "advancement token")
                                             " on " (card-str state target))
                                   :effect (effect (add-prop target :advance-counter agenda-points {:placed true}))}
                                  card nil)))}}}]
     :abilities [(set-autoresolve :auto-sso "SSO")]}))

(defcard "Steve Cambridge: Master Grifter"
  {:events [{:event :successful-run
             :optional
             {:req (req (and (= :hq (target-server context))
                             (first-successful-run-on-server? state :hq)
                             (<= 2 (count (:discard runner)))
                             (not (zone-locked? state :runner :discard))))
              :prompt "Use Steve Cambridge ability?"
              :yes-ability
              {:interactive (req true)
               :async true
               :prompt "Select 2 cards in your Heap"
               :show-discard true
               :choices {:max 2
                         :all true
                         :card #(and (in-discard? %)
                                     (runner? %))}
               :effect
               (effect (continue-ability
                         (let [c1 (first targets)
                               c2 (second targets)]
                           {:waiting-prompt "Corp to make a decision"
                            :prompt "Choose which card to remove from the game"
                            :player :corp
                            :choices [c1 c2]
                            :msg (msg (let [[chosen other](if (= target c1)
                                                            [c1 c2]
                                                            [c2 c1])]
                                        (str "add " (:title other) " to their grip."
                                             " Corp removes " (:title chosen) " from the game")))
                            :effect (req (let [[chosen other] (if (= target c1)
                                                                [c1 c2]
                                                                [c2 c1])]
                                           (move state :runner chosen :rfg)
                                           (move state :runner other :hand)))})
                         card nil))}}}]})

(defcard "Strategic Innovations: Future Forward"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-turn-ends
             :req (req (and (not (:disabled card))
                            (has-most-faction? state :corp "Haas-Bioroid")
                            (pos? (count (:discard corp)))))
             :prompt "Select a card in Archives to shuffle into R&D"
             :choices {:card #(and (corp? %)
                                   (in-discard? %))}
             :player :corp
             :show-discard true
             :msg (msg "shuffle " (if (:seen target) (:title target) "a card")
                       " into R&D")
             :effect (effect (move :corp target :deck)
                             (shuffle! :corp :deck))}]})

(defcard "Sunny Lebeau: Security Specialist"
  ;; No special implementation
  {})

(defcard "SYNC: Everything, Everywhere"
  {:constant-effects [{:type :card-ability-additional-cost
                       :req (req (let [targetcard (first targets)
                                       target (second targets)]
                                   (and (not (:sync-flipped card))
                                        (same-card? targetcard (:basic-action-card runner))
                                        (= "Remove 1 tag" (:label target)))))
                       :value [:credit 1]}
                      {:type :card-ability-additional-cost
                       :req (req (let [targetcard (first targets)
                                       target (second targets)]
                                   (and (:sync-flipped card)
                                        (same-card? targetcard (:basic-action-card corp))
                                        (= "Trash 1 resource if the Runner is tagged" (:label target)))))
                       :value [:credit -2]}]
   :abilities [{:cost [:click 1]
                :effect (req (if (:sync-flipped card)
                               (update! state side (-> card (assoc :sync-flipped false :face :front :code "09001")))
                               (update! state side (-> card (assoc :sync-flipped true :face :back :code "sync")))))
                :label "Flip this identity"
                :msg (msg "flip their ID")}]})

(defcard "Synthetic Systems: The World Re-imagined"
  {:events [{:event :pre-start-game
             :effect draft-points-target}]
   :flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                    (has-most-faction? state :corp "Jinteki")
                                    (<= 2 (count (filter ice? (all-installed state :corp))))))}
   :abilities [{:prompt "Select two pieces of installed ice to swap"
                :label "swap two pieces of installed ice"
                :choices {:card #(and (installed? %)
                                      (ice? %))
                          :max 2
                          :all true}
                :once :per-turn
                :effect (req (apply swap-ice state side targets))
                :msg (msg "swap the positions of " (card-str state (first targets))
                          " and " (card-str state (second targets)))}]})

(defcard "Tāo Salonga: Telepresence Magician"
  (let [swap-ability
        {:interactive (req true)
         :optional
         {:req (req (<= 2 (count (filter ice? (all-installed state :corp)))))
          :prompt "Swap ice with Tāo Salonga ability?"
          :yes-ability
          {:prompt "Select 2 ice"
           :choices {:req (req (and (installed? target)
                                    (ice? target)))
                     :max 2
                     :all true}
           :msg (msg "swap the positions of " (card-str state (first targets))
                     " and " (card-str state (second targets)))
           :effect (req (swap-ice state side (first targets) (second targets)))}}}]
    {:events [(assoc swap-ability :event :agenda-scored)
              (assoc swap-ability :event :agenda-stolen)]}))

(defcard "Tennin Institute: The Secrets Within"
  {:flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                    (not-last-turn? state :runner :successful-run)))}
   :abilities [{:msg (msg "place 1 advancement token on " (card-str state target))
                :label "Place 1 advancement token on a card if the Runner did not make a successful run last turn"
                :choices {:card installed?}
                :req (req (and (:corp-phase-12 @state)
                               (not-last-turn? state :runner :successful-run)))
                :once :per-turn
                :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]})

(defcard "The Catalyst: Convention Breaker"
  ;; No special implementation
  {})

(defcard "The Foundry: Refining the Process"
  {:events [{:event :rez
             :optional
             {:prompt "Add another copy to HQ?"
              :req (req (and (ice? (:card context))
                             (first-event? state :runner :rez #(ice? (:card (first %))))))
              :yes-ability
              {:effect (req (if-let [found-card (some #(when (= (:title %) (:title (:card context))) %) (concat (:deck corp) (:play-area corp)))]
                              (do (move state side found-card :hand)
                                  (system-msg state side (str "uses The Foundry to add a copy of "
                                                              (:title found-card) " to HQ, and shuffles their deck"))
                                  (shuffle! state side :deck))
                              (do (system-msg state side (str "fails to find a target for The Foundry, and shuffles their deck"))
                                  (shuffle! state side :deck))))}}}]})

(defcard "The Masque: Cyber General"
  {:events [{:event :pre-start-game
             :effect draft-points-target}]})

(defcard "The Outfit: Family Owned and Operated"
  {:events [{:event :corp-gain-bad-publicity
             :msg "gain 3 [Credit]"
             :async true
             :effect (effect (gain-credits eid 3))}]})

(defcard "The Professor: Keeper of Knowledge"
  ;; No special implementation
  {})

(defcard "The Shadow: Pulling the Strings"
  {:events [{:event :pre-start-game
             :effect draft-points-target}]})

(defcard "The Syndicate: Profit over Principle"
  ;; No special implementation
  {})

(defcard "Titan Transnational: Investing In Your Future"
  {:events [{:event :agenda-scored
             :msg (msg "add 1 agenda counter to " (:title (:card context)))
             :effect (effect (add-counter (get-card state (:card context)) :agenda 1))}]})

(defcard "Valencia Estevez: The Angel of Cayambe"
  {:events [{:event :pre-start-game
             :req (req (and (= side :runner)
                            (zero? (count-bad-pub state))))
             ;; This doesn't use `gain-bad-publicity` to avoid the event
             :effect (effect (gain :corp :bad-publicity 1))}]})

(defcard "Weyland Consortium: Because We Built It"
  {:recurring 1
   :interactions {:pay-credits {:req (req (= :advance (:source-type eid)))
                                :type :recurring}}})

(defcard "Weyland Consortium: Builder of Nations"
  {:implementation "Erratum: The first time an encounter with a piece of ice with at least 1 advancement token ends each turn, do 1 meat damage."
   :events [{:event :end-of-encounter
             :async true
             :req (req (and (rezzed? (:ice context))
                            (pos? (get-counters (:ice context) :advancement))
                            (first-event? state :runner :end-of-encounter
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (and (rezzed? (:ice context))
                                                   (pos? (get-counters (:ice context) :advancement))))))))
             :msg "do 1 meat damage"
             :effect (effect (damage eid :meat 1 {:card card}))}]})

(defcard "Weyland Consortium: Building a Better World"
  {:events [{:event :play-operation
             :req (req (has-subtype? (:card context) "Transaction"))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Weyland Consortium: Built to Last"
  {:events [{:event :advance
             :async true
             :req (req ((complement pos?)
                        (- (get-counters target :advancement) (:amount (second targets) 0))))
             :msg "gain 2 [Credits]"
             :effect (req (gain-credits state :corp eid 2))}]})

(defcard "Whizzard: Master Gamer"
  {:recurring 3
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (corp? target)))
                                :type :recurring}}})

(defcard "Wyvern: Chemically Enhanced"
  {:events [{:event :pre-start-game
             :effect draft-points-target}
            {:event :runner-trash
             :interactive (req true)
             :req (req (and (has-most-faction? state :runner "Anarch")
                            (corp? (:card target))
                            (pos? (count (:discard runner)))
                            (not (zone-locked? state :runner :discard))))
             :msg (msg "shuffle " (:title (last (:discard runner))) " into their Stack")
             :effect (effect (move :runner (last (:discard runner)) :deck)
                             (shuffle! :runner :deck)
                             (trigger-event :searched-stack nil))}]})

(defcard "Zahya Sadeghi: Versatile Smuggler"
  {:events [{:event :run-ends
             :optional
             {:req (req (and (#{:hq :rd} (target-server context))
                          (pos? (total-cards-accessed context))))
              :prompt "Gain 1 [Credits] for each card you accessed?"
              :async true
              :once :per-turn
              :yes-ability
              {:msg (msg "gain " (total-cards-accessed context) " [Credits]")
               :once :per-turn
               :async true
               :effect (req (gain-credits state :runner eid (total-cards-accessed context)))}}}]})
