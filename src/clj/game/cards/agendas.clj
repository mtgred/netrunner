(ns game.cards.agendas
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

(defn ice-boost-agenda [subtype]
  (letfn [(count-ice [corp]
            (reduce (fn [c server]
                      (+ c (count (filter #(and (has-subtype? % subtype)
                                                (rezzed? %))
                                          (:ices server)))))
                    0 (flatten (seq (:servers corp)))))]
    {:msg (msg "gain " (count-ice corp) " [Credits]")
     :interactive (req true)
     :effect (effect (gain-credits (count-ice corp))
                     (update-all-ice))
     :swapped {:effect (req (update-all-ice state side))}
     :events {:pre-ice-strength {:req (req (has-subtype? target subtype))
                                 :effect (effect (ice-strength-bonus 1 target))}}}))

;; Card definitions
(def card-definitions
  {"15 Minutes"
   {:abilities [{:cost [:click 1] :msg "shuffle 15 Minutes into R&D"
                 :label "Shuffle 15 Minutes into R&D"
                 :effect (req (let [corp-agendas (get-in corp [:scored])
                                    agenda-owner (if (some #(same-card? % card) corp-agendas) :corp :runner)]
                                (gain-agenda-point state agenda-owner (- (:agendapoints card))))
                              ; refresh agendapoints to 1 before shuffle in case it was modified by e.g. The Board
                              (move state :corp (dissoc (assoc card :agendapoints 1) :seen :rezzed) :deck {:front true})
                              (shuffle! state :corp :deck))}]
    :flags {:has-abilities-when-stolen true}}

   "Accelerated Beta Test"
   (letfn [(abt [n i]
             (if (pos? i)
               {:async true
                :prompt "Select a piece of ICE from the Temporary Zone to install"
                :choices {:req #(and (corp? %)
                                     (ice? %)
                                     (in-play-area? %))}
                :effect (req (wait-for (corp-install state side target nil
                                                     {:ignore-all-cost true :install-state :rezzed-no-cost})
                                       (let [card (get-card state card)]
                                         (unregister-events state side card)
                                         (if (not (:shuffle-occurred card))
                                           (if (< n i)
                                             (continue-ability state side (abt (inc n) i) card nil)
                                             (do (doseq [c (get-in @state [:corp :play-area])]
                                                   (system-msg state side "trashes a card")
                                                   (trash state side c {:unpreventable true}))
                                                 (effect-completed state side eid)))
                                           (do (doseq [c (get-in @state [:corp :play-area])]
                                                 (move state side c :deck))
                                               (shuffle! state side :deck)
                                               (effect-completed state side eid))))))
                :cancel-effect (req (doseq [c (get-in @state [:corp :play-area])]
                                      (system-msg state side "trashes a card")
                                      (trash state side c {:unpreventable true})))}
               {:prompt "None of the cards are ice. Say goodbye!"
                :choices ["I have no regrets"]
                :effect (req (doseq [c (get-in @state [:corp :play-area])]
                               (system-msg state side "trashes a card")
                               (trash state side c {:unpreventable true})))}))]
     {:interactive (req true)
      :optional {:prompt "Look at the top 3 cards of R&D?"
                 :yes-ability {:async true
                               :msg "look at the top 3 cards of R&D"
                               :effect (req (register-events state side
                                                             {:corp-shuffle-deck
                                                              {:effect (effect (update! (assoc card :shuffle-occurred true)))}}
                                                             card)
                                         (let [n (count (filter ice? (take 3 (:deck corp))))]
                                           (doseq [c (take (min (count (:deck corp)) 3) (:deck corp))]
                                             (move state side c :play-area))
                                           (continue-ability state side (abt 1 n) card nil)))}}})

   "Advanced Concept Hopper"
   {:events
    {:run
     {:req (req (first-event? state side :run))
      :effect (effect (show-wait-prompt :runner "Corp to use Advanced Concept Hopper")
                      (continue-ability
                        {:player :corp
                         :prompt "Use Advanced Concept Hopper to draw 1 card or gain 1 [Credits]?"
                         :once :per-turn
                         :choices ["Draw 1 card" "Gain 1 [Credits]" "No action"]
                         :effect (req (case target
                                        "Gain 1 [Credits]"
                                        (do (gain-credits state :corp 1)
                                            (system-msg state :corp (str "uses Advanced Concept Hopper to gain 1 [Credits]")))
                                        "Draw 1 card"
                                        (do (draw state :corp)
                                            (system-msg state :corp (str "uses Advanced Concept Hopper to draw 1 card")))
                                        "No action"
                                        (system-msg state :corp (str "doesn't use Advanced Concept Hopper")))
                                      (clear-wait-prompt state :runner)
                                      (effect-completed state side eid))}
                        card nil))}}}

   "Ancestral Imager"
   {:events {:jack-out {:msg "do 1 net damage"
                        :effect (effect (damage :net 1))}}}

   "AR-Enhanced Security"
   {:events {:runner-trash {:once :per-turn
                            :async true
                            :req (req (some corp? targets))
                            :msg "give the Runner a tag for trashing a Corp card"
                            :effect (effect (gain-tags eid 1))}}}

   "Architect Deployment Test"
   {:interactive (req true)
    :async true
    :msg "look at the top 5 cards of R&D"
    :prompt (msg "The top cards of R&D are (top->bottom) " (join ", " (map :title (take 5 (:deck corp)))))
    :choices ["OK"]
    :effect (effect (continue-ability
                      {:prompt "Install a card?"
                       :choices (filter corp-installable-type? (take 5 (:deck corp)))
                       :effect (effect (corp-install eid target nil
                                                     {:ignore-all-cost true
                                                      :install-state :rezzed-no-rez-cost}))
                       :cancel-effect (effect (system-msg "does not install any of the top 5 cards")
                                              (effect-completed eid))}
                      card nil))}

   "Armed Intimidation"
   {:async true
    :effect (effect (show-wait-prompt :corp "Runner to suffer 5 meat damage or take 2 tags")
                    (continue-ability
                      :runner
                      {:async true
                       :choices ["Suffer 5 meat damage" "Take 2 tags"]
                       :prompt "Choose Armed Intimidation score effect"
                       :effect (req (clear-wait-prompt state :corp)
                                    (case target
                                      "Suffer 5 meat damage"
                                      (do (damage state :runner eid :meat 5 {:card card :unboostable true})
                                          (system-msg state :runner "chooses to suffer 5 meat damage from Armed Intimidation"))
                                      "Take 2 tags"
                                      (do (gain-tags state :runner eid 2 {:card card})
                                          (system-msg state :runner "chooses to take 2 tags from Armed Intimidation"))))}
                      card nil))}

   "Armored Servers"
   {:implementation "Runner must trash cards manually when required"
    :effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:agenda 1]
                 :req (req (:run @state))
                 :msg "make the Runner trash a card from their grip to jack out or break subroutines for the remainder of the run"}]}

   "AstroScript Pilot Program"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:agenda 1]
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req can-be-advanced?}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Award Bait"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :req (req (not-empty (filter #(can-be-advanced? %) (all-installed state :corp))))
             :effect (effect (show-wait-prompt :runner "Corp to place advancement tokens with Award Bait")
                             (continue-ability
                               {:async true
                                :choices ["0", "1", "2"]
                                :prompt "How many advancement tokens?"
                                :effect (req (let [c (str->int target)]
                                               (continue-ability
                                                 state side
                                                 {:choices {:req can-be-advanced?}
                                                  :msg (msg "place " c " advancement tokens on " (card-str state target))
                                                  :cancel-effect (req (clear-wait-prompt state :runner)
                                                                      (effect-completed state side eid))
                                                  :effect (effect (add-prop :corp target :advance-counter c {:placed true})
                                                                  (clear-wait-prompt :runner))} card nil)))}
                               card nil))}}

   "Bacterial Programming"
   (letfn [(hq-step [remaining to-trash to-hq]
             {:async true
              :prompt "Select a card to move to HQ"
              :choices (conj (vec remaining) "Done")
              :effect (req (if (= "Done" target)
                             (do
                               (doseq [t to-trash]
                                 (trash state :corp t {:unpreventable true}))
                               (doseq [h to-hq]
                                 (move state :corp h :hand))
                               (if (not-empty remaining)
                                 (continue-ability state :corp (reorder-choice :corp (vec remaining)) card nil)
                                 (do (clear-wait-prompt state :runner)
                                     (effect-completed state :corp eid)))
                               (system-msg state :corp (str "uses Bacterial Programming to add " (count to-hq)
                                                            " cards to HQ, discard " (count to-trash)
                                                            ", and arrange the top cards of R&D")))
                             (continue-ability state :corp (hq-step
                                                             (clojure.set/difference (set remaining) (set [target]))
                                                             to-trash
                                                             (conj to-hq target)) card nil)))})
           (trash-step [remaining to-trash]
             {:async true
              :prompt "Select a card to discard"
              :choices (conj (vec remaining) "Done")
              :effect (req (if (= "Done" target)
                             (continue-ability state :corp (hq-step remaining to-trash `()) card nil)
                             (continue-ability state :corp (trash-step
                                                             (clojure.set/difference (set remaining) (set [target]))
                                                             (conj to-trash target)) card nil)))})]
     (let [arrange-rd (effect (continue-ability
                                {:optional
                                 {:async true
                                  :prompt "Arrange top 7 cards of R&D?"
                                  :yes-ability {:async true
                                                :effect (req (let [c (take 7 (:deck corp))]
                                                               (when (:run @state)
                                                                 (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                                                               (show-wait-prompt state :runner "Corp to use Bacterial Programming")
                                                               (continue-ability state :corp (trash-step c `()) card nil)))}}}
                                card nil))]
       {:effect arrange-rd
        :async true
        :stolen {:async true
                 :effect arrange-rd}
        :interactive (req true)}))

   "Bellona"
   {:steal-cost-bonus (req [:credit 5])
    :effect (req (gain-credits state :corp 5)
                 (system-msg state side (str "uses " (:title card) " to gain 5 [Credits]")))}

   "Better Citizen Program"
   (letfn [(ability [kind]
             (effect (show-wait-prompt :runner "Corp to use Better Citizen Program")
                     (continue-ability
                       :corp
                       {:optional
                        {:prompt "Give the runner 1 tag?"
                         :yes-ability {:async true
                                       :msg (str "give the Runner a tag for " kind)
                                       :effect (req (swap! state assoc-in [:per-turn (:cid card)] true)
                                                    (gain-tags state :corp eid 1))}
                         :end-effect (effect (clear-wait-prompt :runner))}}
                       card nil)))]
     {:events {:play-event {:req (req (and (first-event? state :runner :run)
                                           (has-subtype? target "Run")
                                           (not (used-this-turn? (:cid card) state))))
                            :async true
                            :effect (ability "playing a run event")}
               :runner-install {:silent (req true)
                                :req (req (and (has-subtype? target "Icebreaker")
                                               (first-event? state :runner :runner-install #(has-subtype? (first %) "Icebreaker"))
                                               (not (used-this-turn? (:cid card) state))))
                                :async true
                                :effect (ability "installing an icebreaker")}}})

   "Bifrost Array"
   {:req (req (not (empty? (filter #(not= (:title %)
                                          "Bifrost Array")
                                   (:scored corp)))))
    :optional {:prompt "Trigger the ability of a scored agenda?"
               :yes-ability {:prompt "Select an agenda to trigger the \"when scored\" ability of"
                             :choices {:req #(and (agenda? %)
                                                  (not= (:title %)
                                                        "Bifrost Array")
                                                  (= (first (:zone %))
                                                     :scored)
                                                  (when-scored? %)
                                                  (:abilities %))}
                             :msg (msg "trigger the \"when scored\" ability of " (:title target))
                             :effect (effect (continue-ability (card-def target) target nil))}
               :no-ability {:effect (effect (clear-wait-prompt :runner))}}}

   "Brain Rewiring"
   {:effect (effect (show-wait-prompt :runner "Corp to use Brain Rewiring")
                    (resolve-ability
                      {:optional
                       {:prompt "Pay credits to add random cards from Runner's Grip to the bottom of their Stack?"
                        :yes-ability {:prompt "How many credits?"
                                      :choices {:number (req (min (:credit corp)
                                                                  (count (:hand runner))))}
                                      :async true
                                      :effect (req (when (pos? target)
                                                     (pay state :corp card :credit target)
                                                     (let [from (take target (shuffle (:hand runner)))]
                                                       (doseq [c from]
                                                         (move state :runner c :deck))
                                                       (system-msg state side (str "uses Brain Rewiring to pay " target
                                                                                   " [Credits] and add " target
                                                                                   " cards from the Runner's Grip"
                                                                                   " to the bottom of their Stack."
                                                                                   " The Runner draws 1 card"))
                                                       (wait-for (draw state :runner 1 nil)
                                                                 (clear-wait-prompt state :runner)
                                                                 (effect-completed state side eid)))))}
                        :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                      card nil))}

   "Braintrust"
   {:effect (effect (add-counter card :agenda (quot (- (get-counters card :advancement) 3) 2)))
    :silent (req true)
    :events {:pre-rez-cost {:req (req (ice? target))
                            :effect (req (rez-cost-bonus state side (- (get-counters card :agenda))))}}}

   "Breaking News"
   {:async true
    :effect (effect (gain-tags :corp eid 2)
                    (register-events
                      {:corp-turn-ends {:msg "make the Runner lose 2 tags"
                                        :effect (effect (lose :runner :tag 2)
                                                        (unregister-events card))}
                       :runner-turn-ends {:msg "make the Runner lose 2 tags"
                                          :effect (effect (lose :runner :tag 2)
                                                          (unregister-events card))}}
                      card))
    :silent (req true)
    :msg "give the Runner 2 tags"
    :events {:corp-turn-ends nil
             :runner-turn-ends nil}}

   "Broad Daylight"
   (letfn [(add-counters [state side card eid]
             (add-counter state :corp card :agenda (count-bad-pub state))
             (effect-completed state side eid))]
     {:async true
      :effect (effect
                (continue-ability
                  {:optional
                   {:prompt "Take 1 bad publicity?"
                    :async true
                    :yes-ability {:effect (req (wait-for (gain-bad-publicity state :corp 1)
                                                         (system-msg state :corp "used Broad Daylight to take 1 bad publicity")
                                                         (add-counters state side card eid)))}
                    :no-ability {:effect (effect (add-counters card eid))}}}
                  card nil))
      :abilities [{:cost [:click 1 :agenda 1]
                   :async true
                   :label "Do 2 meat damage"
                   :once :per-turn
                   :msg "do 2 meat damage"
                   :effect (effect (damage eid :meat 2 {:card card}))}]})

   "CFC Excavation Contract"
   {:effect (req (let [bios (count (filter #(has-subtype? % "Bioroid") (all-active-installed state :corp)))
                       bucks (* bios 2)]
                   (gain-credits state side bucks)
                   (system-msg state side (str "gains " bucks " [Credits] from CFC Excavation Contract"))))}

   "Character Assassination"
   {:prompt "Select a resource to trash"
    :choices {:req #(and (installed? %)
                         (resource? %))}
    :msg (msg "trash " (:title target))
    :interactive (req true)
    :async true
    :effect (effect (trash eid target {:unpreventable true}))}

   "Chronos Project"
   {:msg "remove all cards in the Runner's Heap from the game"
    :interactive (req true)
    :effect (effect (move-zone :runner :discard :rfg))}

   "City Works Project"
   (letfn [(meat-damage [s c] (+ 2 (get-counters (get-card s c) :advancement)))]
     {:install-state :face-up
      :access {:req (req installed)
               :msg (msg "do " (meat-damage state card) " meat damage")
               :async true
               :effect (effect (damage eid :meat (meat-damage state card) {:card card}))}})

   "Clone Retirement"
   {:msg "remove 1 bad publicity"
    :effect (effect (lose-bad-publicity 1))
    :silent (req true)
    :stolen {:msg "force the Corp to take 1 bad publicity"
             :effect (effect (gain-bad-publicity :corp 1))}}

   "Corporate Sales Team"
   (let [e {:effect (req (when (pos? (get-counters card :credit))
                           (gain-credits state :corp 1)
                           (system-msg state :corp (str "uses Corporate Sales Team to gain 1 [Credits]"))
                           (add-counter state side card :credit -1)))}]
     {:effect (effect (add-counter card :credit 10))
      :silent (req true)
      :events {:runner-turn-begins e
               :corp-turn-begins e}})

   "Corporate War"
   {:msg (msg (if (> (:credit corp) 6) "gain 7 [Credits]" "lose all credits"))
    :interactive (req true)
    :effect (req (if (> (:credit corp) 6)
                   (gain-credits state :corp 7) (lose-credits state :corp :all)))}

   "Crisis Management"
   (let [ability {:req (req tagged)
                  :async true
                  :label "Do 1 meat damage (start of turn)"
                  :once :per-turn
                  :msg "do 1 meat damage"
                  :effect (effect (damage eid :meat 1 {:card card}))}]
     {:events {:corp-turn-begins ability}
      :abilities [ability]})

   "Cyberdex Sandbox"
   {:effect (effect (continue-ability
                      {:optional {:prompt "Purge virus counters with Cyberdex Sandbox?"
                                  :yes-ability {:msg (msg "purge virus counters")
                                                :effect (effect (purge))}}}
                      card nil))
    :events {:purge {:once :per-turn
                     :msg "gain 4 [Credits]"
                     :effect (req (gain-credits state :corp 4))}}}

   "Dedicated Neural Net"
   (let [psi-effect
         {:async true
          :mandatory true
          :effect (req (if (not-empty (:hand corp))
                         (do (show-wait-prompt state :runner "Corp to select cards in HQ to be accessed")
                             (continue-ability
                               state :corp
                               {:prompt (msg "Select " (access-count state side :hq-access) " cards in HQ for the Runner to access")
                                :choices {:req #(and (in-hand? %) (corp? %))
                                          :max (req (access-count state side :hq-access))}
                                :effect (effect (clear-wait-prompt :runner)
                                                (continue-ability
                                                  :runner
                                                  (access-helper-hq
                                                    state (access-count state side :hq-access)
                                                    ; access-helper-hq uses a set to keep track of which cards have already
                                                    ; been accessed. Using the set difference we make the runner unable to
                                                    ; access non-selected cards from the corp prompt
                                                    (clojure.set/difference (set (:hand corp)) (set targets)))
                                                  card nil))}
                               card nil))
                         (effect-completed state side eid)))}]
     {:events {:successful-run {:interactive (req true)
                                :psi {:req (req (= target :hq))
                                      :once :per-turn
                                      :not-equal {:effect (req (when-not (:replace-access (get-in @state [:run :run-effect]))
                                                                 (swap! state update-in [:run :run-effect]
                                                                        #(assoc % :replace-access psi-effect)))
                                                               (effect-completed state side eid))}}}}})

   "Degree Mill"
   {:steal-cost-bonus (req [:shuffle-installed-to-stack 2])}

   "Director Haas' Pet Project"
   (letfn [(install-ability [server-name n]
             {:prompt "Select a card to install"
              :show-discard true
              :choices {:req #(and (corp? %)
                                   (not (operation? %))
                                   (#{[:hand] [:discard]} (:zone %)))}
              :effect (req (corp-install state side target server-name {:ignore-all-cost true})
                           (if (< n 2)
                             (continue-ability state side
                                               (install-ability (last (get-remote-names state)) (inc n))
                                               card nil)
                             (effect-completed state side eid)))
              :msg (msg (corp-install-msg target)
                        (when (zero? n)
                          ", creating a new remote server")
                        ", ignoring all install costs")})]
     {:optional {:prompt "Install cards in a new remote server?"
                 :yes-ability (install-ability "New remote" 0)}})

   "Divested Trust"
   {:events
    {:agenda-stolen
     {:async true
      :interactive (req true)
      :effect (req (if (:winner @state)
                     (effect-completed state side eid)
                     (let [stolen-agenda target
                           title (:title stolen-agenda)
                           prompt (str "Forfeit Divested Trust to add " title
                                       " to HQ and gain 5[Credits]?")
                           message (str "add " title " to HQ and gain 5 [Credits]")]
                       (show-wait-prompt state :runner "Corp to use Divested Trust")
                       (continue-ability
                         state side
                         {:optional
                          {:prompt prompt
                           :yes-ability
                           {:msg message
                            :effect (effect (forfeit card)
                                            (move stolen-agenda :hand)
                                            (gain-agenda-point :runner (- (:agendapoints stolen-agenda)))
                                            (gain-credits 5)
                                            (effect-completed eid))}
                           :end-effect (effect (clear-wait-prompt :runner))}}
                         card nil))))}}}

   "Domestic Sleepers"
   {:agendapoints-runner (req 0)
    :abilities [{:cost [:click 3] :msg "place 1 agenda counter on Domestic Sleepers"
                 :req (req (not (:counter card)))
                 :effect (effect (gain-agenda-point 1)
                                 (set-prop card :counter {:agenda 1} :agendapoints 1))}]}

   "Eden Fragment"
   {:events {:pre-corp-install
             {:req (req (and (ice? target)
                             (empty? (let [cards (map first (turn-events state side :corp-install))]
                                       (filter ice? cards)))))
              :effect (effect (ignore-install-cost true))}
             :corp-install
             {:req (req (and (ice? target)
                             (empty? (let [cards (map first (turn-events state side :corp-install))]
                                       (filter ice? cards)))))
              :msg (msg "ignore the install cost of the first ICE this turn")}}}

   "Efficiency Committee"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:cost [:click 1 :agenda 1]
                 :effect (effect (gain :click 2)
                                 (register-turn-flag!
                                   card :can-advance
                                   (fn [state side card]
                                     ((constantly false)
                                       (toast state :corp "Cannot advance cards this turn due to Efficiency Committee." "warning")))))
                 :msg "gain [Click][Click]"}]}

   "Elective Upgrade"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 2))
    :abilities [{:cost [:click 1 :agenda 1]
                 :once :per-turn
                 :effect (effect (gain :click 2))
                 :msg "gain [Click][Click]"}]}

   "Encrypted Portals"
   (ice-boost-agenda "Code Gate")

   "Escalate Vitriol"
   {:abilities [{:label "Gain 1 [Credit] for each Runner tag"
                 :cost [:click 1]
                 :once :per-turn
                 :msg (msg "gain " (count-tags state) " [Credits]")
                 :effect (effect (gain-credits (count-tags state)))}]}

   "Executive Retreat"
   {:effect (effect (add-counter card :agenda 1)
                    (shuffle-into-deck :hand))
    :interactive (req true)
    :abilities [{:cost [:click 1 :agenda 1]
                 :msg "draw 5 cards"
                 :effect (effect (draw 5))}]}

   "Explode-a-palooza"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (effect (show-wait-prompt :runner "Corp to use Explode-a-palooza")
                             (continue-ability
                               {:optional {:prompt "Gain 5 [Credits] with Explode-a-palooza ability?"
                                           :yes-ability {:msg "gain 5 [Credits]"
                                                         :effect (effect (gain-credits :corp 5)
                                                                         (clear-wait-prompt :runner))}
                                           :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil))}}

   "False Lead"
   {:abilities [{:req (req (<= 2 (:click runner)))
                 :msg "force the Runner to lose [Click][Click]"
                 :cost [:forfeit-self]
                 :effect (effect (lose :runner :click 2))}]}

   "Fetal AI"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :req (req (not= (first (:zone card)) :discard))
             :msg "do 2 net damage"
             :effect (effect (damage eid :net 2 {:card card}))}
    :steal-cost-bonus (req [:credit 2])}

   "Firmware Updates"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:cost [:agenda 1]
                 :choices {:req #(and (ice? %)
                                      (can-be-advanced? %))}
                 :req (req (pos? (get-counters card :agenda)))
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :once :per-turn
                 :effect (effect (add-prop target :advance-counter 1))}]}

   "Fly on the Wall"
   {:msg "give the runner 1 tag"
    :async true
    :effect (req (gain-tags state :runner eid 1))}

   "Genetic Resequencing"
   {:choices {:req #(= (last (:zone %)) :scored)}
    :msg (msg "add 1 agenda counter on " (:title target))
    :effect (effect (add-counter target :agenda 1))
    :silent (req true)}

   "Geothermal Fracking"
   {:effect (effect (add-counter card :agenda 2))
    :silent (req true)
    :abilities [{:cost [:click 1 :agenda 1]
                 :msg "gain 7 [Credits] and take 1 bad publicity"
                 :effect (effect (gain-credits 7)
                                 (gain-bad-publicity :corp 1))}]}

   "Gila Hands Arcology"
   {:abilities [{:cost [:click 2]
                 :msg "gain 3 [Credits]"
                 :effect (effect (gain-credits 3))}]}

   "Glenn Station"
   {:implementation "Doesn't prohibit hosting multiple cards"
    :abilities [{:label "Host a card from HQ on Glenn Station"
                 :cost [:click 1]
                 :msg "host a card from HQ"
                 :prompt "Choose a card to host on Glenn Station"
                 :choices (req (:hand corp))
                 :effect (effect (host card target {:facedown true}))}
                {:label "Add a card on Glenn Station to HQ"
                 :cost [:click 1]
                 :msg "add a hosted card to HQ"
                 :prompt "Choose a card on Glenn Station"
                 :choices (req (:hosted card))
                 :effect (effect (move target :hand))}]}

   "Global Food Initiative"
   {:agendapoints-runner (req 2)}

   "Government Contracts"
   {:abilities [{:cost [:click 2]
                 :effect (effect (gain-credits 4))
                 :msg "gain 4 [Credits]"}]}

   "Government Takeover"
   {:abilities [{:cost [:click 1]
                 :effect (effect (gain-credits 3))
                 :msg "gain 3 [Credits]"}]}

   "Graft"
   (letfn [(graft [n] {:prompt "Choose a card to add to HQ with Graft"
                       :async true
                       :choices (req (cancellable (:deck corp) :sorted))
                       :msg (msg "add " (:title target) " to HQ from R&D")
                       :cancel-effect (req (shuffle! state side :deck)
                                           (system-msg state side (str "shuffles R&D"))
                                           (effect-completed state side eid))
                       :effect (req (move state side target :hand)
                                    (if (< n 3)
                                      (continue-ability state side (graft (inc n)) card nil)
                                      (do (shuffle! state side :deck)
                                          (system-msg state side (str "shuffles R&D"))
                                          (effect-completed state side eid))))})]
     {:async true
      :msg "add up to 3 cards from R&D to HQ"
      :effect (effect (continue-ability (graft 1) card nil))})

   "Hades Fragment"
   {:flags {:corp-phase-12 (req (and (not-empty (get-in @state [:corp :discard]))
                                     (is-scored? state :corp card)))}
    :abilities [{:prompt "Select a card to add to the bottom of R&D"
                 :show-discard true
                 :choices {:req #(and (corp? %)
                                      (in-discard? %))}
                 :effect (effect (move target :deck))
                 :msg (msg "add "
                           (if (:seen target)
                             (:title target)
                             "a card")
                           " to the bottom of R&D")}]}

   "Helium-3 Deposit"
   {:async true
    :interactive (req true)
    :prompt "How many power counters?"
    :choices ["0" "1" "2"]
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:req #(pos? (get-counters % :power))}
                      :msg (msg "add " c " power counters on " (:title target))
                      :effect (effect (add-counter target :power c))}
                     card nil)))}

   "High-Risk Investment"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:click 1 :agenda 1]
                 :msg (msg "gain " (:credit runner) " [Credits]")
                 :effect (effect (gain-credits (:credit runner)))}]}

   "Hollywood Renovation"
   {:install-state :face-up
    :events {:advance
             {:async true
              :req (req (same-card? card target))
              :effect (req (let [n (if (>= (get-counters (get-card state card) :advancement) 6) 2 1)]
                             (continue-ability
                               state side
                               {:choices {:req #(and (not (same-card? % card))
                                                     (can-be-advanced? %))}
                                :msg (msg "place " n
                                          " advancement tokens on "
                                          (card-str state target))
                                :effect (effect (add-prop :corp target :advance-counter n {:placed true}))}
                               card nil)))}}}

   "Hostile Takeover"
   {:msg "gain 7 [Credits] and take 1 bad publicity"
    :effect (effect (gain-credits 7)
                    (gain-bad-publicity :corp 1))
    :interactive (req true)}

   "House of Knives"
   {:effect (effect (add-counter card :agenda 3))
    :silent (req true)
    :abilities [{:cost [:agenda 1]
                 :msg "do 1 net damage"
                 :req (req (:run @state))
                 :once :per-run
                 :effect (effect (damage eid :net 1 {:card card}))}]}

   "Hyperloop Extension"
   (let [he (req (gain-credits state :corp 3)
                 (system-msg state side (str "uses Hyperloop Extension to gain 3 [Credits]")))]
     {:effect he
      :stolen {:effect he}})

   "Ikawah Project"
   {:steal-cost-bonus (req [:credit 2 :click 1])}

   "Illicit Sales"
   {:async true
    :effect (req (wait-for (resolve-ability
                             state side
                             {:optional
                              {:prompt "Take 1 bad publicity from Illicit Sales?"
                               :yes-ability {:msg "take 1 bad publicity"
                                             :effect (effect (gain-bad-publicity :corp 1))}}}
                             card nil)
                           (let [n (* 3 (count-bad-pub state))]
                             (gain-credits state side n)
                             (system-msg state side (str "gains " n " [Credits] from Illicit Sales"))
                             (effect-completed state side eid))))}

   "Improved Protein Source"
   {:msg "make the Runner gain 4 [Credits]"
    :effect (effect (gain-credits :runner 4))
    :interactive (req true)
    :stolen {:msg "make the Runner gain 4 [Credits]"
             :effect (effect (gain-credits :runner 4))}}

   "Improved Tracers"
   {:silent (req true)
    :effect (req (update-all-ice state side))
    :swapped {:effect (req (update-all-ice state side))}
    :events {:pre-ice-strength {:req (req (has-subtype? target "Tracer"))
                                :effect (effect (ice-strength-bonus 1 target))}
             :pre-init-trace {:req (req (and (has-subtype? target "Tracer")
                                             (= :subroutine (:source-type (second targets)))))
                              :effect (effect (init-trace-bonus 1))}}}

   "Jumon"
   {:events
    {:corp-turn-ends
     {:req (req (some #(and (= (last (:zone %)) :content)
                            (is-remote? (second (:zone %))))
                      (all-installed state :corp)))
      :prompt "Select a card to place 2 advancement tokens on"
      :player :corp
      :choices {:req #(and (= (last (:zone %)) :content)
                           (is-remote? (second (:zone %))))}
      :msg (msg "place 2 advancement token on " (card-str state target))
      :effect (effect (add-prop :corp target :advance-counter 2 {:placed true}))}}}

   "Labyrinthine Servers"
   {:interactions {:prevent [{:type #{:jack-out}
                              :req (req (pos? (get-counters card :power)))}]}
    :silent (req true)
    :effect (effect (add-counter card :power 2))
    :abilities [{:req (req (:run @state))
                 :cost [:power 1]
                 :msg "prevent the Runner from jacking out"
                 :effect (effect (jack-out-prevent))}]}

   "License Acquisition"
   {:interactive (req true)
    :prompt "Select an asset or upgrade to install from Archives or HQ"
    :show-discard true
    :choices {:req #(and (#{"Asset" "Upgrade"} (:type %))
                         (#{[:hand] [:discard]} (:zone %))
                         (corp? %))}
    :msg (msg "install and rez " (:title target) ", ignoring all costs")
    :effect (effect (corp-install eid target nil {:install-state :rezzed-no-cost}))}

   "Mandatory Seed Replacement"
   (letfn [(msr [] {:prompt "Select two pieces of ICE to swap positions"
                    :choices {:req #(and (installed? %)
                                         (ice? %))
                              :max 2}
                    :async true
                    :effect (req (if (= (count targets) 2)
                                   (do (swap-ice state side (first targets) (second targets))
                                       (system-msg state side
                                                   (str "swaps the position of "
                                                        (card-str state (first targets))
                                                        " and "
                                                        (card-str state (second targets))))
                                       (continue-ability state side (msr) card nil))
                                   (do (system-msg state :corp (str "has finished rearranging ICE"))
                                       (effect-completed state side eid))))})]
     {:async true
      :msg "rearrange any number of ICE"
      :effect (effect (continue-ability (msr) card nil))})

   "Mandatory Upgrades"
   {:msg "gain an additional [Click] per turn"
    :silent (req true)
    :effect (req (gain state :corp
                       :click-per-turn 1))
    :swapped {:msg "gain an additional [Click] per turn"
              :effect (req (when (= (:active-player @state) :corp)
                             (gain state :corp :click 1))
                           (gain state :corp :click-per-turn 1))}
    :leave-play (req (lose state :corp
                           :click 1
                           :click-per-turn 1))}

   "Market Research"
   {:interactive (req true)
    :req (req tagged)
    :effect (effect (add-counter card :agenda 1)
                    (set-prop card :agendapoints 3))}

   "Medical Breakthrough"
   {:silent (req true)
    :effect (effect (update-all-advancement-costs))
    :stolen {:effect (effect (update-all-advancement-costs))}
    :advancement-cost-bonus (req (- (count (filter #(= (:title %) "Medical Breakthrough")
                                                   (concat (:scored corp) (:scored runner))))))}

   "Merger"
   {:agendapoints-runner (req 3)}

   "Meteor Mining"
   {:interactive (req true)
    :async true
    :prompt "Use Meteor Mining?"
    :choices (req (if (< (count-tags state) 2)
                    ["Gain 7 [Credits]" "No action"]
                    ["Gain 7 [Credits]" "Do 7 meat damage" "No action"]))
    :effect (req (case target
                   "Gain 7 [Credits]"
                   (do (gain-credits state side 7)
                       (system-msg state side "uses Meteor Mining to gain 7 [Credits]")
                       (effect-completed state side eid))
                   "Do 7 meat damage"
                   (do (damage state side eid :meat 7 {:card card})
                       (system-msg state side "uses Meteor Mining do 7 meat damage"))
                   "No action"
                   (do (system-msg state side "does not use Meteor Mining")
                       (effect-completed state side eid))))}

   "NAPD Contract"
   {:steal-cost-bonus (req [:credit 4])
    :advancement-cost-bonus (req (count-bad-pub state))}

   "Net Quarantine"
   (let [nq {:effect (req (let [extra (int (/ (:runner-spent target) 2))]
                            (when (pos? extra)
                              (gain-credits state side extra)
                              (system-msg state :corp (str "uses Net Quarantine to gain " extra "[Credits]")))))}]
     {:events {:pre-init-trace {:once :per-turn
                                :silent (req true)
                                :effect (req (system-msg state :corp "uses Net Quarantine to reduce Runner's base link to zero")
                                             (swap! state assoc-in [:trace :force-link] 0))}
               :successful-trace nq
               :unsuccessful-trace nq}})

   "New Construction"
   {:install-state :face-up
    :events {:advance
             {:optional
              {:req (req (same-card? card target))
               :prompt "Install a card from HQ in a new remote?"
               :yes-ability {:prompt "Select a card to install"
                             :choices {:req #(and (not (operation? %))
                                                  (not (ice? %))
                                                  (corp? %)
                                                  (in-hand? %))}
                             :msg (msg "install a card from HQ"
                                       (when (>= (get-counters (get-card state card) :advancement) 5)
                                         " and rez it, ignoring all costs"))
                             :effect (req (if (>= (get-counters (get-card state card) :advancement) 5)
                                            (do (corp-install state side target "New remote"
                                                              {:install-state :rezzed-no-cost})
                                                (trigger-event state side :rez target))
                                            (corp-install state side target "New remote")))}}}}}

   "NEXT Wave 2"
   {:not-when-scored true
    :req (req (some #(and (rezzed? %)
                          (ice? %)
                          (has-subtype? % "NEXT"))
                    (all-installed state :corp)))
    :optional {:prompt "Do 1 brain damage with NEXT Wave 2?"
               :yes-ability {:msg "do 1 brain damage"
                             :effect (effect (damage eid :brain 1 {:card card}))}}}

   "Nisei MK II"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 1))
    :abilities [{:req (req (:run @state))
                 :cost [:agenda 1]
                 :msg "end the run"
                 :async true
                 :effect (effect (end-run eid card))}]}

   "Oaktown Renovation"
   {:install-state :face-up
    :events {:advance {:req (req (same-card? card target))
                       :msg (msg "gain " (if (>= (get-counters (get-card state card) :advancement) 5) "3" "2") " [Credits]")
                       :effect (req (gain-credits state side
                                                  (if (>= (get-counters (get-card state card) :advancement) 5) 3 2)))}}}

   "Obokata Protocol"
   {:steal-cost-bonus (req [:net 4])}

   "Paper Trail"
   {:trace {:base 6
            :successful {:msg "trash all connection and job resources"
                         :effect (req (doseq [resource (filter #(or (has-subtype? % "Job")
                                                                    (has-subtype? % "Connection"))
                                                               (all-active-installed state :runner))]
                                        (trash state side resource)))}}}

   "Personality Profiles"
   (let [pp {:req (req (pos? (count (:hand runner))))
             :effect (effect
                       (continue-ability
                         (let [c (first (shuffle (:hand runner)))]
                           {:msg (msg "force the Runner to trash " (:title c) " from their Grip at random")
                            :effect (effect (trash eid c nil))})
                         card nil))}]
     {:events {:searched-stack pp
               :runner-install (assoc pp :req (req (and (some #{:discard} (:previous-zone target))
                                                        (pos? (count (:hand runner))))))}})

   "Philotic Entanglement"
   {:interactive (req true)
    :req (req (pos? (count (:scored runner))))
    :msg (msg "do " (count (:scored runner)) " net damage")
    :effect (effect (damage eid :net (count (:scored runner)) {:card card}))}

   "Posted Bounty"
   {:optional {:prompt "Forfeit Posted Bounty to give the Runner 1 tag and take 1 bad publicity?"
               :yes-ability {:msg "give the Runner 1 tag and take 1 bad publicity"
                             :async true
                             :effect (effect (gain-bad-publicity :corp eid 1)
                                             (gain-tags :corp eid 1)
                                             (forfeit card))}}}

   "Priority Requisition"
   {:interactive (req true)
    :choices {:req #(and (ice? %)
                         (not (rezzed? %))
                         (installed? %))}
    :effect (effect (rez target {:ignore-cost :all-costs}))}

   "Private Security Force"
   {:abilities [{:req (req tagged)
                 :cost [:click 1]
                 :effect (effect (damage eid :meat 1 {:card card}))
                 :msg "do 1 meat damage"}]}

   "Profiteering"
   {:interactive (req true)
    :choices ["0" "1" "2" "3"]
    :prompt "How many bad publicity?"
    :msg (msg "take " target " bad publicity and gain " (* 5 (str->int target)) " [Credits]")
    :effect (req (let [bp (count-bad-pub state)]
                   (gain-bad-publicity state :corp eid (str->int target))
                   (if (< bp (count-bad-pub state))
                     (gain-credits state :corp (* 5 (str->int target))))))}

   "Project Ares"
   (letfn [(trash-count-str [card]
             (quantify (- (get-counters card :advancement) 4) "installed card"))]
     {:silent (req true)
      :req (req (and (> (get-counters card :advancement) 4)
                     (pos? (count (all-installed state :runner)))))
      :msg (msg "force the Runner to trash " (trash-count-str card) " and take 1 bad publicity")
      :async true
      :effect (effect (show-wait-prompt :corp "Runner to trash installed cards")
                      (continue-ability
                        :runner
                        {:prompt (msg "Select " (trash-count-str card) " installed cards to trash")
                         :choices {:max (min (- (get-counters card :advancement) 4)
                                             (count (all-installed state :runner)))
                                   :req #(and (runner? %)
                                              (installed? %))}
                         :effect (effect (trash-cards targets)
                                         (system-msg (str "trashes " (join ", " (map :title targets))))
                                         (gain-bad-publicity :corp 1))}
                        card nil)
                      (clear-wait-prompt :corp))})

   "Project Atlas"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (max 0 (- (get-counters card :advancement) 3))))
    :abilities [{:cost [:agenda 1]
                 :prompt "Choose a card"
                 :label "Search R&D and add 1 card to HQ"
                 ;; we need the req or the prompt will still show
                 :req (req (pos? (get-counters card :agenda)))
                 :msg (msg "add " (:title target) " to HQ from R&D")
                 :choices (req (cancellable (:deck corp) :sorted))
                 :cancel-effect (effect (system-msg "cancels the effect of Project Atlas"))
                 :effect (effect (shuffle! :deck)
                                 (move target :hand))}]}

   "Project Beale"
   {:interactive (req true)
    :agendapoints-runner (req 2)
    :effect (req (let [n (quot (- (get-counters card :advancement) 3) 2)]
                   (set-prop state side card
                             :counter {:agenda n}
                             :agendapoints (+ 2 n))))}

   "Project Kusanagi"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (- (get-counters card :advancement) 2)))
    :events {:run-ends
             {:effect (req (let [cid (:cid card)
                                 ices (get-in card [:special :kusanagi])]
                             (doseq [i ices]
                               (when-let [ice (get-card state i)]
                                 (remove-sub! state side ice #(= cid (:from-cid %))))))
                           (update! state side (dissoc-in card [:special :kusanagi])))}}
    :abilities [{:label "Give a piece of ICE \"[Subroutine] Do 1 net damage\""
                 :prompt "Choose a piece of ICE"
                 :choices {:req #(and (ice? %)
                                      (rezzed? %))}
                 :cost [:agenda 1]
                 :msg (str "make a piece of ICE gain \"[Subroutine] Do 1 net damage\" "
                           "after all its other subroutines for the remainder of the run")
                 :effect  (effect (add-extra-sub! (get-card state target)
                                                  (do-net-damage 1)
                                                  (:cid card) {:back true})
                                  (update! (update-in card [:special :kusanagi] #(conj % target))))}]}

   "Project Vitruvius"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (- (get-counters card :advancement) 3)))
    :abilities [{:cost [:agenda 1]
                 :prompt "Choose a card in Archives to add to HQ"
                 :show-discard true
                 :choices {:req #(and (in-discard? %)
                                      (corp? %))}
                 :req (req (pos? (get-counters card :agenda)))
                 :msg (msg "add "
                           (if (:seen target)
                             (:title target) "an unseen card ")
                           " to HQ from Archives")
                 :effect (effect (move target :hand))}]}

   "Project Wotan"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :events {:run-ends
             {:effect (req (let [cid (:cid card)
                                 ices (get-in card [:special :wotan])]
                             (doseq [i ices]
                               (when-let [ice (get-card state i)]
                                 (remove-sub! state side ice #(= cid (:from-cid %))))))
                           (update! state side (dissoc-in card [:special :wotan])))}}
    :abilities [{:req (req (and (ice? current-ice)
                                (rezzed? current-ice)
                                (has-subtype? current-ice "Bioroid")))
                 :cost [:agenda 1]
                 :msg (str "make the approached piece of Bioroid ICE gain \"[Subroutine] End the run\""
                           "after all its other subroutines for the remainder of this run")
                 :effect  (effect (add-extra-sub! (get-card state current-ice)
                                                  {:label "End the run"
                                                   :msg "end the run"
                                                   :async true
                                                   :effect (effect (end-run eid card))}
                                                  (:cid card) {:back true})
                                  (update! (update-in card [:special :wotan] #(conj % current-ice))))}]}

   "Project Yagi-Uda"
   (letfn [(put-back-counter [state side card]
             (set-prop state side card :counter
                       (merge
                         (:counter card)
                         {:agenda (+ 1 (get-counters card :agenda))})))
           (choose-swap [to-swap]
             {:prompt (str "Select a card in HQ to swap with " (:title to-swap))
              :choices {:not-self true
                        :req #(and (corp? %)
                                   (in-hand? %)
                                   (if (ice? to-swap)
                                     (ice? %)
                                     (or (agenda? %)
                                         (asset? %)
                                         (upgrade? %))))}
              :msg (msg "swap " (card-str state to-swap) " with a card from HQ")
              :effect (req (move state :corp to-swap (:zone target) {:keep-server-alive true})
                           (move state :corp target (:zone to-swap) {:keep-server-alive true})
                           (clear-wait-prompt state :runner))
              :cancel-effect (effect (put-back-counter card)
                                     (clear-wait-prompt :runner))})
           (choose-card [run-server]
             {:prompt "Choose a card in or protecting the attacked server."
              :choices {:req #(= (first run-server) (second (:zone %)))}
              :effect (effect (continue-ability (choose-swap target) card nil))
              :cancel-effect (effect (put-back-counter card)
                                     (clear-wait-prompt :runner))})]
     {:silent (req true)
      :effect (effect (add-counter card :agenda (- (get-counters card :advancement) 3)))
      :abilities [{:cost [:agenda 1]
                   :req (req run)
                   :effect (effect (show-wait-prompt :runner "Corp to use Project Yagi-Uda")
                             (continue-ability (choose-card (:server run))
                                               card nil))}]})
   "Puppet Master"
   {:events {:successful-run
             {:interactive (req true)
              :async true
              :effect (req (show-wait-prompt state :runner "Corp to use Puppet Master")
                           (continue-ability
                             state :corp
                             {:prompt "Select a card to place 1 advancement token on"
                              :player :corp
                              :choices {:req can-be-advanced?}
                              :cancel-effect (effect (clear-wait-prompt :runner)
                                                     (effect-completed eid))
                              :msg (msg "place 1 advancement token on " (card-str state target))
                              :effect (effect (add-prop :corp target :advance-counter 1 {:placed true})
                                              (clear-wait-prompt :runner))} card nil))}}}

   "Quantum Predictive Model"
   {:flags {:rd-reveal (req true)}
    :access {:req (req tagged)
             :async true
             :interactive (req true)
             :effect (req (wait-for (as-agenda state side card 1)
                                    (continue-ability
                                      state :runner
                                      {:prompt "Quantum Predictive Model was added to the corp's score area"
                                       :choices ["OK"]}
                                      card nil)))
             :msg "add it to their score area and gain 1 agenda point"}}

   "Rebranding Team"
   (letfn [(get-assets [state corp]
             (filter asset? (concat (all-installed state :corp)
                                    (:deck corp)
                                    (:hand corp)
                                    (:discard corp))))
           (add-ad [state side c]
             (update! state side (assoc-in c [:persistent :subtype] "Advertisement")))]
     {:interactive (req true)
      :msg "make all assets gain Advertisement"
      :effect (req (doseq [c (get-assets state corp)] (add-ad state side c)))
      :swapped {:msg "make all assets gain Advertisement"
                :effect (req (doseq [c (get-assets state corp)] (add-ad state side c)))}
      :leave-play (req (doseq [c (get-assets state corp)]
                         (update! state side (assoc-in c [:persistent :subtype]
                                                       (->> (split (or (-> c :persistent :subtype) "") #" - ")
                                                            (drop 1) ;so that all actual ads remain ads if agenda leaves play
                                                            (join " - "))))))})

   "Reeducation"
   (letfn [(corp-final [chosen original]
             {:prompt (str "The bottom cards of R&D will be " (join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :async true
              :msg (req (let [n (count chosen)]
                          (str "add " n " cards from HQ to the bottom of R&D and draw " n " cards."
                               " The Runner randomly adds " (if (<= n (count (:hand runner))) n 0)
                               " cards from their Grip to the bottom of the Stack")))
              :effect (req (let [n (count chosen)]
                             (if (= target "Done")
                               (do (doseq [c (reverse chosen)] (move state :corp c :deck))
                                   (draw state :corp n)
                                   ; if corp chooses more cards than runner's hand, don't shuffle runner hand back into Stack
                                   (when (<= n (count (:hand runner)))
                                     (doseq [r (take n (shuffle (:hand runner)))] (move state :runner r :deck)))
                                   (clear-wait-prompt state :runner)
                                   (effect-completed state side eid))
                               (continue-ability state side (corp-choice original '() original) card nil))))})
           (corp-choice [remaining chosen original] ; Corp chooses cards until they press 'Done'
             {:prompt "Choose a card to move to bottom of R&D"
              :choices (conj (vec remaining) "Done")
              :async true
              :effect (req (let [chosen (cons target chosen)]
                             (if (not= target "Done")
                               (continue-ability
                                 state side
                                 (corp-choice (remove-once #(= target %) remaining) chosen original)
                                 card nil)
                               (if (pos? (count (remove #(= % "Done") chosen)))
                                 (continue-ability state side (corp-final (remove #(= % "Done") chosen) original) card nil)
                                 (do (system-msg state side "does not add any cards from HQ to bottom of R&D")
                                     (clear-wait-prompt state :runner)
                                     (effect-completed state side eid))))))})]
     {:async true
      :effect (req (show-wait-prompt state :runner "Corp to add cards from HQ to bottom of R&D")
                   (let [from (get-in @state [:corp :hand])]
                     (if (pos? (count from))
                       (continue-ability state :corp (corp-choice from '() from) card nil)
                       (do (system-msg state side "does not add any cards from HQ to bottom of R&D")
                           (effect-completed state side eid)))))})

   "Remastered Edition"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:agenda 1]
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req installed?}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Remote Data Farm"
   {:silent (req true)
    :msg "increase their maximum hand size by 2"
    :effect (effect (gain :hand-size 2))
    :swapped {:msg "increase their maximum hand size by 2"
              :effect (effect (gain :hand-size 2))}
    :leave-play (effect (lose :hand-size 2))}

   "Remote Enforcement"
   {:interactive (req true)
    :optional {:prompt "Search R&D for a piece of ice to install protecting a remote server?"
               :yes-ability
               {:async true
                :effect (req (when (not-empty (filter ice? (:deck corp)))
                               (continue-ability
                                 state side
                                 {:async true
                                  :prompt "Choose a piece of ice"
                                  :choices (req (filter ice? (:deck corp)))
                                  :effect (req (let [chosen-ice target]
                                                 (continue-ability
                                                   state side
                                                   {:async true
                                                    :prompt (str "Select a server to install " (:title chosen-ice) " on")
                                                    :choices (filter #(not (#{"HQ" "Archives" "R&D"} %))
                                                                     (corp-install-list state chosen-ice))
                                                    :effect (effect (shuffle! :deck)
                                                                    (corp-install eid chosen-ice target
                                                                                  {:install-state :rezzed-no-rez-cost}))}
                                                   card nil)))}
                                 card nil)))}}}

   "Research Grant"
   {:interactive (req true)
    :silent (req (empty? (filter #(= (:title %) "Research Grant") (all-installed state :corp))))
    :async true
    :effect (effect (continue-ability
                      {:prompt "Select another installed copy of Research Grant to score"
                       :choices {:req #(= (:title %) "Research Grant")}
                       :interactive (req true)
                       :async true
                       :req (req (not (empty? (filter #(= (:title %) "Research Grant") (all-installed state :corp)))))
                       :effect (effect (set-prop target :advance-counter (:advancementcost target))
                                       (score eid (get-card state target)))
                       :msg "score another installed copy of Research Grant"}
                      card nil))}

   "Restructured Datapool"
   {:abilities [{:cost [:click 1]
                 :trace {:base 2
                         :successful {:msg "give the Runner 1 tag"
                                      :async true
                                      :effect (effect (gain-tags eid 1))}}}]}

   "SDS Drone Deployment"
   {:steal-cost-bonus (req [:program 1])
    :effect (req (show-wait-prompt state :runner "Corp to use SDS Drone Deployment")
                 (if (seq (all-installed-runner-type state :program))
                   (continue-ability
                     state side
                     {:prompt "Select a program to trash"
                      :label "Trash a program"
                      :msg (msg "trash " (:title target))
                      :choices {:req #(and (installed? %)
                                           (program? %))}
                      :effect (effect (trash target)
                                      (clear-wait-prompt :runner))
                      :end-effect (effect (clear-wait-prompt :runner))}
                     card nil)
                   (clear-wait-prompt state :runner)))}

   "Self-Destruct Chips"
   {:silent (req true)
    :msg "decrease the Runner's maximum hand size by 1"
    :effect (effect (lose :runner :hand-size 1))
    :swapped {:msg "decrease the Runner's maximum hand size by 1"
              :effect (effect (lose :runner :hand-size 1))}
    :leave-play (effect (gain :runner :hand-size 1))}

   "Sensor Net Activation"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:agenda 1]
                 :req (req (some #(and (has-subtype? % "Bioroid") (not (rezzed? %))) (all-installed state :corp)))
                 :prompt "Choose a bioroid to rez, ignoring all costs"
                 :choices {:req #(and (has-subtype? % "Bioroid") (not (rezzed? %)))}
                 :msg (msg "rez " (card-str state target) ", ignoring all costs")
                 :effect (req (let [c target]
                                (rez state side c {:ignore-cost :all-costs})
                                (register-events
                                  state side
                                  {:corp-turn-ends {:effect (effect (derez c)
                                                                    (unregister-events card))}
                                   :runner-turn-ends {:effect (effect (derez c)
                                                                      (unregister-events card))}} card)))}]
    :events {:corp-turn-ends nil
             :runner-turn-ends nil}}

   "Sentinel Defense Program"
   {:events {:pre-resolve-damage {:req (req (and (= target :brain)
                                                 (pos? (last targets))))
                                  :msg "do 1 net damage"
                                  :effect (effect (damage eid :net 1 {:card card}))}}}

   "Show of Force"
   {:async true
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))}

   "SSL Endorsement"
   (let [add-credits (effect (add-counter card :credit 9))
         remove-credits {:optional {:req (req (pos? (get-counters card :credit)))
                                    :once :per-turn
                                    :prompt "Gain 3 [Credits] from SSL Endorsement?"
                                    :autoresolve (get-autoresolve :auto-fire)
                                    :yes-ability
                                    {:effect (req (when (pos? (get-counters card :credit))
                                                    (gain-credits state :corp 3)
                                                    (system-msg state :corp (str "uses SSL Endorsement to gain 3 [Credits]"))
                                                    (add-counter state side card :credit -3)))}}}]
     {:effect add-credits
      :abilities [(set-autoresolve :auto-fire "whether to take credits off SSL")]
      :stolen {:effect add-credits}
      :interactive (req true)
      :events {:corp-turn-begins remove-credits}
      :flags {:has-events-when-stolen true}})

   "Standoff"
   (letfn [(stand [side]
             {:async true
              :prompt "Choose one of your installed cards to trash due to Standoff"
              :choices {:req #(and (installed? %)
                                   (same-side? side (:side %)))}
              :cancel-effect (req (if (= side :runner)
                                    (do (draw state :corp)
                                        (gain-credits state :corp 5)
                                        (clear-wait-prompt state :corp)
                                        (system-msg state :runner "declines to trash a card due to Standoff")
                                        (system-msg state :corp "draws a card and gains 5 [Credits] from Standoff")
                                        (effect-completed state :corp eid))
                                    (do (system-msg state :corp "declines to trash a card from Standoff")
                                        (clear-wait-prompt state :runner)
                                        (effect-completed state :corp eid))))
              :effect (req (wait-for (trash state side target {:unpreventable true})
                                     (do
                                       (system-msg state side (str "trashes " (card-str state target) " due to Standoff"))
                                       (clear-wait-prompt state (other-side side))
                                       (show-wait-prompt state side (str (side-str (other-side side)) " to trash a card for Standoff"))
                                       (continue-ability state (other-side side) (stand (other-side side)) card nil))))})]
     {:interactive (req true)
      :async true
      :effect (effect (show-wait-prompt (str (side-str (other-side side)) " to trash a card for Standoff"))
                      (continue-ability :runner (stand :runner) card nil))})

   "Sting!"
   (letfn [(count-opp-stings [state side]
             (count (filter #(= (:title %) "Sting!") (get-in @state [(other-side side) :scored]))))]
     {:msg (msg "deal " (inc (count-opp-stings state :corp)) " net damage")
      :async true
      :effect (effect (damage eid :net (inc (count-opp-stings state :corp)) {:card card}))
      :stolen {:msg (msg "deal " (inc (count-opp-stings state :runner)) " net damage")
               :async true
               :effect (effect (damage eid :net (inc (count-opp-stings state :runner)) {:card card}))}})

   "Successful Field Test"
   (letfn [(sft [n max] {:prompt "Select a card in HQ to install with Successful Field Test"
                         :priority -1
                         :async true
                         :choices {:req #(and (corp? %)
                                              (not (operation? %))
                                              (in-hand? %))}
                         :effect (req (wait-for
                                        (corp-install state side target nil {:ignore-all-cost true})
                                        (if (< n max)
                                          (continue-ability state side (sft (inc n) max) card nil)
                                          (effect-completed state side eid))))})]
     {:async true
      :msg "install cards from HQ, ignoring all costs"
      :effect (req (let [max (count (filter (complement operation?) (:hand corp)))]
                     (continue-ability state side (sft 1 max) card nil)))})

   "Superior Cyberwalls"
   (ice-boost-agenda "Barrier")

   "TGTBT"
   {:flags {:rd-reveal (req true)}
    :access {:msg "give the Runner 1 tag"
             :async true
             :effect (effect (gain-tags eid 1))}}

   "The Cleaners"
   {:events {:pre-damage {:req (req (and (= target :meat)
                                         (= side :corp)))
                          :msg "do 1 additional meat damage"
                          :effect (effect (damage-bonus :meat 1))}}}

   "The Future is Now"
   {:interactive (req true)
    :prompt "Choose a card to add to HQ"
    :choices (req (:deck corp))
    :msg (msg "add a card from R&D to HQ and shuffle R&D")
    :req (req (pos? (count (:deck corp))))
    :effect (effect (shuffle! :deck)
                    (move target :hand))}

   "The Future Perfect"
   {:flags {:rd-reveal (req true)}
    :access
    {:psi {:req (req (not installed))
           :not-equal {:msg "prevent it from being stolen"
                       :effect (effect (register-run-flag!
                                         card :can-steal
                                         (fn [_ _ c] (not (same-card? c card))))
                                       (effect-completed eid))}}}}

   "Timely Public Release"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 1))
    :abilities [{:cost [:agenda 1]
                 :label "Install a piece of ice in any position, ignoring all costs"
                 :prompt "Select a piece of ice to install"
                 :show-discard true
                 :choices {:req #(and (ice? %)
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                 :msg (msg "install "
                           (if (and (in-discard? target)
                                    (or (faceup? target)
                                        (not (facedown? target))))
                             (:title target)
                             "ICE")
                           " from " (zone->name (:zone target)))
                 :effect (effect
                           (continue-ability
                             (let [chosen-ice target]
                               {:prompt "Choose a server"
                                :choices (req servers)
                                :effect (effect
                                          (continue-ability
                                            (let [chosen-server target
                                                  num-ice (count (get-in (:corp @state)
                                                                         (conj (server->zone state target) :ices)))]
                                              {:prompt "Which position to install in? (0 is innermost)"
                                               :choices (vec (reverse (map str (range (inc num-ice)))))
                                               :effect (req (corp-install state side chosen-ice chosen-server
                                                                          {:ignore-all-cost true :index (Integer/parseInt target)})
                                                            (if (and run
                                                                     (= (zone->name (first (:server run)))
                                                                        chosen-server))
                                                              (let [curr-pos (get-in @state [:run :position])]
                                                                (if (>= curr-pos (Integer/parseInt target))
                                                                  (swap! state assoc-in [:run :position] (inc curr-pos))))))})
                                            card nil))})
                             card nil))}]}

   "Underway Renovation"
   (letfn [(adv4? [s c] (if (>= (get-counters (get-card s c) :advancement) 4) 2 1))]
     {:install-state :face-up
      :events {:advance {:req (req (same-card? card target))
                         :msg (msg (if (pos? (count (:deck runner)))
                                     (str "trash "
                                          (join ", " (map :title (take (adv4? state card) (:deck runner))))
                                          " from the Runner's stack")
                                     "trash from the Runner's stack but it is empty"))
                         :effect (effect (mill :corp :runner (adv4? state card)))}}})

   "Unorthodox Predictions"
   {:implementation "Prevention of subroutine breaking is not enforced"
    :prompt "Choose an ICE type for Unorthodox Predictions"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "prevent subroutines on " target " ICE from being broken until next turn.")
    :effect (effect (effect-completed eid))}

   "Utopia Fragment"
   {:events {:pre-steal-cost {:req (req (pos? (get-counters target :advancement)))
                              :effect (req (let [counter (get-counters target :advancement)]
                                             (steal-cost-bonus state side [:credit (* 2 counter)])))}}}

   "Vanity Project"
   ;; No special implementation
   {}

   "Veterans Program"
   {:interactive (req true)
    :msg "lose 2 bad publicity"
    :effect (effect (lose-bad-publicity 2))}

   "Viral Weaponization"
   (let [dmg {:msg "do 1 net damage for each card in the grip"
              :async true
              :effect (req (let [cnt (count (:hand runner))]
                             (unregister-events state side card)
                             (damage state side eid :net cnt {:card card})))}]
     {:effect (effect (register-events
                        {:corp-turn-ends dmg
                         :runner-turn-ends dmg}
                        card))
      :events {:corp-turn-ends nil
               :runner-turn-ends nil}})

   "Voting Machine Initiative"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :events {:runner-turn-begins
             {:async true
              :req (req (pos? (get-counters card :agenda)))
              :effect (effect (show-wait-prompt :runner "Corp to use Voting Machine Initiative")
                              (continue-ability
                                {:optional
                                 {:player :corp
                                  :prompt "Use Voting Machine Initiative to make the Runner lose 1 [Click]?"
                                  :yes-ability {:msg "make the Runner lose 1 [Click]"
                                                :effect (effect (lose :runner :click 1)
                                                                (add-counter card :agenda -1)
                                                                (clear-wait-prompt :runner))}
                                  :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                                card nil))}}}

   "Vulnerability Audit"
   (let [reg-no-score-flag
         (effect (register-turn-flag! card :can-score
                                      (fn [state side other-card]
                                        (if (same-card? other-card card)
                                          ((constantly false) (toast state :corp "Cannot score Vulnerability Audit the turn it was installed." "warning"))
                                          true))))]
     {:derezzed-events {:pre-agenda-scored {:req (req (and (same-card? target card)
                                                           (let [agenda-cids (map #(:cid (first %))
                                                                                  (filter #(agenda? (first %))
                                                                                          (turn-events state :corp :corp-install)))]
                                                             (contains? (into #{} agenda-cids) (:cid card)))))
                                            :effect reg-no-score-flag}}})

   "Vulcan Coverup"
   {:interactive (req true)
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))
    :stolen {:msg "force the Corp to take 1 bad publicity"
             :effect (effect (gain-bad-publicity :corp 1))}}

   "Water Monopoly"
   {:events {:pre-install {:req (req (and (resource? target)
                                          (not (has-subtype? target "Virtual"))
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 1]))}}}})
