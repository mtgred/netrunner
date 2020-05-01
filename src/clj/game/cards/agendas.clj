(ns game.cards.agendas
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [define-card]]
            [game.core.effects :refer [register-floating-effect]]
            [game.core.eid :refer [effect-completed]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.set :as clj-set]
            [jinteki.utils :refer :all]))

(defn ice-boost-agenda [subtype]
  (letfn [(count-ice [corp]
            (reduce (fn [c server]
                      (+ c (count (filter #(and (has-subtype? % subtype)
                                                (rezzed? %))
                                          (:ices server)))))
                    0
                    (flatten (seq (:servers corp)))))]
    {:msg (msg "gain " (count-ice corp) " [Credits]")
     :interactive (req true)
     :effect (effect (gain-credits (count-ice corp))
               (update-all-ice))
     :swapped {:effect (req (update-all-ice state side))}
     :constant-effects [{:type :ice-strength
                         :req (req (has-subtype? target subtype))
                         :value 1}]}))

;; Card definitions

(define-card "15 Minutes"
  {:abilities [{:cost [:click 1]
                :msg "shuffle 15 Minutes into R&D"
                :label "Shuffle 15 Minutes into R&D"
                :effect (effect (move :corp card :deck nil)
                                (shuffle! :corp :deck)
                                (update-all-agenda-points))}]
   :flags {:has-abilities-when-stolen true}})

(define-card "Accelerated Beta Test"
  (letfn [(abt [titles choices]
            {:async true
             :prompt (str "The top 3 cards of R&D: " titles)
             :choices (concat (filter ice? choices) ["Done"])
             :effect (req (if (= target "Done")
                            (do (unregister-events state side card)
                                (trash-cards state side eid choices {:unpreventable true}))
                            (wait-for (corp-install state side target nil
                                                    {:ignore-all-cost true
                                                     :install-state :rezzed-no-cost})
                                      (let [choices (remove-once #(= target %) choices)]
                                        (cond
                                          ;; Shuffle ends the ability
                                          (get-in (get-card state card) [:special :shuffle-occurred])
                                          (do (unregister-events state side card)
                                              (trash-cards state side eid choices {:unpreventable true}))
                                          ;; There are still ice left
                                          (seq (filter ice? choices))
                                          (continue-ability
                                            state side (abt titles choices) card nil)
                                          ;; Trash what's left
                                          :else
                                          (do (unregister-events state side card)
                                              (trash-cards state side eid choices {:unpreventable true})))))))})
          (suffer [titles choices]
            {:prompt (str "The top 3 cards of R&D: " titles
                          ". None are ice. Say goodbye!")
             :choices ["I have no regrets"]
             :async true
             :effect (effect (system-msg (str "trashes " (quantify (count choices) "card")))
                             (trash-cards eid choices {:unpreventable true}))})]
    {:interactive (req true)
     :optional
     {:prompt "Look at the top 3 cards of R&D?"
      :yes-ability
      {:async true
       :msg "look at the top 3 cards of R&D"
       :effect (req (register-events
                      state side card
                      [{:event :corp-shuffle-deck
                        :effect (effect (update! (assoc-in card [:special :shuffle-occurred] true)))}])
                 (let [choices (take 3 (:deck corp))
                       titles (join ", " (map :title choices))]
                   (continue-ability
                     state side
                     (if (seq (filter ice? choices))
                       (abt titles choices)
                       (suffer titles choices))
                     card nil)))}}}))

(define-card "Advanced Concept Hopper"
  {:events
   [{:event :run
     :req (req (first-event? state side :run))
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
                       card nil))}]})

(define-card "Ancestral Imager"
  {:events [{:event :jack-out
             :msg "do 1 net damage"
             :effect (effect (damage :net 1))}]})

(define-card "AR-Enhanced Security"
  {:events [{:event :runner-trash
             :once :per-turn
             :async true
             :interactive (req true)
             :req (req (some corp? targets))
             :msg "give the Runner a tag"
             :effect (effect (gain-tags eid 1))}]})

(define-card "Architect Deployment Test"
  {:interactive (req true)
   :async true
   :msg "look at the top 5 cards of R&D"
   :prompt (msg "The top cards of R&D are (top->bottom) " (join ", " (map :title (take 5 (:deck corp)))))
   :choices ["OK"]
   :effect (effect (continue-ability
                     {:prompt "Install a card?"
                      :choices (cancellable (filter corp-installable-type? (take 5 (:deck corp))))
                      :async true
                      :effect (effect (corp-install eid target nil
                                                    {:ignore-all-cost true
                                                     :install-state :rezzed-no-rez-cost}))
                      :cancel-effect (effect (system-msg "does not install any of the top 5 cards")
                                             (effect-completed eid))}
                     card nil))})

(define-card "Armed Intimidation"
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
                     card nil))})

(define-card "Armored Servers"
  {:effect (effect (add-counter card :agenda 1))
   :silent (req true)
   :abilities [{:cost [:agenda 1]
                :req (req run)
                :msg "make the Runner trash a card from their grip to jack out or break subroutines for the remainder of the run"
                :effect (effect (register-floating-effect
                                  card
                                  {:type :break-sub-additional-cost
                                   :duration :end-of-run
                                   :value (req (repeat (count (:broken-subs (second targets))) [:trash-from-hand 1]))})
                                (register-floating-effect
                                  card
                                  {:type :jack-out-additional-cost
                                   :duration :end-of-run
                                   :value [:trash-from-hand 1]}))}]})

(define-card "AstroScript Pilot Program"
  {:effect (effect (add-counter card :agenda 1))
   :silent (req true)
   :abilities [{:cost [:agenda 1]
                :msg (msg "place 1 advancement token on " (card-str state target))
                :choices {:card can-be-advanced?}
                :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]})

(define-card "Award Bait"
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
                                                {:choices {:card can-be-advanced?}
                                                 :msg (msg "place " c " advancement tokens on " (card-str state target))
                                                 :cancel-effect (req (clear-wait-prompt state :runner)
                                                                     (effect-completed state side eid))
                                                 :effect (effect (add-prop :corp target :advance-counter c {:placed true})
                                                                 (clear-wait-prompt :runner))} card nil)))}
                              card nil))}})

(define-card "Bacterial Programming"
  (letfn [(hq-step [remaining to-trash to-hq]
            {:async true
             :prompt "Select a card to move to HQ"
             :choices (conj (vec remaining) "Done")
             :effect (req (if (= "Done" target)
                            (wait-for (trash-cards state :corp to-trash {:unpreventable true})
                                      (doseq [h to-hq]
                                        (move state :corp h :hand))
                                      (if (seq remaining)
                                        (continue-ability state :corp (reorder-choice :corp (vec remaining)) card nil)
                                        (do (clear-wait-prompt state :runner)
                                            (system-msg state :corp
                                                        (str "uses Bacterial Programming to add " (count to-hq)
                                                             " cards to HQ, discard " (count to-trash)
                                                             ", and arrange the top cards of R&D"))
                                            (effect-completed state :corp eid))))
                            (continue-ability state :corp (hq-step
                                                            (clj-set/difference (set remaining) (set [target]))
                                                            to-trash
                                                            (conj to-hq target)) card nil)))})
          (trash-step [remaining to-trash]
            {:async true
             :prompt "Select a card to discard"
             :choices (conj (vec remaining) "Done")
             :effect (req (if (= "Done" target)
                            (continue-ability state :corp (hq-step remaining to-trash '()) card nil)
                            (continue-ability state :corp (trash-step
                                                            (clj-set/difference (set remaining) (set [target]))
                                                            (conj to-trash target)) card nil)))})]
    (let [arrange-rd
          (effect (continue-ability
                    {:optional
                     {:async true
                      :prompt "Arrange top 7 cards of R&D?"
                      :yes-ability
                      {:async true
                       :effect (req (let [c (take 7 (:deck corp))]
                                      (when (:run @state)
                                        (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                                      (show-wait-prompt state :runner "Corp to use Bacterial Programming")
                                      (continue-ability state :corp (trash-step c '()) card nil)))}}}
                    card nil))]
      {:effect arrange-rd
       :async true
       :stolen {:async true
                :effect arrange-rd}
       :interactive (req true)})))

(define-card "Bellona"
  {:steal-cost-bonus (req [:credit 5])
   :effect (req (gain-credits state :corp 5)
                (system-msg state side (str "uses " (:title card) " to gain 5 [Credits]")))})

(define-card "Better Citizen Program"
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
    {:events [{:event :play-event
               :req (req (and (has-subtype? target "Run")
                              (first-event? state :runner :play-event #(has-subtype? (first %) "Run"))
                              (not (used-this-turn? (:cid card) state))))
               :async true
               :effect (ability "playing a run event")}
              {:event :runner-install
               :silent (req true)
               :req (req (and (has-subtype? target "Icebreaker")
                              (first-event? state :runner :runner-install #(has-subtype? (first %) "Icebreaker"))
                              (not (used-this-turn? (:cid card) state))))
               :async true
               :effect (ability "installing an icebreaker")}]}))

(define-card "Bifrost Array"
  {:req (req (not (empty? (filter #(not= (:title %)
                                         "Bifrost Array")
                                  (:scored corp)))))
   :optional {:prompt "Trigger the ability of a scored agenda?"
              :yes-ability {:prompt "Select an agenda to trigger the \"when scored\" ability of"
                            :choices {:card #(and (agenda? %)
                                                  (not= (:title %)
                                                        "Bifrost Array")
                                                  (= (first (:zone %))
                                                     :scored)
                                                  (when-scored? %)
                                                  (:abilities %))}
                            :msg (msg "trigger the \"when scored\" ability of " (:title target))
                            :effect (effect (continue-ability (card-def target) target nil))}
              :no-ability {:effect (effect (clear-wait-prompt :runner))}}})

(define-card "Brain Rewiring"
  {:async true
   :effect (effect (show-wait-prompt :runner "Corp to use Brain Rewiring")
                   (continue-ability
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
                     card nil))})

(define-card "Braintrust"
  {:effect (effect (add-counter card :agenda (quot (- (get-counters card :advancement) 3) 2)))
   :silent (req true)
   :constant-effects [{:type :rez-cost
                       :req (req (ice? target))
                       :value (req (- (get-counters card :agenda)))}]})

(define-card "Breaking News"
  {:async true
   :silent (req true)
   :msg "give the Runner 2 tags"
   :effect (effect (gain-tags :corp eid 2))
   :events (let [event {:unregister-once-resolved true
                        :req (effect (first-event? :agenda-scored #(same-card? card (first %))))
                        :msg "make the Runner lose 2 tags"
                        :effect (effect (lose :runner :tag 2))}]
             [(assoc event :event :corp-turn-ends)
              (assoc event :event :runner-turn-ends)])})

(define-card "Broad Daylight"
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
                  :effect (effect (damage eid :meat 2 {:card card}))}]}))

(define-card "CFC Excavation Contract"
  {:effect (req (let [bios (count (filter #(has-subtype? % "Bioroid") (all-active-installed state :corp)))
                      bucks (* bios 2)]
                  (gain-credits state side bucks)
                  (system-msg state side (str "gains " bucks " [Credits] from CFC Excavation Contract"))))})

(define-card "Character Assassination"
  {:prompt "Select a resource to trash"
   :choices {:card #(and (installed? %)
                         (resource? %))}
   :msg (msg "trash " (:title target))
   :interactive (req true)
   :async true
   :effect (effect (trash eid target {:unpreventable true}))})

(define-card "Chronos Project"
  {:msg "remove all cards in the Runner's Heap from the game"
   :interactive (req true)
   :effect (effect (move-zone :runner :discard :rfg))})

(define-card "City Works Project"
  (letfn [(meat-damage [s c] (+ 2 (get-counters (get-card s c) :advancement)))]
    {:install-state :face-up
     :access {:req (req installed)
              :msg (msg "do " (meat-damage state card) " meat damage")
              :async true
              :effect (effect (damage eid :meat (meat-damage state card) {:card card}))}}))

(define-card "Clone Retirement"
  {:msg "remove 1 bad publicity"
   :effect (effect (lose-bad-publicity 1))
   :silent (req true)
   :stolen {:msg "force the Corp to take 1 bad publicity"
            :effect (effect (gain-bad-publicity :corp 1))}})

(define-card "Corporate Sales Team"
  (let [e {:req (req (pos? (get-counters card :credit)))
           :msg "gain 1 [Credits]"
           :effect (req (gain-credits state :corp 1)
                        (add-counter state side card :credit -1))}]
    {:effect (effect (add-counter card :credit 10))
     :silent (req true)
     :events [(assoc e :event :runner-turn-begins)
              (assoc e :event :corp-turn-begins)]}))

(define-card "Corporate War"
  {:msg (msg (if (> (:credit corp) 6) "gain 7 [Credits]" "lose all credits"))
   :interactive (req true)
   :effect (req (if (> (:credit corp) 6)
                  (gain-credits state :corp 7) (lose-credits state :corp :all)))})

(define-card "Crisis Management"
  (let [ability {:req (req tagged)
                 :async true
                 :label "Do 1 meat damage (start of turn)"
                 :once :per-turn
                 :msg "do 1 meat damage"
                 :effect (effect (damage eid :meat 1 {:card card}))}]
    {:events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Cyberdex Sandbox"
  {:optional {:prompt "Purge virus counters with Cyberdex Sandbox?"
              :yes-ability {:msg (msg "purge virus counters")
                            :effect (effect (purge))}}
   :events [{:event :purge
             :once :per-turn
             :msg "gain 4 [Credits]"
             :effect (req (gain-credits state :corp 4))}]})

(define-card "Dedicated Neural Net"
  {:events [{:event :successful-run
             :interactive (req true)
             :psi {:req (req (= target :hq))
                   :once :per-turn
                   :not-equal {:effect (effect (register-floating-effect
                                                 card
                                                 {:type :corp-choose-hq-access
                                                  :duration :end-of-access
                                                  :value true})
                                               (effect-completed eid))}}}]})

(define-card "Degree Mill"
  {:steal-cost-bonus (req [:shuffle-installed-to-stack 2])})

(define-card "Director Haas' Pet Project"
  (letfn [(install-ability [server-name n]
            {:prompt "Select a card to install"
             :show-discard true
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (#{[:hand] [:discard]} (:zone %)))}
             :msg (msg (corp-install-msg target)
                       (when (zero? n)
                         ", creating a new remote server")
                       ", ignoring all install costs")
             :async true
             :effect (req (wait-for (corp-install state side target server-name {:ignore-all-cost true})
                                    (continue-ability state side
                                                      (when (< n 2)
                                                        (install-ability (last (get-remote-names state)) (inc n)))
                                                      card nil)))})]
    {:optional {:prompt "Install cards in a new remote server?"
                :yes-ability (install-ability "New remote" 0)}}))

(define-card "Divested Trust"
  {:events
   [{:event :agenda-stolen
     :async true
     :interactive (req true)
     :effect (req (if (:winner @state)
                    (effect-completed state side eid)
                    (let [card (find-latest state card)
                          stolen-agenda (find-latest state target)
                          title (:title stolen-agenda)
                          prompt (str "Forfeit Divested Trust to add " title
                                      " to HQ and gain 5[Credits]?")
                          message (str "add " title " to HQ and gain 5 [Credits]")
                          agenda-side (if (in-runner-scored? state side stolen-agenda)
                                        :runner :corp)
                          card-side (if (in-runner-scored? state side card)
                                      :runner :corp)]
                      (show-wait-prompt state :runner "Corp to use Divested Trust")
                      (continue-ability
                        state side
                        {:optional
                         {:prompt prompt
                          :yes-ability
                          {:msg message
                           :effect (req (forfeit state card-side card)
                                        (move state side stolen-agenda :hand)
                                        (update-all-agenda-points state side)
                                        (gain-credits state side 5))}
                          :end-effect (effect (clear-wait-prompt :runner))}}
                        card nil))))}]})

(define-card "Domestic Sleepers"
  {:agendapoints-corp (req (if (pos? (get-counters card :agenda)) 1 0))
   :abilities [{:cost [:click 3]
                :msg "place 1 agenda counter on Domestic Sleepers"
                :effect (effect (add-counter card :agenda 1)
                                (update-all-agenda-points)
                                (check-winner))}]})

(define-card "Eden Fragment"
  {:constant-effects [{:type :ignore-install-cost
                       :req (req (and (ice? target)
                                      (->> (turn-events state side :corp-install)
                                           (map first)
                                           (filter ice?)
                                           empty?)))
                       :value true}]
   :events [{:event :corp-install
             :req (req (and (ice? target)
                            (empty? (let [cards (map first (turn-events state side :corp-install))]
                                      (filter ice? cards)))))
             :msg (msg "ignore the install cost of the first ICE this turn")}]})

(define-card "Efficiency Committee"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 3))
   :abilities [{:cost [:click 1 :agenda 1]
                :effect (effect (gain :click 2)
                                (register-turn-flag!
                                  card :can-advance
                                  (fn [state side card]
                                    ((constantly false)
                                     (toast state :corp "Cannot advance cards this turn due to Efficiency Committee." "warning")))))
                :msg "gain [Click][Click]"}]})

(define-card "Elective Upgrade"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 2))
   :abilities [{:cost [:click 1 :agenda 1]
                :once :per-turn
                :effect (effect (gain :click 2))
                :msg "gain [Click][Click]"}]})

(define-card "Encrypted Portals"
  (ice-boost-agenda "Code Gate"))

(define-card "Escalate Vitriol"
  {:abilities [{:label "Gain 1 [Credit] for each Runner tag"
                :cost [:click 1]
                :once :per-turn
                :msg (msg "gain " (count-tags state) " [Credits]")
                :effect (effect (gain-credits (count-tags state)))}]})

(define-card "Executive Retreat"
  {:effect (effect (add-counter card :agenda 1)
                   (shuffle-into-deck :hand))
   :interactive (req true)
   :abilities [{:cost [:click 1 :agenda 1]
                :msg "draw 5 cards"
                :effect (effect (draw 5))}]})

(define-card "Explode-a-palooza"
  {:flags {:rd-reveal (req true)}
   :access {:async true
            :effect (effect (show-wait-prompt :runner "Corp to use Explode-a-palooza")
                            (continue-ability
                              {:optional {:prompt "Gain 5 [Credits] with Explode-a-palooza ability?"
                                          :yes-ability {:msg "gain 5 [Credits]"
                                                        :effect (effect (gain-credits :corp 5)
                                                                        (clear-wait-prompt :runner))}
                                          :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                              card nil))}})

(define-card "False Lead"
  {:abilities [{:req (req (<= 2 (:click runner)))
                :msg "force the Runner to lose [Click][Click]"
                :cost [:forfeit-self]
                :effect (effect (lose :runner :click 2))}]})

(define-card "Fetal AI"
  {:flags {:rd-reveal (req true)}
   :access {:async true
            :req (req (not= (first (:zone card)) :discard))
            :msg "do 2 net damage"
            :effect (effect (damage eid :net 2 {:card card}))}
   :steal-cost-bonus (req [:credit 2])})

(define-card "Firmware Updates"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 3))
   :abilities [{:cost [:agenda 1]
                :choices {:card #(and (ice? %)
                                      (can-be-advanced? %))}
                :req (req (pos? (get-counters card :agenda)))
                :msg (msg "place 1 advancement token on " (card-str state target))
                :once :per-turn
                :effect (effect (add-prop target :advance-counter 1))}]})

(define-card "Flower Sermon"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 5))
   :abilities [{:cost [:agenda 1]
                :once :per-turn
                :msg (msg "reveal " (:title (first (:deck corp))) " and draw 2 cards")
                :async true
                :effect (req (reveal state side [(first (:deck corp))])
                             (show-wait-prompt state :runner (str "Corp to resolve " (:title card)))
                             (wait-for (draw state side 2 nil)
                                       (continue-ability state side
                                                         {:req (req (pos? (count (:hand corp))))
                                                          :prompt "Choose a card in HQ to move to the top of R&D"
                                                          :msg "add 1 card in HQ to the top of R&D"
                                                          :choices {:card #(and (in-hand? %)
                                                                                (corp? %))}
                                                          :effect (effect (move target :deck {:front true})
                                                                          (clear-wait-prompt :runner)
                                                                          (effect-completed eid))}
                                                         card nil)))}]})

(define-card "Fly on the Wall"
  {:msg "give the runner 1 tag"
   :async true
   :effect (req (gain-tags state :runner eid 1))})

(define-card "Genetic Resequencing"
  {:choices {:card #(= (last (:zone %)) :scored)}
   :msg (msg "add 1 agenda counter on " (:title target))
   :effect (effect (add-counter target :agenda 1)
                   (update-all-agenda-points))
   :silent (req true)})

(define-card "Geothermal Fracking"
  {:effect (effect (add-counter card :agenda 2))
   :silent (req true)
   :abilities [{:cost [:click 1 :agenda 1]
                :msg "gain 7 [Credits] and take 1 bad publicity"
                :effect (effect (gain-credits 7)
                                (gain-bad-publicity :corp 1))}]})

(define-card "Gila Hands Arcology"
  {:abilities [{:cost [:click 2]
                :msg "gain 3 [Credits]"
                :effect (effect (gain-credits 3))}]})

(define-card "Glenn Station"
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
                :effect (effect (move target :hand))}]})

(define-card "Global Food Initiative"
  {:agendapoints-runner (req 2)})

(define-card "Government Contracts"
  {:abilities [{:cost [:click 2]
                :effect (effect (gain-credits 4))
                :msg "gain 4 [Credits]"}]})

(define-card "Government Takeover"
  {:abilities [{:cost [:click 1]
                :effect (effect (gain-credits 3))
                :msg "gain 3 [Credits]"}]})

(define-card "Graft"
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
     :effect (effect (continue-ability (graft 1) card nil))}))

(define-card "Hades Fragment"
  {:flags {:corp-phase-12 (req (and (not-empty (get-in @state [:corp :discard]))
                                    (is-scored? state :corp card)))}
   :abilities [{:prompt "Select a card to add to the bottom of R&D"
                :show-discard true
                :choices {:card #(and (corp? %)
                                      (in-discard? %))}
                :effect (effect (move target :deck))
                :msg (msg "add "
                          (if (:seen target)
                            (:title target)
                            "a card")
                          " to the bottom of R&D")}]})

(define-card "Helium-3 Deposit"
  {:async true
   :interactive (req true)
   :prompt "How many power counters?"
   :choices ["0" "1" "2"]
   :effect (req (let [c (str->int target)]
                  (continue-ability
                    state side
                    {:choices {:card #(pos? (get-counters % :power))}
                     :msg (msg "add " c " power counters on " (:title target))
                     :effect (effect (add-counter target :power c))}
                    card nil)))})

(define-card "High-Risk Investment"
  {:effect (effect (add-counter card :agenda 1))
   :silent (req true)
   :abilities [{:cost [:click 1 :agenda 1]
                :msg (msg "gain " (:credit runner) " [Credits]")
                :effect (effect (gain-credits (:credit runner)))}]})

(define-card "Hollywood Renovation"
  {:install-state :face-up
   :events [{:event :advance
             :async true
             :req (req (same-card? card target))
             :effect (req (let [n (if (>= (get-counters (get-card state card) :advancement) 6) 2 1)]
                            (continue-ability
                              state side
                              {:choices {:card #(and (not (same-card? % card))
                                                     (can-be-advanced? %))}
                               :msg (msg "place " n
                                         " advancement tokens on "
                                         (card-str state target))
                               :effect (effect (add-prop :corp target :advance-counter n {:placed true}))}
                              card nil)))}]})

(define-card "Hostile Takeover"
  {:msg "gain 7 [Credits] and take 1 bad publicity"
   :effect (effect (gain-credits 7)
                   (gain-bad-publicity :corp 1))
   :interactive (req true)})

(define-card "House of Knives"
  {:effect (effect (add-counter card :agenda 3))
   :silent (req true)
   :abilities [{:cost [:agenda 1]
                :msg "do 1 net damage"
                :req (req (:run @state))
                :once :per-run
                :effect (effect (damage eid :net 1 {:card card}))}]})

(define-card "Hyperloop Extension"
  (let [he (req (gain-credits state :corp 3)
                (system-msg state side (str "uses Hyperloop Extension to gain 3 [Credits]")))]
    {:effect he
     :stolen {:effect he}}))

(define-card "Ikawah Project"
  {:steal-cost-bonus (req [:credit 2 :click 1])})

(define-card "Illicit Sales"
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
                            (effect-completed state side eid))))})

(define-card "Improved Protein Source"
  {:msg "make the Runner gain 4 [Credits]"
   :effect (effect (gain-credits :runner 4))
   :interactive (req true)
   :stolen {:msg "make the Runner gain 4 [Credits]"
            :effect (effect (gain-credits :runner 4))}})

(define-card "Improved Tracers"
  {:silent (req true)
   :effect (req (update-all-ice state side))
   :swapped {:effect (req (update-all-ice state side))}
   :constant-effects [{:type :ice-strength
                       :req (req (has-subtype? target "Tracer"))
                       :value 1}]
   :events [{:event :pre-init-trace
             :req (req (and (has-subtype? target "Tracer")
                            (= :subroutine (:source-type (second targets)))))
             :effect (effect (init-trace-bonus 1))}]})

(define-card "Jumon"
  {:events
   [{:event :corp-turn-ends
     :req (req (some #(and (= (last (:zone %)) :content)
                           (is-remote? (second (:zone %))))
                     (all-installed state :corp)))
     :prompt "Select a card to place 2 advancement tokens on"
     :player :corp
     :choices {:card #(and (= (last (:zone %)) :content)
                           (is-remote? (second (:zone %))))}
     :msg (msg "place 2 advancement token on " (card-str state target))
     :effect (effect (add-prop :corp target :advance-counter 2 {:placed true}))}]})

(define-card "Labyrinthine Servers"
  {:interactions {:prevent [{:type #{:jack-out}
                             :req (req (pos? (get-counters card :power)))}]}
   :silent (req true)
   :effect (effect (add-counter card :power 2))
   :abilities [{:req (req (:run @state))
                :cost [:power 1]
                :msg "prevent the Runner from jacking out"
                :effect (effect (jack-out-prevent))}]})

(define-card "License Acquisition"
  {:interactive (req true)
   :prompt "Select an asset or upgrade to install from Archives or HQ"
   :show-discard true
   :choices {:card #(and (or (asset? %) (upgrade? %))
                         (#{[:hand] [:discard]} (:zone %))
                         (corp? %))}
   :msg (msg "install and rez " (:title target) ", ignoring all costs")
   :async true
   :effect (effect (corp-install eid target nil {:install-state :rezzed-no-cost}))})

(define-card "Mandatory Seed Replacement"
  (letfn [(msr [] {:prompt "Select two pieces of ICE to swap positions"
                   :choices {:card #(and (installed? %)
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
     :effect (effect (continue-ability (msr) card nil))}))

(define-card "Mandatory Upgrades"
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
                          :click-per-turn 1))})

(define-card "Market Research"
  {:interactive (req true)
   :req (req tagged)
   :effect (effect (add-counter card :agenda 1)
                   (update-all-agenda-points)
                   (check-winner))
   :agendapoints-corp (req (if (zero? (get-counters card :agenda)) 2 3))})

(define-card "Medical Breakthrough"
  {:silent (req true)
   :effect (effect (update-all-advancement-costs))
   :stolen {:effect (effect (update-all-advancement-costs))}
   :advancement-cost-bonus (req (- (count (filter #(= (:title %) "Medical Breakthrough")
                                                  (concat (:scored corp) (:scored runner))))))})

(define-card "Megaprix Qualifier"
  {:silent (req true)
   :req (req (< 1 (count (filter #(= (:title %) "Megaprix Qualifier")
                                 (concat (:scored corp) (:scored runner))))))
   :effect (effect (add-counter card :agenda 1))
   :agendapoints-corp (req (if (zero? (get-counters card :agenda)) 1 2))})

(define-card "Merger"
  {:agendapoints-runner (req 3)})

(define-card "Meteor Mining"
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
                  (do (system-msg state side "uses Meteor Mining do 7 meat damage")
                      (damage state side eid :meat 7 {:card card}))
                  "No action"
                  (do (system-msg state side "does not use Meteor Mining")
                      (effect-completed state side eid))))})

(define-card "NAPD Contract"
  {:steal-cost-bonus (req [:credit 4])
   :advancement-cost-bonus (req (count-bad-pub state))})

(define-card "Net Quarantine"
  (let [nq {:effect (req (let [extra (int (/ (:runner-spent target) 2))]
                           (when (pos? extra)
                             (gain-credits state side extra)
                             (system-msg state :corp (str "uses Net Quarantine to gain " extra "[Credits]")))))}]
    {:events [{:event :pre-init-trace
               :once :per-turn
               :silent (req true)
               :effect (req (system-msg state :corp "uses Net Quarantine to reduce Runner's base link to zero")
                            (swap! state assoc-in [:trace :force-link] 0))}
              (assoc nq :event :successful-trace)
              (assoc nq :event :unsuccessful-trace)]}))

(define-card "New Construction"
  {:install-state :face-up
   :events [{:event :advance
             :optional
             {:req (req (same-card? card target))
              :prompt "Install a card from HQ in a new remote?"
              :yes-ability {:prompt "Select a card to install"
                            :choices {:card #(and (not (operation? %))
                                                  (not (ice? %))
                                                  (corp? %)
                                                  (in-hand? %))}
                            :msg (msg "install a card from HQ"
                                      (when (<= 5 (get-counters (get-card state card) :advancement))
                                        " and rez it, ignoring all costs"))
                            :async true
                            :effect (effect (corp-install
                                              eid target "New remote"
                                              (when (<= 5 (get-counters (get-card state card) :advancement))
                                                {:install-state :rezzed-no-cost})))}}}]})

(define-card "NEXT Wave 2"
  {:not-when-scored true
   :req (req (some #(and (rezzed? %)
                         (ice? %)
                         (has-subtype? % "NEXT"))
                   (all-installed state :corp)))
   :optional {:prompt "Do 1 brain damage with NEXT Wave 2?"
              :yes-ability {:msg "do 1 brain damage"
                            :effect (effect (damage eid :brain 1 {:card card}))}}})

(define-card "Nisei MK II"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 1))
   :abilities [{:req (req (:run @state))
                :cost [:agenda 1]
                :msg "end the run"
                :async true
                :effect (effect (end-run eid card))}]})

(define-card "Oaktown Renovation"
  {:install-state :face-up
   :events [{:event :advance
             :req (req (same-card? card target))
             :msg (msg "gain " (if (>= (get-counters (get-card state card) :advancement) 5) "3" "2") " [Credits]")
             :effect (req (gain-credits state side
                                        (if (>= (get-counters (get-card state card) :advancement) 5) 3 2)))}]})

(define-card "Obokata Protocol"
  {:steal-cost-bonus (req [:net 4])})

(define-card "Paper Trail"
  {:trace {:base 6
           :successful {:msg "trash all connection and job resources"
                        :async true
                        :effect (req (let [resources (filter #(or (has-subtype? % "Job")
                                                                  (has-subtype? % "Connection"))
                                                             (all-active-installed state :runner))]
                                       (trash-cards state side eid resources)))}}})

(define-card "Personality Profiles"
  (let [pp {:req (req (pos? (count (:hand runner))))
            :async true
            :effect (req (let [c (first (shuffle (:hand runner)))]
                           (system-msg state side (str "uses Personality Profiles to force the Runner to trash "
                                                       (:title c) " from their Grip at random"))
                           (trash state side eid c nil)))}]
    {:events [(assoc pp :event :searched-stack)
              (assoc pp
                     :event :runner-install
                     :req (req (and (some #{:discard} (:previous-zone target))
                                    (pos? (count (:hand runner))))))]}))

(define-card "Philotic Entanglement"
  {:interactive (req true)
   :req (req (pos? (count (:scored runner))))
   :msg (msg "do " (count (:scored runner)) " net damage")
   :effect (effect (damage eid :net (count (:scored runner)) {:card card}))})

(define-card "Posted Bounty"
  {:optional {:prompt "Forfeit Posted Bounty to give the Runner 1 tag and take 1 bad publicity?"
              :yes-ability {:msg "give the Runner 1 tag and take 1 bad publicity"
                            :async true
                            :effect (effect (gain-bad-publicity :corp eid 1)
                                            (gain-tags :corp eid 1)
                                            (forfeit card))}}})

(define-card "Priority Requisition"
  {:interactive (req true)
   :choices {:card #(and (ice? %)
                         (not (rezzed? %))
                         (installed? %))}
   :effect (effect (rez target {:ignore-cost :all-costs}))})

(define-card "Private Security Force"
  {:abilities [{:req (req tagged)
                :cost [:click 1]
                :effect (effect (damage eid :meat 1 {:card card}))
                :msg "do 1 meat damage"}]})

(define-card "Profiteering"
  {:interactive (req true)
   :choices ["0" "1" "2" "3"]
   :prompt "How many bad publicity?"
   :msg (msg "take " target " bad publicity and gain " (* 5 (str->int target)) " [Credits]")
   :effect (req (let [bp (count-bad-pub state)]
                  (gain-bad-publicity state :corp eid (str->int target))
                  (if (< bp (count-bad-pub state))
                    (gain-credits state :corp (* 5 (str->int target))))))})

(define-card "Project Ares"
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
                            :card #(and (runner? %)
                                        (installed? %))}
                  :async true
                  :effect (req (wait-for (trash-cards state side targets)
                                         (system-msg state side (str "trashes " (join ", " (map :title targets))))
                                         (clear-wait-prompt state :corp)
                                         (gain-bad-publicity state :corp eid 1)))}
                 card nil))}))

(define-card "Project Atlas"
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
                                (move target :hand))}]})

(define-card "Project Beale"
  {:interactive (req true)
   :agendapoints-runner (req 2)
   :agendapoints-corp (req (+ 2 (get-counters card :agenda)))
   :effect (req (let [n (quot (- (get-counters card :advancement) 3) 2)]
                  (add-counter state side card :agenda n)
                  (update-all-agenda-points state side)
                  (check-winner state side)))})

(define-card "Project Kusanagi"
  {:silent (req true)
   :effect (effect (add-counter card :agenda (- (get-counters card :advancement) 2)))
   :events [{:event :run-ends
             :effect (req (let [cid (:cid card)
                                ices (get-in card [:special :kusanagi])]
                            (doseq [i ices]
                              (when-let [ice (get-card state i)]
                                (remove-sub! state side ice #(= cid (:from-cid %))))))
                          (update! state side (dissoc-in card [:special :kusanagi])))}]
   :abilities [{:label "Give a piece of ICE \"[Subroutine] Do 1 net damage\""
                :prompt "Choose a piece of ICE"
                :choices {:card #(and (ice? %)
                                      (rezzed? %))}
                :cost [:agenda 1]
                :msg (str "make a piece of ICE gain \"[Subroutine] Do 1 net damage\" "
                          "after all its other subroutines for the remainder of the run")
                :effect  (effect (add-extra-sub! (get-card state target)
                                                 (do-net-damage 1)
                                                 (:cid card) {:back true})
                                 (update! (update-in card [:special :kusanagi] #(conj % target))))}]})

(define-card "Project Vacheron"
  (let [vacheron-ability
        {:req (req (and (in-scored? card)
                        (not= (first (:previous-zone card)) :discard)
                        (same-card? card target)))
         :msg (msg "add 4 agenda counters on " (:title card))
         :effect (effect (add-counter (get-card state card) :agenda 4)
                         (update! (assoc-in (get-card state card) [:special :vacheron] true)))}]
    {:agendapoints-runner (req (if (or (= (first (:previous-zone card)) :discard)
                                       (and (get-in card [:special :vacheron])
                                            (zero? (get-counters card :agenda)))) 3 0))
     :stolen vacheron-ability
     :events [(assoc vacheron-ability :event :as-agenda)
              {:event :runner-turn-begins
               :req (req (pos? (get-counters card :agenda)))
               :msg (msg "remove 1 agenda token from " (:title card))
               :effect (req (when (pos? (get-counters card :agenda))
                              (add-counter state side card :agenda -1))
                            (when (= 0 (get-counters (get-card state card) :agenda))
                              (let [points (get-agenda-points state :runner (assoc-in card [:counter :agenda] 0))]
                                (system-msg state :runner
                                            (str "gains " (quantify points "agenda point")
                                                 " from " (:title card)))))
                            (update-all-agenda-points state side)
                            (check-winner state side))}]
     :flags {:has-events-when-stolen true}}))

(define-card "Project Vitruvius"
  {:silent (req true)
   :effect (effect (add-counter card :agenda (- (get-counters card :advancement) 3)))
   :abilities [{:cost [:agenda 1]
                :label "Add 1 card from Archives to HQ"
                :prompt "Choose a card in Archives to add to HQ"
                :show-discard true
                :choices {:card #(and (in-discard? %)
                                      (corp? %))}
                :req (req (pos? (get-counters card :agenda)))
                :msg (msg "add "
                          (if (:seen target)
                            (:title target) "an unseen card ")
                          " to HQ from Archives")
                :effect (effect (move target :hand))}]})

(define-card "Project Wotan"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 3))
   :events [{:event :run-ends
             :effect (req (let [cid (:cid card)
                                ices (get-in card [:special :wotan])]
                            (doseq [i ices]
                              (when-let [ice (get-card state i)]
                                (remove-sub! state side ice #(= cid (:from-cid %))))))
                          (update! state side (dissoc-in card [:special :wotan])))}]
   :abilities [{:req (req (and current-ice
                               (rezzed? current-ice)
                               (has-subtype? current-ice "Bioroid")
                               (= :approach-ice (:phase run))))
                :cost [:agenda 1]
                :msg (str "make the approached piece of Bioroid ICE gain \"[Subroutine] End the run\""
                          "after all its other subroutines for the remainder of this run")
                :effect  (effect (add-extra-sub! (get-card state current-ice)
                                                 {:label "End the run"
                                                  :msg "end the run"
                                                  :async true
                                                  :effect (effect (end-run eid card))}
                                                 (:cid card) {:back true})
                                 (update! (update-in card [:special :wotan] #(conj % current-ice))))}]})

(define-card "Project Yagi-Uda"
  (letfn [(put-back-counter [state side card]
            (set-prop state side card :counter
                      (merge
                        (:counter card)
                        {:agenda (+ 1 (get-counters card :agenda))})))
          (choose-swap [to-swap]
            {:prompt (str "Select a card in HQ to swap with " (:title to-swap))
             :choices {:not-self true
                       :card #(and (corp? %)
                                   (in-hand? %)
                                   (if (ice? to-swap)
                                     (ice? %)
                                     (or (agenda? %)
                                         (asset? %)
                                         (upgrade? %))))}
             :msg (msg "swap " (card-str state to-swap) " with a card from HQ")
             :effect (req (move state :corp to-swap (:zone target) {:keep-server-alive true
                                                                    :index (:index target)})
                          (move state :corp target (:zone to-swap) {:keep-server-alive true
                                                                    :index (:index to-swap)})
                          (clear-wait-prompt state :runner))
             :cancel-effect (effect (put-back-counter card)
                                    (clear-wait-prompt :runner))})
          (choose-card [run-server]
            {:prompt "Choose a card in or protecting the attacked server."
             :choices {:card #(= (first run-server) (second (:zone %)))}
             :effect (effect (continue-ability (choose-swap target) card nil))
             :cancel-effect (effect (put-back-counter card)
                                    (clear-wait-prompt :runner))})]
    {:silent (req true)
     :effect (effect (add-counter card :agenda (- (get-counters card :advancement) 3)))
     :abilities [{:cost [:agenda 1]
                  :req (req run)
                  :effect (effect (show-wait-prompt :runner "Corp to use Project Yagi-Uda")
                            (continue-ability (choose-card (:server run))
                                              card nil))}]}))

(define-card "Puppet Master"
  {:events [{:event :successful-run
             :interactive (req true)
             :async true
             :effect (req (show-wait-prompt state :runner "Corp to use Puppet Master")
                          (continue-ability
                            state :corp
                            {:prompt "Select a card to place 1 advancement token on"
                             :player :corp
                             :choices {:card can-be-advanced?}
                             :cancel-effect (effect (clear-wait-prompt :runner)
                                                    (effect-completed eid))
                             :msg (msg "place 1 advancement token on " (card-str state target))
                             :effect (effect (add-prop :corp target :advance-counter 1 {:placed true})
                                             (clear-wait-prompt :runner))} card nil))}]})

(define-card "Quantum Predictive Model"
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
            :msg "add it to their score area and gain 1 agenda point"}})

(define-card "Rebranding Team"
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
                                                           (join " - "))))))}))

(define-card "Reeducation"
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
                       (effect-completed state side eid)))))}))

(define-card "Remastered Edition"
  {:effect (effect (add-counter card :agenda 1))
   :silent (req true)
   :abilities [{:cost [:agenda 1]
                :msg (msg "place 1 advancement token on " (card-str state target))
                :label "Place 1 advancement token on an installed card"
                :choices {:card installed?}
                :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]})

(define-card "Remote Data Farm"
  {:silent (req true)
   :msg "increase their maximum hand size by 2"
   :effect (effect (gain :hand-size 2))
   :swapped {:msg "increase their maximum hand size by 2"
             :effect (effect (gain :hand-size 2))}
   :leave-play (effect (lose :hand-size 2))})

(define-card "Remote Enforcement"
  {:interactive (req true)
   :optional {:prompt "Search R&D for a piece of ice to install protecting a remote server?"
              :yes-ability
              {:async true
               :effect (effect
                         (continue-ability
                           (if (not-empty (filter ice? (:deck corp)))
                             {:async true
                              :prompt "Choose a piece of ice"
                              :choices (req (filter ice? (:deck corp)))
                              :effect (effect
                                        (continue-ability
                                          (let [chosen-ice target]
                                            {:async true
                                             :prompt (str "Select a server to install " (:title chosen-ice) " on")
                                             :choices (filter #(not (#{"HQ" "Archives" "R&D"} %))
                                                              (corp-install-list state chosen-ice))
                                             :effect (effect (shuffle! :deck)
                                                             (corp-install eid chosen-ice target
                                                                           {:install-state :rezzed-no-rez-cost}))})
                                          card nil))}
                             {:prompt "You have no ice in R&D"
                              :choices ["Carry on!"]
                              :prompt-type :bogus
                              :effect (effect (shuffle! :deck))})
                           card nil))}}})

(define-card "Research Grant"
  {:interactive (req true)
   :silent (req (empty? (filter #(= (:title %) "Research Grant") (all-installed state :corp))))
   :async true
   :effect (effect (continue-ability
                     {:prompt "Select another installed copy of Research Grant to score"
                      :choices {:card #(= (:title %) "Research Grant")}
                      :interactive (req true)
                      :async true
                      :req (req (not (empty? (filter #(= (:title %) "Research Grant") (all-installed state :corp)))))
                      :effect (effect (set-prop target :advance-counter (:advancementcost target))
                                      (score eid (get-card state target)))
                      :msg "score another installed copy of Research Grant"}
                     card nil))})

(define-card "Restructured Datapool"
  {:abilities [{:cost [:click 1]
                :trace {:base 2
                        :successful {:msg "give the Runner 1 tag"
                                     :async true
                                     :effect (effect (gain-tags eid 1))}}}]})

(define-card "SDS Drone Deployment"
  {:steal-cost-bonus (req [:program 1])
   :async true
   :effect (req (show-wait-prompt state :runner "Corp to use SDS Drone Deployment")
                (if (seq (all-installed-runner-type state :program))
                  (continue-ability
                    state side
                    {:prompt "Select a program to trash"
                     :label "Trash a program"
                     :msg (msg "trash " (:title target))
                     :choices {:card #(and (installed? %)
                                           (program? %))
                               :all true}
                     :async true
                     :effect (effect (clear-wait-prompt :runner)
                                     (trash eid target nil))}
                    card nil)
                  (do (clear-wait-prompt state :runner)
                      (effect-completed state side eid))))})

(define-card "Self-Destruct Chips"
  {:silent (req true)
   :msg "decrease the Runner's maximum hand size by 1"
   :effect (effect (lose :runner :hand-size 1))
   :swapped {:msg "decrease the Runner's maximum hand size by 1"
             :effect (effect (lose :runner :hand-size 1))}
   :leave-play (effect (gain :runner :hand-size 1))})

(define-card "Sensor Net Activation"
  {:effect (effect (add-counter card :agenda 1))
   :silent (req true)
   :abilities [{:cost [:agenda 1]
                :req (req (some #(and (has-subtype? % "Bioroid") (not (rezzed? %))) (all-installed state :corp)))
                :label "Choose a bioroid to rez, ignoring all costs"
                :prompt "Choose a bioroid to rez, ignoring all costs"
                :choices {:card #(and (has-subtype? % "Bioroid")
                                      (not (rezzed? %)))}
                :msg (msg "rez " (card-str state target) ", ignoring all costs")
                :effect (req (let [c target]
                               (rez state side c {:ignore-cost :all-costs})
                               (register-events
                                 state side card
                                 [{:event :corp-turn-ends
                                   :effect (effect (derez c)
                                                   (unregister-events card))}
                                  {:event :runner-turn-ends
                                   :effect (effect (derez c)
                                                   (unregister-events card))}])))}]
   :events [{:event :corp-turn-ends}
            {:event :runner-turn-ends}]})

(define-card "Sentinel Defense Program"
  {:events [{:event :pre-resolve-damage
             :req (req (and (= target :brain)
                            (pos? (last targets))))
             :msg "do 1 net damage"
             :effect (effect (damage eid :net 1 {:card card}))}]})

(define-card "Show of Force"
  {:async true
   :msg "do 2 meat damage"
   :effect (effect (damage eid :meat 2 {:card card}))})

(define-card "SSL Endorsement"
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
     :events [(assoc remove-credits :event :corp-turn-begins)]
     :flags {:has-events-when-stolen true}}))

(define-card "Standoff"
  (letfn [(stand [side]
            {:async true
             :prompt "Choose one of your installed cards to trash due to Standoff"
             :choices {:card #(and (installed? %)
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
                                    (system-msg state side (str "trashes " (card-str state target) " due to Standoff"))
                                    (clear-wait-prompt state (other-side side))
                                    (show-wait-prompt state side (str (side-str (other-side side)) " to trash a card for Standoff"))
                                    (continue-ability state (other-side side) (stand (other-side side)) card nil)))})]
    {:interactive (req true)
     :async true
     :effect (effect (show-wait-prompt (str (side-str (other-side side)) " to trash a card for Standoff"))
               (continue-ability :runner (stand :runner) card nil))}))

(define-card "Sting!"
  (letfn [(count-opp-stings [state side]
            (count (filter #(= (:title %) "Sting!") (get-in @state [(other-side side) :scored]))))]
    {:msg (msg "deal " (inc (count-opp-stings state :corp)) " net damage")
     :async true
     :effect (effect (damage eid :net (inc (count-opp-stings state :corp)) {:card card}))
     :stolen {:msg (msg "deal " (inc (count-opp-stings state :runner)) " net damage")
              :async true
              :effect (effect (damage eid :net (inc (count-opp-stings state :runner)) {:card card}))}}))

(define-card "Successful Field Test"
  (letfn [(sft [n max-ops]
            {:prompt "Select a card in HQ to install with Successful Field Test"
             :async true
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (in-hand? %))}
             :effect (req (wait-for
                            (corp-install state side target nil {:ignore-all-cost true})
                            (continue-ability state side (when (< n max-ops) (sft (inc n) max-ops)) card nil)))})]
    {:async true
     :msg "install cards from HQ, ignoring all costs"
     :effect (req (let [max-ops (count (filter (complement operation?) (:hand corp)))]
                    (continue-ability state side (sft 1 max-ops) card nil)))}))

(define-card "Superior Cyberwalls"
  (ice-boost-agenda "Barrier"))

(define-card "TGTBT"
  {:flags {:rd-reveal (req true)}
   :access {:msg "give the Runner 1 tag"
            :async true
            :effect (effect (gain-tags eid 1))}})

(define-card "The Cleaners"
  {:events [{:event :pre-damage
             :req (req (and (= target :meat)
                            (= side :corp)))
             :msg "do 1 additional meat damage"
             :effect (effect (damage-bonus :meat 1))}]})

(define-card "The Future is Now"
  {:interactive (req true)
   :prompt "Choose a card to add to HQ"
   :choices (req (:deck corp))
   :msg (msg "add a card from R&D to HQ and shuffle R&D")
   :req (req (pos? (count (:deck corp))))
   :effect (effect (shuffle! :deck)
                   (move target :hand))})

(define-card "The Future Perfect"
  {:flags {:rd-reveal (req true)}
   :access
   {:psi {:req (req (not installed))
          :not-equal {:msg "prevent it from being stolen"
                      :effect (effect (register-run-flag!
                                        card :can-steal
                                        (fn [_ _ c] (not (same-card? c card))))
                                      (effect-completed eid))}}}})

(define-card "Timely Public Release"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 1))
   :abilities [{:cost [:agenda 1]
                :label "Install a piece of ice in any position, ignoring all costs"
                :prompt "Select a piece of ice to install"
                :show-discard true
                :choices {:card #(and (ice? %)
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                :msg (msg "install "
                          (if (and (in-discard? target)
                                   (or (faceup? target)
                                       (not (facedown? target))))
                            (:title target)
                            "ICE")
                          " from " (zone->name (:zone target)))
                :async true
                :effect (effect
                          (continue-ability
                            (let [chosen-ice target]
                              {:prompt "Choose a server"
                               :choices (req servers)
                               :async true
                               :effect (effect
                                         (continue-ability
                                           (let [chosen-server target
                                                 num-ice (count (get-in (:corp @state)
                                                                        (conj (server->zone state target) :ices)))]
                                             {:prompt "Which position to install in? (0 is innermost)"
                                              :choices (vec (reverse (map str (range (inc num-ice)))))
                                              :async true
                                              :effect (req (let [target (Integer/parseInt target)]
                                                             (wait-for (corp-install
                                                                         state side chosen-ice chosen-server
                                                                         {:ignore-all-cost true :index target})
                                                                       (when (and run
                                                                                  (= (zone->name (first (:server run)))
                                                                                     chosen-server))
                                                                         (let [curr-pos (get-in @state [:run :position])]
                                                                           (when (< target curr-pos)
                                                                             (swap! state update-in [:run :position] inc))))
                                                                       (effect-completed state side eid))))})
                                           card nil))})
                            card nil))}]})

(define-card "Transport Monopoly"
  (let [suppress-event {:req (req (and (get-in (get-card state card) [:special :transport-monopoly])
                                       (not (same-card? target card))))}]
    {:silent (req true)
     :effect (effect (add-counter card :agenda 2))
     :abilities [{:cost [:agenda 1]
                  :req (req run)
                  :msg "prevent this run from becoming successful"
                  :effect (effect (update! (assoc-in (get-card state card) [:special :transport-monopoly] true)))}]
     :suppress [(assoc suppress-event :event :pre-successful-run)
                (assoc suppress-event :event :successful-run)]
     :events [{:event :pre-successful-run
               :silent (req true)
               :req (req (get-in (get-card state card) [:special :transport-monopoly]))
               :effect (req (swap! state update-in [:run :run-effects] #(mapv (fn [x] (dissoc x :replace-access)) %))
                            (swap! state update-in [:run] dissoc :successful)
                            (swap! state update-in [:runner :register :successful-run] #(seq (rest %))))}
              {:event :run-ends
               :silent (req true)
               :effect (req (update! state side (dissoc-in (get-card state card) [:special :transport-monopoly])))}]}))

(define-card "Underway Renovation"
  (letfn [(adv4? [s c] (if (>= (get-counters (get-card s c) :advancement) 4) 2 1))]
    {:install-state :face-up
     :events [{:event :advance
               :async true
               :req (req (same-card? card target))
               :msg (msg (if (pos? (count (:deck runner)))
                           (str "trash "
                                (join ", " (map :title (take (adv4? state card) (:deck runner))))
                                " from the Runner's stack")
                           "trash from the Runner's stack but it is empty"))
               :effect (effect (mill :corp eid :runner (adv4? state card)))}]}))

(define-card "Unorthodox Predictions"
  {:implementation "Prevention of subroutine breaking is not enforced"
   :prompt "Choose an ICE type for Unorthodox Predictions"
   :choices ["Barrier" "Code Gate" "Sentry"]
   :msg (msg "prevent subroutines on " target " ICE from being broken until next turn.")
   :effect (effect (effect-completed eid))})

(define-card "Utopia Fragment"
  {:events [{:event :pre-steal-cost
             :req (req (pos? (get-counters target :advancement)))
             :effect (req (let [counter (get-counters target :advancement)]
                            (steal-cost-bonus state side [:credit (* 2 counter)])))}]})

(define-card "Vanity Project"
  ;; No special implementation
  {})

(define-card "Veterans Program"
  {:interactive (req true)
   :msg "lose 2 bad publicity"
   :effect (effect (lose-bad-publicity 2))})

(define-card "Viral Weaponization"
  (let [dmg {:msg "do 1 net damage for each card in the grip"
             :async true
             :effect (req (let [cnt (count (:hand runner))]
                            (unregister-events state side card)
                            (damage state side eid :net cnt {:card card})))}]
    {:effect (effect (register-events
                       card
                       [(assoc dmg :event :corp-turn-ends)
                        (assoc dmg :event :runner-turn-ends)]))
     :events [{:event :corp-turn-ends}
              {:event :runner-turn-ends}]}))

(define-card "Voting Machine Initiative"
  {:silent (req true)
   :effect (effect (add-counter card :agenda 3))
   :events [{:event :runner-turn-begins
             :async true
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
                               card nil))}]})

(define-card "Vulnerability Audit"
  {:derezzed-events
   [{:event :pre-agenda-scored
     :req (req (and (same-card? target card)
                    (= :this-turn (installed? target))))
     :effect (effect (register-turn-flag!
                       card :can-score
                       (fn [state side other-card]
                         (if (same-card? other-card card)
                           ((constantly false)
                            (toast state :corp "Cannot score Vulnerability Audit the turn it was installed." "warning"))
                           true))))}]})

(define-card "Vulcan Coverup"
  {:interactive (req true)
   :msg "do 2 meat damage"
   :effect (effect (damage eid :meat 2 {:card card}))
   :stolen {:msg "force the Corp to take 1 bad publicity"
            :effect (effect (gain-bad-publicity :corp 1))}})

(define-card "Water Monopoly"
  {:constant-effects [{:type :install-cost
                       :req (req (and (resource? target)
                                      (not (has-subtype? target "Virtual"))
                                      (not (:facedown (second targets)))))
                       :value 1}]})
