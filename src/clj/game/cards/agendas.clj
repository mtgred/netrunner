(ns game.cards.agendas
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [steal-cost-bonus]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-corp
                            all-installed-runner-type get-remote-names installable-servers server->zone]]
   [game.core.card :refer [agenda? asset? can-be-advanced?
                           corp-installable-type? corp? facedown? faceup? get-advancement-requirement get-agenda-points
                           get-card get-counters get-title get-zone has-subtype? ice? in-discard? in-hand?
                           in-scored? installed? operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [rez-cost install-cost]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [corp-recur defcard do-net-damage draw-abi gain-credits-ability give-tags
                                  offer-jack-out reorder-choice take-credits get-x-fn]]
   [game.core.drawing :refer [draw draw-up-to]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [checkpoint pay queue-event register-events resolve-ability
                             unregister-events]]
   [game.core.events :refer [first-event? first-run-event? no-event? run-events run-event-count turn-events]]
   [game.core.finding :refer [find-latest]]
   [game.core.flags :refer [in-runner-scored? is-scored? register-run-flag!
                            register-turn-flag! when-scored? zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [get-current-ice update-all-ice update-all-icebreakers]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-msg swap-cards-async]]
   [game.core.moving :refer [forfeit mill move move-zone swap-cards swap-ice
                             trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? ->c]]
   [game.core.prevention :refer [damage-boost preventable? prevent-jack-out]]
   [game.core.prompts :refer [cancellable clear-wait-prompt show-wait-prompt]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez rez-multiple-cards]]
   [game.core.runs :refer [clear-encounter end-run get-current-encounter force-ice-encounter redirect-run start-next-phase]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.servers :refer [is-remote? target-server zone->name]]
   [game.core.set-aside :refer [set-aside-for-me]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [check-win-by-agenda]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defn- add-agenda-point-counters
  "Adds a number of agenda counters to an agenda that checks for a win"
  [state side eid card counters]
  (wait-for (add-counter state side card :agenda counters nil)
            (update-all-agenda-points state side)
            (check-win-by-agenda state side)
            (effect-completed state side eid)))

(defn ice-boost-agenda [subtype]
  (letfn [(count-ice [corp]
            (reduce (fn [c server]
                      (+ c (count (filter #(and (has-subtype? % subtype)
                                                (rezzed? %))
                                          (:ices server)))))
                    0
                    (flatten (seq (:servers corp)))))]
    {:on-score {:msg (msg "gain " (count-ice corp) " [Credits]")
                :interactive (req true)
                :async true
                :effect (effect (gain-credits eid (count-ice corp)))}
     :static-abilities [{:type :ice-strength
                         :req (req (has-subtype? target subtype))
                         :value 1}]}))

(defn- project-agenda
  "Associates an ability which places agenda counters on a scored agenda based on the
  quantity of advancement counters on it when it was scored.
  Args:
  1) :mode        - :printed or :computed - default is :printed.
                      Is the overadvancement based on the PRINTED agenda req, or the computed advancement req?
                      Note that newer nsg cards use the computed req, and older cards use the printed req
  2) :granularity - granularity - gran 2 = 1 quantity every 2 counters. Default is 1:1
  3) :quantity    - quantity per granularity. Ie 3 counters every 2 excess
  4) :type        - counter type. Default is agenda counters."
  ([cdef] (project-agenda nil cdef))
  ([{:keys [mode granularity quantity type]
     :or {granularity 1 quantity 1 type :agenda mode :printed}}
    cdef]
   (assoc cdef
          :on-score
          {:silent (req true)
           :async true
           :effect (req (add-counter
                          state side eid card type
                          (* quantity
                             (quot (max 0 (- (:advancement-tokens context)
                                             (if (= mode :computed)
                                               (:advancement-requirement context)
                                               (:advancementcost card))))
                                   granularity))))})))

(defn- agenda-counters
  ([qty] (agenda-counters qty :agenda))
  ([qty ctype]
   {:effect (req (add-counter state side eid card ctype qty nil))
    :async true
    :silent (req true)}))

;; Card definitions

(defcard "15 Minutes"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :msg "shuffle itself into R&D"
                :label "Shuffle this agenda into R&D"
                :effect (effect (move :corp card :deck nil)
                                (shuffle! :corp :deck)
                                (update-all-agenda-points))}]
   :flags {:has-abilities-when-stolen true}})

(defcard "Above the Law"
  {:on-score
   {:interactive (req true)
    :prompt "Choose a resource to trash"
    :req (req (some #(and (installed? %)
                          (resource? %))
                    (all-active-installed state :runner)))
    :choices {:card #(and (installed? %)
                          (resource? %))}
    :msg (msg "trash " (card-str state target))
    :async true
    :effect (effect (trash eid target {:cause-card card}))}})

(defcard "Accelerated Beta Test"
  (letfn [(abt [choices]
            {:async true
             :prompt "Choose a card to install and rez at no cost"
             :choices (cancellable (filter ice? choices) :sorted)
             :cancel-effect (effect (unregister-events card)
                                    (system-msg (str "declines to use " (get-title card) " to install any of the top 3 cards or R&D"))
                                    (trash-cards eid choices {:unpreventable true :cause-card card}))
             :effect (req (wait-for (corp-install state side target nil
                                                  {:ignore-all-cost true
                                                   :install-state :rezzed-no-cost
                                                   :msg-keys {:install-source card
                                                              :display-origin true}})
                                    (let [choices (remove-once #(= target %) choices)]
                                      (cond
                                        ;; Shuffle ends the ability
                                        (get-in (get-card state card) [:special :shuffle-occurred])
                                        (do (unregister-events state side card)
                                            (trash-cards state side eid choices {:unpreventable true :cause-card card}))
                                        ;; There are still cards left
                                        (seq choices)
                                        (continue-ability state side (abt choices) card nil)
                                        ;; Trash what's left
                                        :else
                                        (do (unregister-events state side card)
                                            (effect-completed state side eid))))))})]
    {:on-score
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
                  (let [choices (take 3 (:deck corp))]
                    (wait-for
                      (resolve-ability state side
                                       {:async true
                                        :prompt (str "The top cards of R&D are (top->bottom): "
                                                     (enumerate-cards choices))
                                        :choices ["OK"]}
                                       card nil)
                      (continue-ability state side (abt choices) card nil))))}}}}))

(defcard "Advanced Concept Hopper"
  {:events
   [{:event :run
     :req (req (first-event? state side :run))
     :async true
     :waiting-prompt true
     :prompt "Choose one"
     :choices ["Draw 1 card" "Gain 1 [Credits]" "No action"]
     :effect (req (case target
                    "Gain 1 [Credits]"
                    (do (system-msg state :corp (str "uses " (:title card) " to gain 1 [Credits]"))
                        (gain-credits state :corp eid 1))
                    "Draw 1 card"
                    (do (system-msg state :corp (str "uses " (:title card) " to draw 1 card"))
                        (draw state :corp eid 1))
                    "No action"
                    (do (system-msg state :corp (str "declines to use " (:title card)))
                        (effect-completed state side eid))))}]})

(defcard "Aggressive Trendsetting"
  {:events [{:event :runner-trash
             :interactive (req true)
             :once-per-instance true
             :optional {:req (req (letfn [(valid-ctx? [contexts]
                                            (some (every-pred installed? corp?) (map :card contexts)))]
                                    (and (valid-ctx? targets)
                                         (= :runner (:active-player @state))
                                         (first-event? state side :runner-trash valid-ctx?))))
                        :player :runner
                        :prompt "Spend [click] to prevent the corporation having +1 allotted [click] during their next turn?"
                        :yes-ability {:cost [(->c :click 1)]
                                      :display-side :runner
                                      :msg :cost}
                        :no-ability {:display-side :corp
                                     :msg (msg "gain [Click] during their next turn")
                                     :effect (req (register-events
                                                    state side card
                                                    [{:event :corp-turn-begins
                                                      :unregister-once-resolved true
                                                      :duration :until-corp-turn-begins
                                                      :effect (effect (gain-clicks :corp 1))}]))}}}]})

(defcard "Ancestral Imager"
  {:events [{:event :jack-out
             :msg "do 1 net damage"
             :async true
             :effect (effect (damage eid :net 1 {:card card}))}]})

(defcard "AR-Enhanced Security"
  {:events [{:event :runner-trash
             :async true
             :interactive (req true)
             :once-per-instance true
             :req (req (letfn [(valid-ctx? [evs] (some #(corp? (:card %)) evs))]
                         (and (valid-ctx? targets)
                              (first-event? state side :runner-trash valid-ctx?))))
             :msg "give the Runner a tag"
             :effect (effect (gain-tags eid 1))}]})

(defcard "Architect Deployment Test"
  {:on-score
   {:interactive (req true)
    :async true
    :msg "look at the top 5 cards of R&D"
    ;; this is explicitly so the prompt shows up before we look at the menu
    :effect (req (continue-ability
                   state side
                   {:prompt (msg "The top cards of R&D are (top->bottom): "
                                 (enumerate-cards (take 5 (:deck corp))))
                    :choices ["OK"]
                    :async true
                    :req (req (not-empty (:deck corp)))
                    :effect (effect (continue-ability
                                      {:prompt "Choose a card to install"
                                       :choices (cancellable (filter corp-installable-type?
                                                                     (take 5 (:deck corp))))
                                       :async true
                                       :effect (req (let [target-position (first (positions #{target} (take 5 (:deck corp))))]
                                                      (corp-install state side
                                                        eid target nil
                                                        {:ignore-all-cost true
                                                         :msg-keys {:install-source card
                                                                    :origin-index target-position
                                                                    :display-origin true}
                                                         :install-state :rezzed-no-cost})))
                                       :cancel-effect
                                       (effect (system-msg
                                                 (str "declines to use "
                                                      (get-title card)
                                                      " to install any of the top 5 cards of R&D"))
                                               (effect-completed eid))}
                                      card nil))}
                   card nil))}})

(defcard "Armed Intimidation"
  {:on-score
   {:player :runner
    :interactive (req true)
    :async true
    :waiting-prompt true
    :prompt "Choose one"
    :choices ["Suffer 5 meat damage" "Take 2 tags"]
    :msg (msg "force the Runner to " (decapitalize target))
    :effect (req (if (= target "Take 2 tags")
                   (gain-tags state :runner eid 2 {:card card})
                   (damage state :runner eid :meat 5 {:card card :unboostable true})))}})

(defcard "Armored Servers"
  {:on-score (agenda-counters 1)
   :abilities [{:cost [(->c :agenda 1)]
                :req (req run)
                :label "increase cost to break subroutines or jack out"
                :msg "make the Runner trash a card from the grip as an additional cost to jack out or break subroutines for the remainder of the run"
                :effect (effect (register-lingering-effect
                                  card
                                  {:type :break-sub-additional-cost
                                   :duration :end-of-run
                                   :value (req (repeat (count (:broken-subs (:ability context)))
                                                       (->c :trash-from-hand 1)))})
                                (register-lingering-effect
                                  card
                                  {:type :jack-out-additional-cost
                                   :duration :end-of-run
                                   :value (->c :trash-from-hand 1)}))}]})

(defcard "Artificial Cryptocrash"
  {:on-score
   {:async true
    :msg "make the Runner lose 7 [Credits]"
    :effect (effect (lose-credits :runner eid 7))}})

(defcard "AstroScript Pilot Program"
  {:on-score (agenda-counters 1)
   :abilities [{:cost [(->c :agenda 1)]
                :label "place 1 advancement counter"
                :msg (msg "place 1 advancement counter on " (card-str state target))
                :choices {:req (req (can-be-advanced? state target))}
                :async true
                :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}]})

(defcard "Award Bait"
  {:flags {:rd-reveal (req true)}
   :on-access {:async true
               :req (req (not-empty (filter #(can-be-advanced? state %) (all-installed state :corp))))
               :waiting-prompt true
               :prompt "How many advancement tokens do you want to place?"
               :choices ["0" "1" "2"]
               :effect (effect (continue-ability
                                 (let [c (str->int target)]
                                   {:choices {:req (req (can-be-advanced? state target))}
                                    :msg (msg "place " (quantify c "advancement token")
                                              " on " (card-str state target))
                                    :async true
                                    :effect (req (add-prop state :corp eid target :advance-counter c {:placed true}))})
                                 card nil))}})

(defcard "Azef Protocol"
  {:additional-cost [(->c :trash-other-installed 1)]
   :on-score {:async true
              :msg "do 2 meat damage"
              :effect (effect (damage eid :meat 2 {:card card}))}})

(defcard "Bacterial Programming"
  (letfn [(hq-step [remaining to-trash to-hq]
            {:async true
             :prompt "Choose a card to move to HQ"
             :choices (conj (vec remaining) "Done")
             :effect (req (if (= "Done" target)
                            (wait-for (trash-cards state :corp to-trash {:unpreventable true :cause-card card})
                                      (doseq [h to-hq]
                                        (move state :corp h :hand))
                                      (do
                                        (system-msg state :corp
                                                    (str "uses " (:title card)
                                                         " to trash " (quantify (count to-trash) "card")
                                                         ", add " (quantify (count to-hq) "card")
                                                         " to HQ, and arrange the top " (quantify (- 7 (count to-trash) (count to-hq)) "card") " of R&D"))
                                        (if (seq remaining)
                                          (continue-ability state :corp (reorder-choice :corp (vec remaining)) card nil)
                                          (effect-completed state :corp eid))))
                            (continue-ability state :corp (hq-step
                                                            (set/difference (set remaining) (set [target]))
                                                            to-trash
                                                            (conj to-hq target)) card nil)))})
          (trash-step [remaining to-trash]
            {:async true
             :prompt "Choose a card to discard"
             :choices (conj (vec remaining) "Done")
             :effect (req (if (= "Done" target)
                            (continue-ability state :corp (hq-step remaining to-trash '()) card nil)
                            (continue-ability state :corp (trash-step
                                                            (set/difference (set remaining) (set [target]))
                                                            (conj to-trash target)) card nil)))})]
    (let [arrange-rd
          {:interactive (req true)
           :optional
           {:waiting-prompt true
            :prompt "Look at the top 7 cards of R&D?"
            :yes-ability
            {:async true
             :msg "look at the top 7 cards of R&D"
             :effect (req (let [c (take 7 (:deck corp))]
                            (when (and
                                   (:access @state)
                                   (:run @state))
                              (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                            (continue-ability state :corp (trash-step c '()) card nil)))}}}]
      {:on-score arrange-rd
       :stolen arrange-rd})))

(defcard "The Basalt Spire"
  {:on-score (agenda-counters 2)
   :stolen {:async true
            :effect (effect (continue-ability (corp-recur) card nil))}
   :flags {:has-abilities-when-stolen true}
   :abilities [{:label "Choose a card to add to HQ"
                :cost [(->c :trash-from-deck 1) (->c :agenda 1)]
                :once :per-turn
                :msg "add 1 card from Archives to HQ"
                :async true
                :effect (effect (continue-ability (corp-recur) card nil))}]})

(defcard "Bellona"
  {:steal-cost-bonus (req [(->c :credit 5)])
   :on-score (gain-credits-ability 5)})

(defcard "Better Citizen Program"
  {:events [{:event :play-event
             :optional
             {:req (req (and (has-subtype? (:card context) "Run")
                             (first-event? state :runner :play-event #(has-subtype? (:card (first %)) "Run"))
                             (no-event? state :runner :runner-install #(has-subtype? (:card (first %)) "Icebreaker"))))
              :waiting-prompt true
              :prompt "Give the runner 1 tag?"
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability
              {:async true
               :msg "give the Runner a tag for playing a run event"
               :effect (effect (gain-tags :corp eid 1))}}}
            {:event :runner-install
             :silent (req true)
             :optional
             {:req (req (and (not (:facedown context))
                             (has-subtype? (:card context) "Icebreaker")
                             (first-event? state :runner :runner-install #(has-subtype? (:card (first %)) "Icebreaker"))
                             (no-event? state :runner :play-event #(has-subtype? (:card (first %)) "Run"))))
              :waiting-prompt true
              :prompt "Give the runner 1 tag?"
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability
              {:async true
               :msg "give the Runner a tag for installing an icebreaker"
               :effect (effect (gain-tags :corp eid 1))}}}]
   :abilities [(set-autoresolve :auto-fire "Better Citizen Program")]})

(defcard "Bifrost Array"
  {:on-score
   {:optional
    {:req (req (seq (filter #(not= (:title %) "Bifrost Array") (:scored corp))))
     :prompt "Trigger the ability of a scored agenda?"
     :yes-ability
     {:prompt "Choose an agenda to trigger its \"when scored\" ability"
      :choices {:card #(and (agenda? %)
                            (not= (:title %) "Bifrost Array")
                            (in-scored? %)
                            (when-scored? %))}
      :msg (msg "trigger the \"when scored\" ability of " (:title target))
      :async true
      :effect (effect (continue-ability (:on-score (card-def target)) target nil))}}}})

(defcard "Blood in the Water"
  {:x-fn (req (count (:hand runner)))
   :advancement-requirement (get-x-fn)})

(defcard "Brain Rewiring"
  {:on-score
   {:optional
    {:waiting-prompt true
     :prompt "Pay credits to add random cards from the grip to the bottom of the stack?"
     :yes-ability
     {:prompt "How many credits do you want to pay?"
      :choices {:number (req (min (:credit corp)
                                  (count (:hand runner))))}
      :async true
      :effect (req (if (pos? target)
                     (wait-for
                       (pay state :corp (make-eid state eid) card (->c :credit target))
                       (let [from (take target (shuffle (:hand runner)))]
                         (doseq [c from]
                           (move state :runner c :deck))
                         (system-msg state side (str "uses " (:title card) " to pay " target
                                                     " [Credits] and add " (quantify target "card")
                                                     " from the grip"
                                                     " to the bottom of the stack."
                                                     " The Runner draws 1 card"))
                         (queue-event state :runner-hand-changed?)
                         (wait-for
                           (checkpoint state side)
                           (draw state :runner eid 1))))
                     (effect-completed state side eid)))}}}})

(defcard "Braintrust"
  (project-agenda {:granularity 2})
  {:static-abilities [{:type :rez-cost
                       :req (req (ice? target))
                       :value (req (- (get-counters card :agenda)))}]})

(defcard "Breaking News"
  {:on-score (give-tags 2)
   :events (let [event {:unregister-once-resolved true
                        :req (effect (first-event? :agenda-scored #(same-card? card (:card (first %)))))
                        :msg "make the Runner lose 2 tags"
                        :effect (effect (lose :runner :tag 2))}]
             [(assoc event :event :corp-turn-ends)
              (assoc event :event :runner-turn-ends)])})

(defcard "Broad Daylight"
  (letfn [(agenda-counters [state side card eid]
            (add-counter state :corp eid card :agenda (count-bad-pub state) nil))]
    {:on-score
     {:optional
      {:prompt "Take 1 bad publicity?"
       :yes-ability {:async true
                     :msg "take 1 bad publicity"
                     :effect (req (wait-for (gain-bad-publicity state :corp 1)
                                            (agenda-counters state side card eid)))}
       :no-ability {:async true
                    :effect (effect (agenda-counters card eid))}}}
     :abilities [{:action true
                  :cost [(->c :click 1) (->c :agenda 1)]
                  :async true
                  :label "Do 2 meat damage"
                  :once :per-turn
                  :msg "do 2 meat damage"
                  :effect (effect (damage eid :meat 2 {:card card}))}]}))

(defcard "CFC Excavation Contract"
  (letfn [(bucks [state]
            (->> (all-active-installed state :corp)
                 (filter #(has-subtype? % "Bioroid"))
                 (count)
                 (* 2)))]
    {:on-score
     {:async true
      :msg (msg "gain " (bucks state) " [Credits]")
      :effect (effect (gain-credits :corp eid (bucks state)))}}))

(defcard "Character Assassination"
  {:on-score
   {:prompt "Choose a resource to trash"
    :choices {:card #(and (installed? %)
                          (resource? %))}
    :msg (msg "trash " (:title target))
    :interactive (req true)
    :async true
    :effect (effect (trash eid target {:unpreventable true :cause-card card}))}})

(defcard "Chronos Project"
  {:on-score
   {:req (req (not (zone-locked? state :runner :discard)))
    :msg "remove all cards in the heap from the game"
    :interactive (req true)
    :effect (effect (move-zone :runner :discard :rfg))}})

(defcard "City Works Project"
  (letfn [(meat-damage [s c] (+ 2 (get-counters (get-card s c) :advancement)))]
    {:install-state :face-up
     :on-access {:req (req installed)
                 :msg (msg "do " (meat-damage state card) " meat damage")
                 :async true
                 :effect (effect (damage eid :meat (meat-damage state card) {:card card}))}}))

(defcard "Clone Retirement"
  {:on-score {:msg "remove 1 bad publicity"
              :effect (effect (lose-bad-publicity 1))
              :silent (req true)}
   :stolen {:msg "force the Corp to take 1 bad publicity"
            :effect (effect (gain-bad-publicity :corp 1))}})

(defcard "Corporate Oversight A"
  {:on-score
   {:interactive (req true)
    :optional
    {:prompt "Search R&D for a piece of ice to install protecting a remote server?"
     :yes-ability
     {:async true
      :effect (effect
                (continue-ability
                  (if (not-empty (filter ice? (:deck corp)))
                    {:async true
                     :prompt "Choose a piece of ice"
                     :choices (req (filter ice? (:deck corp)))
                     :effect
                     (effect
                       (continue-ability
                         (let [chosen-ice target]
                           {:async true
                            :prompt (str "Choose a server to install " (:title chosen-ice) " on")
                            :choices (filter #(not (#{"HQ" "Archives" "R&D"} %))
                                             (installable-servers state chosen-ice))
                            :effect (effect (shuffle! :deck)
                                            (corp-install eid chosen-ice target
                                                          {:ignore-all-cost true
                                                           :install-state :rezzed-no-cost}))})
                         card nil))}
                    {:prompt "You have no ice in R&D"
                     :choices ["Carry on!"]
                     :prompt-type :bogus
                     :effect (effect (shuffle! :deck))})
                  card nil))}}}})

(defcard "Corporate Oversight B"
  {:on-score
   {:interactive (req true)
    :optional
    {:prompt "Search R&D for a piece of ice to install protecting a central server?"
     :yes-ability
     {:async true
      :effect (effect
                (continue-ability
                  (if (not-empty (filter ice? (:deck corp)))
                    {:async true
                     :prompt "Choose a piece of ice"
                     :choices (req (filter ice? (:deck corp)))
                     :effect
                     (effect
                       (continue-ability
                         (let [chosen-ice target]
                           {:async true
                            :prompt (str "Choose a server to install " (:title chosen-ice) " on")
                            :choices (filter #(#{"HQ" "Archives" "R&D"} %)
                                             (installable-servers state chosen-ice))
                            :effect (effect (shuffle! :deck)
                                            (corp-install eid chosen-ice target
                                                          {:ignore-all-cost true
                                                           :install-state :rezzed-no-cost}))})
                         card nil))}
                    {:prompt "You have no ice in R&D"
                     :choices ["Carry on!"]
                     :prompt-type :bogus
                     :effect (effect (shuffle! :deck))})
                  card nil))}}}})

(defcard "Corporate Sales Team"
  (let [e {:req (req (pos? (get-counters card :credit)))
           :msg "gain 1 [Credits]"
           :automatic :gain-credits
           :async true
           :effect (req (take-credits state side eid card :credit 1))}]
    {:on-score (agenda-counters 10 :credit)
     :events [(assoc e :event :runner-turn-begins)
              (assoc e :event :corp-turn-begins)]}))

(defcard "Corporate War"
  {:on-score
   {:msg (msg (if (> (:credit corp) 6) "gain 7 [Credits]" "lose all credits"))
    :interactive (req true)
    :async true
    :effect (req (if (> (:credit corp) 6)
                   (gain-credits state :corp eid 7)
                   (lose-credits state :corp eid :all)))}})

(defcard "Crisis Management"
  (let [ability {:req (req tagged)
                 :async true
                 :label "Do 1 meat damage (start of turn)"
                 :automatic :corp-damage
                 :once :per-turn
                 :msg "do 1 meat damage"
                 :effect (effect (damage eid :meat 1 {:card card}))}]
    {:events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Cyberdex Sandbox"
  {:on-score {:optional
              {:prompt "Purge virus counters?"
               :yes-ability {:msg "purge virus counters"
                             :async true
                             :effect (effect (purge eid))}}}
   :events [{:event :purge
             :req (req (first-event? state :corp :purge))
             :msg "gain 4 [Credits]"
             :async true
             :effect (req (gain-credits state :corp eid 4))}]})

(defcard "Dedicated Neural Net"
  {:events [{:event :successful-run
             :interactive (req true)
             :psi {:req (req (= :hq (target-server context))
                             (first-event? state side :successful-run
                                           #(= :hq (target-server (first %)))))
                   :not-equal {:async true
                               :effect (effect (register-lingering-effect
                                                 card
                                                 {:type :corp-choose-hq-access
                                                  :duration :end-of-run
                                                  :value true})
                                               (effect-completed eid))}}}]})

(defcard "Degree Mill"
  {:steal-cost-bonus (req [(->c :shuffle-installed-to-stack 2)])})

(defcard "Director Haas' Pet Project"
  (letfn [(install-ability [server-name n]
            {:prompt "Choose a card to install"
             :show-discard true
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (or (in-hand? %)
                                       (in-discard? %)))}
             :async true
             :effect (req (wait-for (corp-install state side target server-name {:ignore-all-cost true
                                                                                 :msg-keys {:install-source card
                                                                                            :display-origin true}})
                                    (continue-ability state side
                                                      (when (< n 2)
                                                        (install-ability (last (get-remote-names state)) (inc n)))
                                                      card nil)))})]
    {:on-score
     {:optional
      {:prompt "Install cards in a new remote server?"
       :yes-ability (install-ability "New remote" 0)}}}))

(defcard "Divested Trust"
  {:events
   [{:event :agenda-stolen
     :async true
     :interactive (req true)
     :effect (req (if (:winner @state)
                    (effect-completed state side eid)
                    (let [card (find-latest state card)
                          stolen-agenda (find-latest state (:card context))
                          title (get-title stolen-agenda)
                          prompt (str "Forfeit Divested Trust to add " title
                                      " to HQ and gain 5 [Credits]?")
                          message (str "add " title " to HQ and gain 5 [Credits]")
                          card-side (if (in-runner-scored? state side card)
                                      :runner :corp)]
                      (continue-ability
                        state side
                        {:optional
                         {:waiting-prompt true
                          :prompt prompt
                          :yes-ability
                          {:msg message
                           :async true
                           :effect (req (wait-for (forfeit state card-side (make-eid state eid) card)
                                                  (move state side stolen-agenda :hand)
                                                  (update-all-agenda-points state side)
                                                  (gain-credits state side eid 5)))}}}
                        card nil))))}]})

(defcard "Domestic Sleepers"
  {:agendapoints-corp (req (if (pos? (get-counters card :agenda)) 1 0))
   :abilities [{:action true
                :cost [(->c :click 3)]
                :msg "place 1 agenda counter on itself"
                :async true
                :effect (effect (add-agenda-point-counters eid card 1))}]})

(defcard "Élivágar Bifurcation"
  {:on-score
   {:interactive (req true)
    :waiting-prompt true
    :prompt "Choose a card to derez"
    :choices {:card #(rezzed? %)}
    :async true
    :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                           (effect-completed eid))
    :effect (req (derez state side eid target))}})

(defcard "Eden Fragment"
  {:static-abilities [{:type :ignore-install-cost
                       :req (req (and (ice? target)
                                      (->> (turn-events state side :corp-install)
                                           (map #(:card (first %)))
                                           (filter ice?)
                                           empty?)))
                       :value true}]
   :events [{:event :corp-install
             :req (req (and (ice? target)
                            (->> (turn-events state side :corp-install)
                                 (map #(:card (first %)))
                                 (filter ice?)
                                 empty?)))
             :msg "ignore the install cost of the first piece of ice this turn"}]})

(defcard "Efficiency Committee"
  {:on-score (agenda-counters 3)
   :abilities [{:action true
                :cost [(->c :click 1) (->c :agenda 1)]
                :effect (effect (gain-clicks 2)
                                (register-turn-flag!
                                  card :can-advance
                                  (constantly false)))
                :keep-menu-open :while-agenda-tokens-left
                :msg "gain [Click][Click]"}]})

(defcard "Elective Upgrade"
  {:on-score (agenda-counters 2)
   :abilities [{:action true
                :cost [(->c :click 1) (->c :agenda 1)]
                :once :per-turn
                :effect (effect (gain-clicks 2))
                :msg "gain [Click][Click]"}]})

(defcard "Embedded Reporting"
  (project-agenda {:quantity 2 :mode :computed})
  {:events [{:event :corp-turn-ends
             :interactive (req true)
             :skippable true
             :optional {:prompt "Search R&D for an Operation?"
                        :waiting-prompt true
                        :req (req (and (pos? (get-counters card :agenda))
                                       (seq (:deck corp))))
                        :yes-ability {:choices (req (cancellable (filter operation? (:deck corp)) :sorted))
                                      :prompt "Move an operation to the top of R&D"
                                      :async true
                                      :msg (msg "reveal " (:title target)
                                                " from R&D, shuffle R&D, and place it ontop")
                                      :cost [(->c :agenda 1)]
                                      :effect (req (wait-for
                                                     (reveal state side target)
                                                     (let [c (first (set-aside-for-me state side eid [target]))]
                                                       (shuffle! state side :deck)
                                                       (move state side c :deck {:front true}))
                                                     (effect-completed state side eid)))
                                      :cancel-effect (req (continue-ability
                                                            state side
                                                            {:cost [(->c :agenda 1)]
                                                             :msg "shuffle R&D"
                                                             :effect (req (shuffle! state side :deck))}
                                                            card nil))}}}]})

(defcard "Eminent Domain"
  (let [expend-abi {:req (req (some corp-installable-type? (:hand corp)))
                    :cost [(->c :credit 1)]
                    :prompt "Choose 1 card to install and rez, paying 5 [Credits] less"
                    :choices {:req (req (and (in-hand? target)
                                             (corp-installable-type? target)
                                             (not (same-card? card target))))}
                    :msg "install and rez 1 card from HQ, paying 5 [Credits] less"
                    :async true
                    :effect (req (corp-install state side eid target nil
                                               {:install-state :rezzed
                                                :msg-keys {:install-source card
                                                           :display-origin true}
                                                :combined-credit-discount 5}))}
        score-abi {:interactive (req true)
                   :optional
                   {:prompt "Search R&D for 1 card to install and rez, ignoring all costs?"
                    :yes-ability
                    {:async true
                     :effect (effect
                               (continue-ability
                                 {:async true
                                  :prompt "Choose a card to install"
                                  :choices (req (concat
                                                  (->> (:deck corp)
                                                       (filter #(corp-installable-type? %))
                                                       (sort-by :title)
                                                       (seq))
                                                  ["Done"]))
                                  :effect (req (shuffle! state side :deck)
                                               (if (= "Done" target)
                                                 (do (system-msg state side (str "uses " (:title card) " to shuffle R&D"))
                                                     (effect-completed state side eid))
                                                 (corp-install state side eid target nil
                                                               {:install-state :rezzed-no-cost
                                                                :msg-keys {:install-source card
                                                                           :display-origin true}
                                                                :ignore-all-cost true})))}
                                 card nil))}}}]
    {:on-score score-abi
     :expend expend-abi}))

(defcard "Encrypted Portals"
  (ice-boost-agenda "Code Gate"))

(defcard "Escalate Vitriol"
  {:abilities [{:action true
                :label "Gain 1 [Credit] for each Runner tag"
                :change-in-game-state {:req (req tagged)}
                :cost [(->c :click 1)]
                :once :per-turn
                :msg (msg "gain " (count-tags state) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (count-tags state)))}]})

(defcard "Executive Retreat"
  {:on-score {:async true
              :effect (req (shuffle-into-deck state side :hand)
                           (add-counter state side eid card :agenda 1 nil))
              :interactive (req true)}
   :abilities [(draw-abi 5 nil {:action true
                                :cost [(->c :click 1) (->c :agenda 1)]
                                :keep-menu-open :while-agenda-tokens-left})]})

(defcard "Explode-a-palooza"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:waiting-prompt true
                :prompt "Gain 5 [Credits]?"
                :yes-ability (gain-credits-ability 5)}}})

(defcard "Evidence Collection"
  {:events [{:event :win
             :req (req (= :corp (:winner context)))
             :msg "reveal set 2"}]})

(defcard "Evidence Collection 2"
  {:events [{:event :win
             :req (req (= :corp (:winner context)))
             :msg "reveal set 5"}]})

(defcard "Evidence Collection 3"
  {:events [{:event :win
             :req (req (= :corp (:winner context)))
             :msg "reveal set 8"}]})

(defcard "Evidence Collection 4"
  {:agendapoints-runner (req 1)})

(defcard "False Lead"
  (let [ab {:change-in-game-state {:req (req (<= 2 (:click runner)))}
            :label "runner loses [Click][Click]"
            :msg "force the Runner to lose [Click][Click]"
            :cost [(->c :forfeit-self)]
            :effect (effect (lose-clicks :runner 2))}]
    ;; this even doesn't correlate 1:1 with the card text -
    ;; this exists so you can avoid needing to click this before the runner clicks a basic action
    ;; (ie click racing your opponent).
    {:events [{:event :post-runner-turn-begins
               :optional {:req (req (case (get-in card [:special :ask-when-runner-turn-starts] nil)
                                      "Always" true
                                      "When tagged" tagged
                                      nil))
                          :prompt "Fire False Lead?"
                          :waiting-prompt true
                          :yes-ability ab}}]
     :abilities [ab
                 {:label "Ask when runner turn begins?"
                  :prompt "Ask to use False Lead after the Runner turn begins?"
                  :choices ["Always" "Never" "When tagged"]
                  :effect (req (update! state side (assoc-in card [:special :ask-when-runner-turn-starts] target))
                               (toast state :corp (str "False Lead prompt set to: " target) "warning"))}]}))

(defcard "Fetal AI"
  {:flags {:rd-reveal (req true)}
   :on-access (assoc (do-net-damage 2) :req (req (not (in-discard? card))))
   :steal-cost-bonus (req [(->c :credit 2)])})

(defcard "Firmware Updates"
  {:on-score (agenda-counters 3)
   :abilities [{:cost [(->c :agenda 1)]
                :label "Place 1 advancement counter"
                :choices {:req (req (and (ice? target)
                                         (can-be-advanced? state target)))}
                :req (req (pos? (get-counters card :agenda)))
                :msg (msg "place 1 advancement counter on " (card-str state target))
                :once :per-turn
                :async true
                :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}]})

(defcard "Flower Sermon"
  {:on-score (agenda-counters 5)
   :abilities [{:cost [(->c :agenda 1)]
                :label "Reveal the top card of R&D and draw 2 cards"
                :once :per-turn
                :msg (msg "reveal " (:title (first (:deck corp))) " and draw 2 cards")
                :async true
                :waiting-prompt true
                :effect (req (wait-for
                               (reveal state side (first (:deck corp)))
                               (wait-for
                                 (draw state side 2)
                                 (continue-ability
                                   state side
                                   {:req (req (pos? (count (:hand corp))))
                                    :prompt "Choose a card in HQ to move to the top of R&D"
                                    :msg "add 1 card in HQ to the top of R&D"
                                    :choices {:card #(and (in-hand? %)
                                                          (corp? %))}
                                    :effect (effect (move target :deck {:front true}))}
                                   card nil))))}]})

(defcard "Fly on the Wall"
  {:on-score (give-tags 1)})

(defcard "Freedom of Information"
  {:advancement-requirement (req (- (count-tags state)))})

(defcard "Fujii Asset Retrieval"
  (let [ability {:async true
                 :interactive (req true)
                 :msg "do 2 net damage"
                 :effect (effect (damage eid :net 2 {:card card}))}]
    {:stolen ability
     :on-score ability}))

(defcard "Genetic Resequencing"
  {:on-score {:choices {:card in-scored?}
              :msg (msg "place 1 agenda counter on " (:title target))
              :async true
              :effect (req (wait-for (add-counter state side target :agenda 1 nil)
                                     (update-all-agenda-points state)
                                     (effect-completed state side eid)))
              :silent (req true)}})

(defcard "Geothermal Fracking"
  {:on-score (agenda-counters 2)
   :abilities [{:action true
                :cost [(->c :click 1) (->c :agenda 1)]
                :msg "gain 7 [Credits] and take 1 bad publicity"
                :async true
                :keep-menu-open :while-agenda-tokens-left
                :effect (req (wait-for (gain-credits state side 7)
                                       (gain-bad-publicity state side eid 1)))}]})

(defcard "Gila Hands Arcology"
  {:abilities [{:action true
                :cost [(->c :click 2)]
                :msg "gain 3 [Credits]"
                :async true
                :keep-menu-open :while-2-clicks-left
                :effect (effect (gain-credits eid 3))}]})

(defcard "Glenn Station"
  {:abilities [{:action true
                :label "Host a card from HQ"
                :change-in-game-state {:req (req (and (not-empty (:hand corp))
                                                      (empty? (filter corp? (:hosted card)))))}
                :cost [(->c :click 1)]
                :msg "host a card from HQ"
                :prompt "Choose a card to host"
                :choices {:card #(and (corp? %) (in-hand? %))}
                :effect (effect (host card target {:facedown true}))}
               {:action true
                :label "Add a hosted card to HQ"
                :change-in-game-state {:req (req (not-empty (filter corp? (:hosted card))))}
                :cost [(->c :click 1)]
                :msg "add a hosted card to HQ"
                :prompt "Choose a hosted card"
                :choices {:all true
                          :req (req (let [hosted-corp-cards
                                          (->> (:hosted card)
                                               (filter corp?)
                                               (map :cid)
                                               (into #{}))]
                                      (hosted-corp-cards (:cid target))))}
                :effect (effect (move target :hand))}]})

(defcard "Global Food Initiative"
  {:agendapoints-runner (req 2)})

(defcard "Government Contracts"
  {:abilities [(assoc (gain-credits-ability 4)
                      :action true
                      :cost [(->c :click 2)]
                      :keep-menu-open :while-2-clicks-left)]})

(defcard "Government Takeover"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :async true
                :keep-menu-open :while-clicks-left
                :effect (effect (gain-credits eid 3))
                :msg "gain 3 [Credits]"}]})

(defcard "Graft"
  (letfn [(graft [n] {:prompt "Choose a card to add to HQ"
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
    {:on-score
     {:async true
      :msg "add up to 3 cards from R&D to HQ"
      :effect (effect (continue-ability (graft 1) card nil))}}))

(defcard "Greenmail"
  ;; "When you score this agenda, gain 2{c}.
  ;; {interrupt} → When you would
  ;; forfeit this agenda, gain 4{c}."
  {:on-score   (gain-credits-ability 2)
   :on-forfeit (gain-credits-ability 4)})

(defcard "Hades Fragment"
  (let [abi {:prompt "Choose a card to add to the bottom of R&D"
             :label "add card to bottom of R&D"
             :show-discard true
             :event :corp-turn-begins
             :once :per-turn
             :choices {:card #(and (corp? %)
                                   (in-discard? %))}
             :effect (effect (move target :deck))
             :msg (msg "add "
                       (if (:seen target)
                         (:title target)
                         "a card")
                       " to the bottom of R&D")}]
    {:flags {:corp-phase-12 (req (and (seq (:discard corp))
                                      (is-scored? state :corp card)))}
     :abilities [abi]
     :events [(assoc abi
                     :change-in-game-state {:req (req (seq (:discard corp))) :silent (req true)})]}))

(defcard "Helium-3 Deposit"
  {:on-score
   {:async true
    :interactive (req true)
    :prompt "How many power counters do you want to place?"
    :choices ["0" "1" "2"]
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:card #(pos? (get-counters % :power))}
                      :msg (msg "place " (quantify c "power counter") " on " (:title target))
                      :async true
                      :effect (req (add-counter state side eid target :power c nil))}
                     card nil)))}})

(defcard "High-Risk Investment"
  {:on-score (agenda-counters 1)
   :abilities [{:action true
                :cost [(->c :click 1) (->c :agenda 1)]
                :change-in-game-state {:req (req (pos? (:credit runner)))}
                :label "gain credits"
                :msg (msg "gain " (:credit runner) " [Credits]")
                :async true
                :keep-menu-open :while-agenda-tokens-left
                :effect (effect (gain-credits eid (:credit runner)))}]})

(defcard "Hollywood Renovation"
  {:install-state :face-up
   :events [{:event :advance
             :condition :faceup
             :async true
             :req (req (same-card? card (:card context)))
             :effect (req (let [n (if (>= (get-counters (get-card state card) :advancement) 6) 2 1)]
                            (continue-ability
                              state side
                              {:choices {:not-self true
                                         :req (req (can-be-advanced? state target))}
                               :msg (msg "place " (quantify n "advancement token")
                                         " on " (card-str state target))
                               :async true
                               :effect (effect (add-prop :corp eid target :advance-counter n {:placed true}))}
                              card nil)))}]})

(defcard "Hostile Takeover"
  {:on-score {:msg "gain 7 [Credits] and take 1 bad publicity"
              :async true
              :effect (req (wait-for (gain-credits state side 7 {:suppress-checkpoint true})
                                     (gain-bad-publicity state :corp eid 1)))
              :interactive (req true)}})

(defcard "House of Knives"
  {:on-score (agenda-counters 3)
   :abilities [{:cost [(->c :agenda 1)]
                :msg "do 1 net damage"
                :req (req (:run @state))
                :once :per-run
                :async true
                :effect (effect (damage eid :net 1 {:card card}))}]})

(defcard "Hybrid Release"
  {:on-score {:prompt "Choose a facedown card in Archives to install"
              :show-discard true
              :waiting-prompt true
              :req (req (some #(not (faceup? %)) (:discard corp)))
              :async true
              :choices {:card #(and (corp-installable-type? %)
                                    (in-discard? %)
                                    (not (faceup? %)))}
              :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                       :display-origin true}}))
              :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                     (effect-completed eid))}})

(defcard "Hyperloop Extension"
  {:on-score (gain-credits-ability 3)
   :stolen   (gain-credits-ability 3)})

(defcard "Ikawah Project"
  {:steal-cost-bonus (req [(->c :click 1) (->c :credit 2)])})

(defcard "Illicit Sales"
  {:on-score
   {:async true
    :effect (req (wait-for (resolve-ability
                             state side
                             {:optional
                              {:prompt "Take 1 bad publicity?"
                               :yes-ability {:msg "take 1 bad publicity"
                                             :async true
                                             :effect (effect (gain-bad-publicity :corp eid 1))}}}
                             card nil)
                           (let [n (* 3 (count-bad-pub state))]
                             (system-msg state side (str "uses " (:title card) " to gain " n " [Credits]"))
                             (gain-credits state side eid n))))}})

(defcard "Improved Protein Source"
  (let [ability {:async true
                 :interactive (req true)
                 :msg "make the Runner gain 4 [Credits]"
                 :effect (effect (gain-credits :runner eid 4))}]
    {:on-score ability
     :stolen ability}))

(defcard "Improved Tracers"
  {:move-zone (req (if (and (in-scored? card)
                              (= :corp (:scored-side card)))
                     (do (system-msg state side (str "uses " (:title card) " to increase the strength of Tracer ice by 1"))
                         (system-msg state side (str "uses " (:title card) " to increase the base strength of all trace subroutines by 1"))
                         (update-all-ice state side)
                         (effect-completed state side eid))
                     (effect-completed state side eid)))
   :static-abilities [{:type :ice-strength
                       :req (req (has-subtype? target "Tracer"))
                       :value 1}
                      {:type :trace-base-strength
                       :req (req (= :subroutine (:source-type (second targets))))
                       :value 1}]})

(defcard "Jumon"
  {:events
   [{:event :corp-turn-ends
     :req (req (some #(and (= (last (get-zone %)) :content)
                           (is-remote? (second (get-zone %))))
                     (all-installed state :corp)))
     :prompt "Choose a card to place 2 advancement tokens on"
     :choices {:card #(and (= (last (get-zone %)) :content)
                           (is-remote? (second (get-zone %))))}
     :msg (msg "place 2 advancement token on " (card-str state target))
     :async true
     :effect (effect (add-prop :corp eid target :advance-counter 2 {:placed true}))}]})

(defcard "Kimberlite Field"
  {:on-score
   {:interactive (req true)
    :async true
    :waiting-prompt true
    :prompt "Choose a rezzed card to trash"
    :msg (msg "trash " (card-str state target))
    :req (req (some rezzed? (all-installed state :corp)))
    :choices {:card #(rezzed? %)}
    :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                           (effect-completed eid))
    :effect (req (let [target-cost (:cost target)]
                   (wait-for (trash state side target {:cause-card card})
                             (continue-ability
                               state side
                               {:prompt (str "Choose a runner card that costs " target-cost " or less to trash")
                                :choices {:card #(and (installed? %)
                                                      (runner? %)
                                                      (<= (:cost %) target-cost))}
                                :msg (msg "trash " (:title target))
                                :async true
                                :effect (effect (trash eid target))}
                               card nil))))}})

(defcard "Kingmaking"
  (let [add-abi
        {:prompt "Choose 1 agenda worth 1 or less points"
         :req (req (seq (:hand corp)))
         :async true
         ;; we want a prompt even if there are no valid targets,
         ;; to make sure we don't give away hidden info
         :choices {:card #(and (agenda? %)
                               (in-hand? %)
                               (>= 1 (:agendapoints %)))}
         :waiting-prompt true
         :msg (msg "add " (:title target) " from HQ to [their] score area")
         :effect (req
                   (let [c (move state :corp target :scored)]
                        (card-init state :corp c {:resolve-effect false
                                                  :init-data true}))
                      (update-all-advancement-requirements state)
                      (update-all-agenda-points state)
                      (check-win-by-agenda state side)
                      (effect-completed state side eid))
         :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                (effect-completed eid))}]
    {:on-score {:async true
                :effect (req (wait-for
                               (draw-up-to state side card 3)
                               (continue-ability state side add-abi card nil)))}}))

(defcard "Labyrinthine Servers"
  {:on-score (agenda-counters 2 :power)
   :prevention [{:prevents :jack-out
                 :type :ability
                 :ability {:cost [(->c :power 1)]
                           :msg "prevent the runner from jacking out for the remainder of this run"
                           :condition :active
                           :async true
                           :req (req (preventable? context))
                           :effect (req (wait-for (prevent-jack-out state side)
                                                  (register-lingering-effect
                                                    state side card
                                                    {:type :cannot-jack-out
                                                     :value true
                                                     :duration :end-of-run})
                                                  (effect-completed state side eid)))}}]})

(defcard "License Acquisition"
  {:on-score {:interactive (req true)
              :prompt "Choose an asset or upgrade to install from Archives or HQ"
              :show-discard true
              :choices {:card #(and (corp? %)
                                    (or (asset? %) (upgrade? %))
                                    (or (in-hand? %) (in-discard? %)))}
              :msg (msg "install and rez " (:title target) ", ignoring all costs")
              :async true
              :effect (effect (corp-install eid target nil {:install-state :rezzed-no-cost
                                                            :msg-keys {:install-source card
                                                                       :display-origin true}}))}})

(defcard "Lightning Laboratory"
  (letfn [(ice-derez [zone]
            {:event :runner-turn-ends
             :req (req (seq (filter #(= (:zone %) [:servers zone :ices])
                                    (all-active-installed state :corp))))
             :duration :end-of-turn
             :async true
             :effect (req (let [derez-count (min 2 (count (filter #(= (:zone %) [:servers zone :ices]) (all-active-installed state :corp))))]
                            (continue-ability
                              state side
                              {:prompt (msg "Choose " (quantify derez-count "piece") " of ice protecting " (zone->name [zone]) " to derez")
                               :waiting-prompt true
                               :choices {:card #(and (ice? %)
                                                     (rezzed? %)
                                                     (= (second (get-zone %)) zone))
                                         :max derez-count
                                         :min derez-count}
                               :msg (msg "derez " (enumerate-str (map #(card-str state %) targets)))
                               :async true
                               :effect (req (derez state side eid targets))}
                              card nil)))})]
    {:on-score (agenda-counters 1)
     :events [{:event :run
               :async true
               :optional
               {:prompt (msg "Remove 1 hosted agenda counter to rez up to 2 pieces of ice protecting " (zone->name (:server context)) ", ignoring all costs?")
                :req (req (pos? (get-counters card :agenda)))
                :yes-ability
                {:cost [(->c :agenda 1)]
                 :async true
                 :effect (req (let [current-server (first (:server (:run @state)))]
                                (continue-ability
                                  state side
                                  {:prompt (msg "Choose up to 2 pieces of ice protecting " (zone->name current-server))
                                   :waiting-prompt true
                                   :choices {:card #(and (ice? %)
                                                         (not (rezzed? %))
                                                         (= (second (get-zone %)) current-server))
                                             :max 2}
                                   :async true
                                   :cancel-effect (req (register-events state side card [(ice-derez current-server)])
                                                       (effect-completed state side eid))
                                   :effect (req (register-events state side card [(ice-derez current-server)])
                                                (rez-multiple-cards state side eid targets {:ignore-cost :all-costs :msg-keys {:include-cost-from-eid eid}}))}
                                  card nil)))}}}]}))

(defcard "Longevity Serum"
  {:on-score
   {:prompt "Choose any number of cards in HQ to trash"
    :choices {:max (req (count (:hand corp)))
              :card #(and (corp? %)
                          (in-hand? %))}
    :msg (msg "trash " (quantify (count targets) "card") " from HQ")
    :async true
    :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to trash any cards from HQ"))
                           (shuffle-into-rd-effect eid card 3))
    :effect (req (wait-for (trash-cards state side targets {:unpreventable true :cause-card card})
                           (shuffle-into-rd-effect state side eid card 3)))}})

(defcard "Luminal Transubstantiation"
  {:on-score
   {:silent (req true)
    :effect (req (gain-clicks state :corp 3)
                 (register-turn-flag!
                   state side card :can-score
                   (fn [state side card]
                     ((constantly false)
                      (toast state :corp "Cannot score cards this turn due to Luminal Transubstantiation." "warning")))))}})

(defcard "Mandatory Seed Replacement"
  (letfn [(msr [] {:prompt "Choose two pieces of ice to swap positions"
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
                                  (do (system-msg state :corp "has finished rearranging ice")
                                      (effect-completed state side eid))))})]
    {:on-score {:async true
                :msg "rearrange any number of ice"
                :effect (effect (continue-ability (msr) card nil))}}))

(defcard "Mandatory Upgrades"
  {:move-zone (req (if (and (in-scored? card)
                              (= :corp (:scored-side card)))
                     (do (system-msg state side (str "uses " (:title card) " to gain 1 addition [Click] per turn"))
                         (when (= :corp (:active-player @state))
                           (gain-clicks state :corp 1))
                         (gain state :corp :click-per-turn 1)
                         (effect-completed state side eid))
                     (effect-completed state side eid)))
   :leave-play (req (lose state :corp
                          :click 1
                          :click-per-turn 1))})

(defcard "Market Research"
  {:on-score {:interactive (req true)
              :req (req tagged)
              :async true
              :effect (effect (add-agenda-point-counters eid card 1))}
   :agendapoints-corp (req (if (zero? (get-counters card :agenda)) 2 3))})

(defcard "Medical Breakthrough"
  {:flags {:has-events-when-stolen true}
   :static-abilities [{:type :advancement-requirement
                       :req (req (= (:title target) "Medical Breakthrough"))
                       :value -1}]})

(defcard "Megaprix Qualifier"
  {:on-score {:silent (req true)
              :req (req (< 1 (count (filter #(= (:title %) "Megaprix Qualifier")
                                            (concat (:scored corp) (:scored runner))))))
              :async true
              :effect (effect (add-agenda-point-counters eid card 1))}
   :agendapoints-corp (req (if (zero? (get-counters card :agenda)) 1 2))})

(defcard "Merger"
  {:agendapoints-runner (req 3)})

(defcard "Meteor Mining"
  {:on-score {:interactive (req true)
              :async true
              :prompt "Choose one"
              :waiting-prompt true
              :choices (req ["Gain 7 [Credits]"
                             (when (>= (count-tags state) 2) "Do 7 meat damage")
                             "No action"])
              :effect (req (case target
                             "Gain 7 [Credits]"
                             (do (system-msg state side (str "uses " (:title card) " to gain 7 [Credits]"))
                                 (gain-credits state side eid 7))
                             "Do 7 meat damage"
                             (do (system-msg state side (str "uses " (:title card) " to do 7 meat damage"))
                                 (damage state side eid :meat 7 {:card card}))
                             "No action"
                             (do (system-msg state side (str "declines to use " (:title card)))
                                 (effect-completed state side eid))))}})

(defcard "Midnight-3 Arcology"
  {:on-score {:async true
              :msg (msg "draw 3 cards and skip [their] discard step this turn")
              :effect (effect
                        (register-lingering-effect
                          card
                          {:type :skip-discard
                           :duration :end-of-turn
                           :value true})
                        (draw :corp eid 3))}})

(defcard "NAPD Contract"
  {:steal-cost-bonus (req [(->c :credit 4)])
   :advancement-requirement (req (count-bad-pub state))})

(defcard "Net Quarantine"
  (let [nq {:async true
            :effect (req (let [extra (int (/ (:runner-spent target) 2))]
                           (if (pos? extra)
                             (do (system-msg state :corp (str "uses " (:title card) " to gain " extra " [Credits]"))
                                 (gain-credits state side eid extra))
                             (effect-completed state side eid))))}]
    {:static-abilities [{:type :trace-force-link
                         :req (req (= 1 (count (turn-events state side :initialize-trace))))
                         :value 0}]
     :events [(assoc nq :event :successful-trace)
              (assoc nq :event :unsuccessful-trace)]}))

(defcard "New Construction"
  {:install-state :face-up
   :events [{:event :advance
             :condition :faceup
             :optional
             {:req (req (same-card? card (:card context)))
              :prompt "Install a card from HQ in a new remote?"
              :yes-ability {:prompt "Choose a card to install"
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
                                              {:install-state (when (<= 5 (get-counters (get-card state card) :advancement)) :rezzed-no-cost)
                                               :msg-keys {:install-source card
                                                          :display-origin true}}))}}}]})

(defcard "Next Big Thing"
  {:on-score (agenda-counters 1)
   :stolen (agenda-counters 1)
   :flags {:has-abilities-when-stolen true}
   :abilities [{:action true
                :cost [(->c :click 1) (->c :agenda)]
                :label "Draw 4 cards"
                :msg "draw 4 cards"
                :async true
                :effect (req
                          (play-sfx state side "click-card-2")
                          (wait-for
                               (draw state side 4)
                               (continue-ability
                                 state side
                                 {:prompt "Shuffle any number of cards into R&D"
                                  :waiting-prompt true
                                  :choices {:max (req (count (:hand corp)))
                                            :card (every-pred corp? in-hand?)}
                                  :msg (msg "shuffle " (quantify (count targets) "card")
                                            " from HQ into R&D")
                                  :effect (req (doseq [t targets]
                                                 (move state side t :deck))
                                               (shuffle! state side :deck))}
                                 card nil)))}]})

(defcard "NEXT Wave 2"
  {:on-score
   {:async true
    :effect
    (effect
      (continue-ability
        (when (some #(and (rezzed? %)
                          (ice? %)
                          (has-subtype? % "NEXT"))
                    (all-installed state :corp))
          {:optional
           {:prompt "Do 1 core damage?"
            :yes-ability {:msg "do 1 core damage"
                          :async true
                          :effect (effect (damage eid :brain 1 {:card card}))}}})
        card nil))}})

(defcard "Nisei MK II"
  {:on-score (agenda-counters 1)
   :abilities [{:req (req (:run @state))
                :cost [(->c :agenda 1)]
                :msg "end the run"
                :async true
                :effect (effect (end-run eid card))}]})

(defcard "Oaktown Renovation"
  {:install-state :face-up
   :events [{:event :advance
             :condition :faceup
             :req (req (same-card? card (:card context)))
             :msg (msg "gain " (if (>= (get-counters (get-card state card) :advancement) 5) "3" "2") " [Credits]")
             :async true
             :effect (effect (gain-credits eid (if (<= 5 (get-counters (get-card state card) :advancement)) 3 2)))}]})

(defcard "Obokata Protocol"
  {:steal-cost-bonus (req [(->c :net 4)])})

(defcard "Offworld Office"
  {:on-score (gain-credits-ability 7)})

(defcard "Off the Books"
  (project-agenda {:mode :computed})
  {:events [{:event :corp-turn-ends
             :interactive (req true)
             :skippable true
             :optional {:req (req (and (pos? (get-counters card :agenda))
                                       (seq (:deck corp))))
                        :prompt "Search R&D for a card?"
                        :yes-ability {:cost [(->c :agenda 1)]
                                      :choices (req (cancellable (:deck corp) :sorted))
                                      :prompt "Tutor a card"
                                      :async true
                                      :msg (msg "reveal " (:title target) " from R&D")
                                      :effect (req (shuffle! state side :deck)
                                                   (wait-for
                                                     (reveal state side target)
                                                     (let [target-card target]
                                                       (continue-ability
                                                         state side
                                                         (choose-one-helper
                                                           [{:option (str "Install " (:title target-card))
                                                             :ability {:async true
                                                                       :effect (req (corp-install state side eid target-card nil {:ignore-install-cost true :msg-args {:display-origin true :install-source card}}))}}
                                                            {:option (str "Add " (:title target-card) " to HQ")
                                                             :ability {:msg (msg "add " (:title target-card) " to HQ")
                                                                       :effect (req (move state side target-card :hand))}}])
                                                         card nil))))}}}]})

(defcard "Ontological Dependence"
  {:advancement-requirement (req (- (or (get-in @state [:runner :brain-damage]) 0)))})

(defcard "Oracle Thinktank"
  {:stolen (give-tags 1)
   :abilities [{:action true
                :cost [(->c :click 1) (->c :tag 1)]
                :req (req (is-scored? state :runner card))
                :msg "shuffle itself into R&D"
                :label "Shuffle this agenda into R&D"
                :effect (effect (move :corp card :deck nil)
                                (shuffle! :corp :deck)
                                (update-all-agenda-points))}]
   :flags {:has-abilities-when-stolen true}})

(defcard "Orbital Superiority"
  {:on-score
   {:msg (msg (if (is-tagged? state) "do 4 meat damage" "give the Runner 1 tag"))
    :async true
    :effect (req (if (is-tagged? state)
                   (damage state :corp eid :meat 4 {:card card})
                   (gain-tags state :corp eid 1)))}})

(defcard "Paper Trail"
  {:on-score
   {:trace
    {:base 6
     :successful
     {:msg "trash all connection and job resources"
      :async true
      :effect (req (let [resources (filter #(or (has-subtype? % "Job")
                                                (has-subtype? % "Connection"))
                                           (all-active-installed state :runner))]
                     (trash-cards state side eid resources {:cause-card card})))}}}})

(defcard "Personality Profiles"
  (let [pp {:req (req (pos? (count (:hand runner))))
            :async true
            :effect (req (let [c (first (shuffle (:hand runner)))]
                           (system-msg state side (str "uses " (:title card) " to force the Runner"
                                                       " to trash " (:title c)
                                                       " from the grip at random"))
                           (trash state side eid c {:cause-card card})))}]
    {:events [(assoc pp :event :searched-stack)
              (assoc pp
                     :event :runner-install
                     :req (req (and (some #{:discard} (:previous-zone (:card context)))
                                    (pos? (count (:hand runner))))))]}))

(defcard "Philotic Entanglement"
  {:on-score {:interactive (req true)
              :req (req (pos? (count (:scored runner))))
              :msg (msg "do " (count (:scored runner)) " net damage")
              :async true
              :effect (effect (damage eid :net (count (:scored runner)) {:card card}))}})

(defcard "Post-Truth Dividend"
  {:on-score {:optional
              {:prompt "Draw 1 card?"
               :yes-ability
               {:msg "draw 1 card"
                :async true
                :effect (effect (draw eid 1))}
               :no-ability
               {:effect (effect (system-msg (str "declines to use " (:title card))))}}}})

(defcard "Posted Bounty"
  {:on-score {:optional
              {:prompt "Forfeit this agenda to give the Runner 1 tag and take 1 bad publicity?"
               :yes-ability
               {:msg "give the Runner 1 tag and take 1 bad publicity"
                :cost [(->c :forfeit-self)]
                :async true
                :effect (req (wait-for
                               (gain-bad-publicity state :corp (make-eid state eid) 1 {:suppress-checkpoint true})
                               (gain-tags state :corp eid 1)))}}}})

(defcard "Priority Requisition"
  {:on-score {:interactive (req true)
              :choices {:card (every-pred ice? installed? (complement rezzed?))}
              :async true
              :effect (effect (rez eid target {:ignore-cost :all-costs}))}})

(defcard "Private Security Force"
  {:abilities [{:action true
                :req (req tagged)
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :async true
                :effect (effect (damage eid :meat 1 {:card card}))
                :msg "do 1 meat damage"}]})

(defcard "Profiteering"
  {:on-score {:interactive (req true)
              :choices ["0" "1" "2" "3"]
              :prompt "How many bad publicity do you want to take?"
              :msg (msg "take " target " bad publicity and gain " (* 5 (str->int target)) " [Credits]")
              :async true
              :effect (req (let [bp (count-bad-pub state)]
                             (wait-for (gain-bad-publicity state :corp (str->int target))
                                       (if (< bp (count-bad-pub state))
                                         (gain-credits state :corp eid (* 5 (str->int target)))
                                         (effect-completed state side eid)))))}})

(defcard "Project Ares"
  (letfn [(trash-count-str [card]
            (quantify (- (get-counters card :advancement) 4) "installed card"))]
    {:on-score {:player :runner
                :silent (req true)
                :req (req (and (< 4 (get-counters (:card context) :advancement))
                               (pos? (count (all-installed state :runner)))))
                :waiting-prompt true
                :prompt (msg "Choose " (quantify (trash-count-str (:card context)) "installed card") " to trash")
                :choices {:max (req (min (- (get-counters (:card context) :advancement) 4)
                                         (count (all-installed state :runner))))
                          :card #(and (runner? %)
                                      (installed? %))}
                :msg (msg "force the Runner to trash " (trash-count-str (:card context)) " and take 1 bad publicity")
                :async true
                :effect (req (wait-for (trash-cards state side targets {:cause-card card :cause :forced-to-trash})
                                       (system-msg state side (str "trashes " (enumerate-cards targets)))
                                       (gain-bad-publicity state :corp eid 1)))}}))

(defcard "Project Atlas"
  project-agenda
  {:abilities [{:cost [(->c :agenda 1)]
                :keep-menu-open false ; not using :while-agenda-tokens-left as the typical use case is only one token, even if there are multiple
                :prompt "Choose a card"
                :label "Search R&D and add 1 card to HQ"
                ;; we need the req or the prompt will still show
                :req (req (pos? (get-counters card :agenda)))
                :msg (msg "add " (:title target) " to HQ from R&D")
                :choices (req (cancellable (:deck corp) :sorted))
                :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                       (effect-completed eid))
                :effect (effect (shuffle! :deck)
                                (move target :hand))}]})

(defcard "Project Beale"
  (project-agenda {:granularity 2})
  {:agendapoints-runner (req 2)
   :agendapoints-corp (req (+ 2 (get-counters card :agenda)))})

(defcard "Project Ingatan"
  (project-agenda
    {:mode :computed}
    {:events [{:event :corp-turn-ends
               :cost [(->c :agenda 1)]
               :req (req (can-pay? state side eid card nil [(->c :agenda 1)]))
               :interactive (req true)
               :label "Install a card from Archives"
               :prompt "Install a card from Archives, ignoring all costs"
               :show-discard true
               :change-in-game-state {:silent true
                                      :req (req (some #(or (not (:seen %))
                                                           (not (operation? %)))
                                                      (:discard corp)))}
               :choices {:req (req (and (not (operation? target))
                                        (in-discard? target)))}
               :async true
               :effect (req (corp-install state side eid target nil {:ignore-all-cost true
                                                                     :msg-keys {:install-source card
                                                                                :display-origin true}}))}]}))

(defcard "Project Kusanagi"
  project-agenda
  {:abilities [{:label "Give a piece of ice \"[Subroutine] Do 1 net damage\""
                :prompt "Choose a piece of ice"
                :choices {:card #(and (ice? %)
                                      (rezzed? %))}
                :cost [(->c :agenda 1)]
                :change-in-game-state {:req (req (and run (some (every-pred ice? rezzed?)
                                                                (all-installed state :corp))))}
                :keep-menu-open :while-agenda-tokens-left
                :msg (str "make a piece of ice gain \"[Subroutine] Do 1 net damage\" "
                          "after all its other subroutines for the remainder of the run")
                :effect  (effect (register-lingering-effect
                                   card
                                   (let [t target]
                                     {:type :additional-subroutines
                                      :duration :end-of-run
                                      :req (req (same-card? target t))
                                      :value {:subroutines [(do-net-damage 1)]}})))}]})

(defcard "Project Vacheron"
  {:flags {:has-events-when-stolen true}
   :agendapoints-runner (req (if (or (= (first (:previous-zone card)) :discard)
                                     (zero? (get-counters card :agenda))) 3 0))
   :move-zone (req (if (and (in-scored? card)
                              (= :runner (:scored-side card))
                              (not= (first (:previous-zone card)) :discard))
                     (do (system-msg state side (str "uses " (:title card) " to place 4 agenda counters on itself"))
                         (add-counter state side eid (get-card state card) :agenda 4 nil))
                     (effect-completed state side eid)))
   :events [{:event :runner-turn-begins
             :req (req (pos? (get-counters card :agenda)))
             :msg (msg "remove 1 agenda token from " (:title card))
             :async true
             :effect (req (if (pos? (get-counters card :agenda))
                            (wait-for
                              (add-counter state side card :agenda -1 nil)
                              (update-all-agenda-points state side)
                              (let [card (get-card state card)]
                                (when (zero? (get-counters card :agenda))
                                  (let [points (get-agenda-points card)]
                                    (system-msg state :runner
                                                (str "gains " (quantify points "agenda point")
                                                     " from " (:title card))))))
                              (check-win-by-agenda state side)
                              (effect-completed state side eid))
                            (effect-completed state side eid)))}]})

(defcard "Project Vitruvius"
  project-agenda
  {:abilities [(into
                 (corp-recur)
                 {:cost [(->c :agenda 1)]
                  :keep-menu-open false ; not using :while-agenda-tokens-left as the typical use case is only one token, even if there are multiple
                  :req (req (pos? (get-counters card :agenda)))})]})

(defcard "Project Wotan"
  {:on-score (agenda-counters 3)
   :abilities [{:req (req (and current-ice
                               (rezzed? current-ice)
                               (has-subtype? current-ice "Bioroid")
                               (= :approach-ice (:phase run))))
                :cost [(->c :agenda 1)]
                :keep-menu-open :while-agenda-tokens-left
                :msg (str "make the approached piece of Bioroid ice gain \"[Subroutine] End the run\""
                          "after all its other subroutines for the remainder of this run")
                :effect (effect (register-lingering-effect
                                  card
                                  (let [card-target current-ice]
                                    {:type :additional-subroutines
                                     :duration :end-of-run
                                     :req (req (same-card? target card-target))
                                     :value {:subroutines [{:label "End the run"
                                                            :msg "end the run"
                                                            :async true
                                                            :effect (effect (end-run eid card))}]}})))}]})

(defcard "Project Yagi-Uda"
  (letfn [(put-back-counter [state side card]
            (update! state side (assoc-in card [:counter :agenda] (+ 1 (get-counters card :agenda)))))
          (choose-swap [to-swap]
            {:async true
             :prompt (str "Choose a card in HQ to swap with " (:title to-swap))
             :choices {:not-self true
                       :card #(and (corp? %)
                                   (in-hand? %)
                                   (if (ice? to-swap)
                                     (ice? %)
                                     (or (agenda? %)
                                         (asset? %)
                                         (upgrade? %))))}
             :msg (msg "swap " (card-str state to-swap) " with a card from HQ")
             :effect (req (wait-for (swap-cards-async state side to-swap target)
                                    (continue-ability state :runner (offer-jack-out) card nil)))
             :cancel-effect (effect (put-back-counter card)
                                    (effect-completed eid))})
          (choose-card [run-server]
            {:async true
             :prompt "Choose a card in or protecting the attacked server"
             :choices {:card #(= (first run-server) (second (get-zone %)))}
             :effect (effect (continue-ability (choose-swap target) card nil))
             :cancel-effect (effect (put-back-counter card)
                                    (effect-completed eid))})]
    (project-agenda
      {:abilities [{:async true
                    :waiting-prompt true
                    :cost [(->c :agenda 1)]
                    :keep-menu-open false ; not using :while-agenda-tokens-left as the typical use case is only one token, even if there are multiple
                    :label "swap card in HQ with installed card"
                    :req (req run)
                    :effect (effect (continue-ability (choose-card (:server run)) card nil))}]})))

(defcard "Puppet Master"
  {:events [{:event :successful-run
             :skippable true
             :interactive (req true)
             :waiting-prompt true
             :prompt "Choose a card that can be advanced to place 1 advancement token on"
             :choices {:req (req (can-be-advanced? state card))}
             :msg (msg "place 1 advancement token on " (card-str state target))
             :async true
             :effect (effect (add-prop :corp eid target :advance-counter 1 {:placed true}))}]})

(defcard "Proprionegation"
  {:on-score {:silent (req true)
              :async true
              :effect (effect (add-counter eid card :agenda 1))}
   :abilities [{:req (req (:run @state))
                :cost [(->c :agenda 1)]
                ;; you can't get redirected from a ganked encounter (ie during success)
                :change-in-game-state {:req (req (not= :success (:phase (:run @state))))}
                :label "Redirect runner to archives"
                :msg "make the Runner continue the run on Archives"
                :async true
                :effect (req
                          (cond
                            ;; note that the underlying run is adjusted,
                            ;; but the encounter does not end -> relevant for konjin, sisyphus, etc
                            (:forced-encounter @state)
                            (do (redirect-run state side "Archives" :approach-ice)
                                (effect-completed state side eid))
                            (->> @state :run :phase (= :encounter-ice))
                            ;; need to clear the encounter before redirecting, and fire the events
                            (do
                              (when (get-current-encounter state)
                                (queue-event state :end-of-encounter {:ice (get-current-ice state)}))
                              (wait-for
                                (checkpoint state side {:duration :end-of-encounter})
                                (clear-encounter state)
                                (redirect-run state side "Archives" :approach-ice)
                                (start-next-phase state side eid)))
                            :else
                            (do (clear-encounter state)
                                (redirect-run state side "Archives" :approach-ice)
                                (start-next-phase state side eid))))}]})

(defcard "Quantum Predictive Model"
  {:flags {:rd-reveal (req true)}
   :on-access {:req (req tagged)
               :player :runner
               :interactive (req true)
               :prompt "Quantum Predictive Model will be added to the Corp's score area"
               :choices ["OK"]
               :msg (msg "add itself to [their] score area and gain 1 agenda point")
               :effect (effect (move :corp card :scored {:force true})
                               (update-all-agenda-points)
                               (check-win-by-agenda))}})

(defcard "Rebranding Team"
  {:move-zone (req (when (and (in-scored? card)
                              (= :corp (:scored-side card)))
                     (system-msg state side (str "uses " (:title card) " to make all assets gain Advertisement")))
                   (effect-completed state side eid))
   :static-abilities [{:type :gain-subtype
                       :req (req (asset? target))
                       :value "Advertisement"}]})

(defcard "Reeducation"
  (letfn [(corp-final [chosen original]
            {:prompt (str "The bottom cards of R&D will be " (enumerate-cards chosen))
             :choices ["Done" "Start over"]
             :async true
             :msg (req (let [n (count chosen)]
                         (str "add " (quantify n "card") " from HQ to the bottom of R&D and draw " (quantify n "card")
                              (when (<= n (count (:hand runner)))
                                (str
                                  ". The Runner randomly adds " (quantify n "card")
                                  " from [runner-pronoun] Grip to the bottom of the Stack")))))
             :effect (req (let [n (count chosen)]
                            (if (= target "Done")
                              (do (doseq [c (reverse chosen)] (move state :corp c :deck))
                                  (wait-for (draw state :corp n)
                                            ; if corp chooses more cards than runner's hand, don't shuffle runner hand back into Stack
                                            (if (<= n (count (:hand runner)))
                                              (do (doseq [r (take n (shuffle (:hand runner)))] (move state :runner r :deck))
                                                  (queue-event state :runner-hand-changed?)
                                                  (checkpoint state side eid))
                                              (effect-completed state side eid))))
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
                                    (effect-completed state side eid))))))})]
    {:on-score {:async true
                :waiting-prompt true
                :effect (req (let [from (get-in @state [:corp :hand])]
                               (if (pos? (count from))
                                 (continue-ability state :corp (corp-choice from '() from) card nil)
                                 (do (system-msg state side "does not add any cards from HQ to bottom of R&D")
                                     (effect-completed state side eid)))))}}))

(defcard "Regenesis"
  {:on-score {:req (req (and (some #(not (faceup? %)) (:discard corp))
                             (no-event? state side :card-moved (fn [[context]]
                                                                 (and (in-discard? (:moved-card context))
                                                                      (corp? (:moved-card context)))))))
              ;; we want a prompt even if there are no valid targets,
              ;; to make sure we don't give away hidden info
              :prompt "Choose a face-down agenda in Archives"
              :choices {:card #(and (agenda? %)
                                    (in-discard? %)
                                    (not (faceup? %)))}
              :show-discard true
              :async true
              :msg (msg "reveal " (:title (first targets)) " and add it to [their] score area")
              :effect (req (wait-for (reveal state side target)
                                     (let [c (move state :corp target :scored)]
                                       (card-init state :corp c {:resolve-effect false
                                                                 :init-data true}))
                                     (update-all-advancement-requirements state)
                                     (update-all-agenda-points state)
                                     (check-win-by-agenda state side)
                                     (effect-completed state side eid)))
              :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                     (effect-completed eid))}})

(defcard "Regulatory Capture"
  {:advancement-requirement (req (- (min 4 (count-bad-pub state))))})

(defcard "Remastered Edition"
  {:on-score (agenda-counters 1)
   :abilities [{:cost [(->c :agenda 1)]
                :msg (msg "place 1 advancement token on " (card-str state target))
                :label "place 1 advancement token"
                :keep-menu-open :while-agenda-tokens-left
                :choices {:card installed?}
                :async true
                :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}]})

(defcard "Remote Data Farm"
  {:move-zone (req (when (and (in-scored? card)
                              (= :corp (:scored-side card)))
                     (system-msg state side (str "uses " (:title card) " to increase [their] maximum hand size by 2")))
                   (effect-completed state side eid))
   :static-abilities [(corp-hand-size+ 2)]})

(defcard "Remote Enforcement"
  {:on-score
   {:interactive (req true)
    :optional
    {:prompt "Search R&D for a piece of ice to install protecting a remote server?"
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
                                    :prompt (str "Choose a server to install " (:title chosen-ice) " on")
                                    :choices (filter #(not (#{"HQ" "Archives" "R&D"} %))
                                                     (installable-servers state chosen-ice))
                                    :effect (effect (shuffle! :deck)
                                                    (corp-install eid chosen-ice target
                                                                  {:install-state :rezzed-no-rez-cost
                                                                   :msg-keys {:install-source card
                                                                              :display-origin true}}))})
                                 card nil))}
                    {:prompt "You have no ice in R&D"
                     :choices ["Carry on!"]
                     :prompt-type :bogus
                     :effect (effect (shuffle! :deck))})
                  card nil))}}}})

(defcard "Research Grant"
  {:on-score {:interactive (req true)
              :silent (req (empty? (filter #(= (:title %) (:title card)) (all-installed state :corp))))
              :async true
              :effect (effect (continue-ability
                                {:prompt (str "Choose another installed copy of " (:title card) " to score")
                                 :choices {:card #(= (:title %) (:title card))}
                                 :interactive (req true)
                                 :async true
                                 :req (req (seq (filter #(= (:title %) (:title card)) (all-installed state :corp))))
                                 :effect (effect (score eid (get-card state target) {:no-req true}))
                                 :msg "score another installed copy of itself"}
                                card nil))}})

(defcard "Restructured Datapool"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :label "give runner 1 tag"
                :keep-menu-open :while-clicks-left
                :trace {:base 2
                        :successful (give-tags 1)}}]})

(defcard "Salvo Testing"
  {:events [{:event :agenda-scored
             :interactive (req true)
             :optional
             {:prompt "Do 1 core damage?"
              :waiting-prompt true
              :yes-ability
              {:msg "do 1 core damage"
               :async true
               :effect (effect (damage eid :brain 1 {:card card}))}}}]})

(defcard "SDS Drone Deployment"
  {:steal-cost-bonus (req [(->c :program 1)])
   :on-score {:req (req (seq (all-installed-runner-type state :program)))
              :waiting-prompt true
              :prompt "Choose a program to trash"
              :choices {:card #(and (installed? %)
                                    (program? %))
                        :all true}
              :msg (msg "trash " (:title target))
              :async true
              :effect (effect (trash eid target {:cause-card card}))}})

(defcard "See How They Run"
  {:on-score {:interactive (req true)
              :msg "give the runner 1 tag"
              :async true
              :effect (req (wait-for
                             (gain-tags state :runner 1)
                             (continue-ability
                               state side
                               {:msg "start a psi game (do 1 core damage / do 1 net damage)"
                                :psi {:not-equal {:msg "do 1 core damage"
                                                  :async true
                                                  :effect (effect (damage eid :brain 1 {:card card}))}
                                      :equal {:async true
                                              :msg "do 1 net damage"
                                              :effect (effect (damage eid :net 1 {:card card}))}}}
                               card nil)))}})

(defcard "Self-Destruct Chips"
  {:move-zone (req (when (and (in-scored? card)
                              (= :corp (:scored-side card)))
                     (system-msg state side (str "uses " (:title card)
                                                 " to decrease the Runner's maximum hand size by 1")))
                   (effect-completed state side eid))
   :static-abilities [(runner-hand-size+ -1)]})

(defcard "Send a Message"
  (let [ability
        {:interactive (req true)
         :choices {:card #(and (ice? %)
                               (not (rezzed? %))
                               (installed? %))}
         :async true
         :effect (effect (rez eid target {:ignore-cost :all-costs}))}]
    {:on-score ability
     :stolen ability}))

(defcard "Sensor Net Activation"
  {:on-score (agenda-counters 1)
   :abilities [{:cost [(->c :agenda 1)]
                :req (req (some #(and (has-subtype? % "Bioroid")
                                      (not (rezzed? %)))
                                (all-installed state :corp)))
                :label "Choose a bioroid to rez, ignoring all costs"
                :prompt "Choose a bioroid to rez, ignoring all costs"
                :choices {:card #(and (has-subtype? % "Bioroid")
                                      (not (rezzed? %)))}
                :async true
                :effect (req (wait-for (rez state side target {:ignore-cost :all-costs
                                                               :msg-keys {:include-cost-from-eid eid}})
                                       (let [c (:card async-result)
                                             ev (if (= (:active-player @state) :corp) :corp-turn-ends :runner-turn-ends)]
                                         (register-events
                                           state side card
                                           [{:event ev
                                             :unregister-once-resolved true
                                             :duration :end-of-turn
                                             :async true
                                             :effect (effect (derez eid c))}])
                                         (effect-completed state side eid))))}]})

(defcard "Sentinel Defense Program"
  {:events [{:event :damage
             :req (req (and (pos? (:amount context))
                            (= (:damage-type context) :brain)))
             :msg "do 1 net damage"
             :async true
             :effect (effect (damage eid :net 1 {:card card}))}]})

(defcard "Sericulture Expansion"
  (project-agenda {:mode :computed})
  {:events [{:event :corp-turn-ends
             :req (req (and (seq (all-installed state :corp))
                            (can-pay? state side eid card nil [(->c :agenda 1)])))
             :prompt "Choose a card to place 2 advancement counters on"
             :player :corp
             :cost [(->c :agenda 1)]
             :choices {:card (every-pred corp? installed?)}
             :msg (msg "place 2 advancement counters on " (card-str state target))
             :async true
             :effect (req (add-prop state :corp eid target :advance-counter 2 {:placed true}))}]})

(defcard "Show of Force"
  {:on-score {:async true
              :msg "do 2 meat damage"
              :effect (effect (damage eid :meat 2 {:card card}))}})

(defcard "Sisyphus Protocol"
  (letfn [(rezzed-gate-or-sentry [context]
            (and (rezzed? (:ice context))
                 (or (has-subtype? (:ice context) "Code Gate")
                     (has-subtype? (:ice context) "Sentry"))))]
    {:events [{:event :pass-ice
               :req (req (and (rezzed-gate-or-sentry context)
                              (first-event? state side :pass-ice
                                            #(rezzed-gate-or-sentry (first %)))))
               :async true
               :effect (req (let [enc-ice (get-card state (:ice context))]
                              (continue-ability
                                state side
                                {:prompt (msg "Make the runner encounter " (:title (:ice context)) " again?")
                                 :choices (req [(when (can-pay? state :corp eid card nil (->c :credit 1))
                                                  "Pay 1 [Credit]")
                                                (when (can-pay? state :corp eid card nil (->c :trash-from-hand 1))
                                                  "Trash 1 card from HQ")
                                                "Done"])
                                 :async true
                                 :effect (req (if (= target "Done")
                                                (effect-completed state side eid)
                                                (continue-ability
                                                  state side
                                                  {:cost (if (= target "Pay 1 [Credit]")
                                                           (->c :credit 1)
                                                           (->c :trash-from-hand 1))
                                                   :change-in-game-state {:req (req (and enc-ice (rezzed? enc-ice)))}
                                                   :msg (msg "make the runner encounter " (card-str state enc-ice) " again")
                                                   :async true
                                                   :effect (req (force-ice-encounter state side eid enc-ice))}
                                                  card nil)))}
                              card targets)))}]}))

(defcard "Slash and Burn Agriculture"
  {:expend {:req (req (some #(can-be-advanced? state %) (all-installed state :corp)))
            :cost [(->c :credit 1)]
            :choices {:req (req (can-be-advanced? state target))}
            :msg (msg "place 2 advancement counters on " (card-str state target))
            :async true
            :effect (req
                      (add-prop state :corp eid target :advance-counter 2 {:placed true}))}})

(defcard "SSL Endorsement"
  {:flags {:has-events-when-stolen true}
   :abilities [(set-autoresolve :auto-fire "SSL Endorsement")]
   :stolen (agenda-counters 9 :credit)
   :on-score (agenda-counters 9 :credit)
   :events [{:event :corp-turn-begins
             :automatic :gain-credits
             :optional
             {:req (req (pos? (get-counters card :credit)))
              :once :per-turn
              :prompt "Gain 3 [Credits]?"
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability
              {:async true
               :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
               :effect (req (if (pos? (get-counters card :credit))
                              (take-credits state side eid card :credit 3)
                              (effect-completed state side eid)))}}}]})

(defcard "Standoff"
  (letfn [(stand [side]
            {:async true
             :prompt "Choose one of your installed cards to trash"
             :choices {:card #(and (installed? %)
                                   (same-side? side (:side %)))}
             :cancel-effect (req (if (= side :runner)
                                   (wait-for (draw state :corp 1)
                                             (clear-wait-prompt state :corp)
                                             (system-msg state :runner (str "declines to trash a card for " (:title card)))
                                             (system-msg state :corp (str "uses " (:title card) " to draw 1 card and gain 5 [Credits]"))
                                             (gain-credits state :corp eid 5))
                                   (do (system-msg state :corp (str "declines to trash a card for " (:title card)))
                                       (clear-wait-prompt state :runner)
                                       (effect-completed state :corp eid))))
             :effect (req (wait-for (trash state side target
                                           (if (= side :corp)
                                             {:unpreventable true :cause-card card}
                                             {:unpreventable true :cause-card card :cause :forced-to-trash}))
                                    (system-msg state side (str "trashes " (card-str state target) " for " (:title card)))
                                    (clear-wait-prompt state (other-side side))
                                    (show-wait-prompt state side (str (side-str (other-side side)) " to trash a card for " (:title card)))
                                    (continue-ability state (other-side side) (stand (other-side side)) card nil)))})]
    {:on-score
     {:interactive (req true)
      :async true
      :effect (effect (show-wait-prompt (str (side-str (other-side side)) " to trash a card for Standoff"))
                      (continue-ability :runner (stand :runner) card nil))}}))

(defcard "Stegodon MK IV"
  {:events [{:event :run
             :async true
             :effect
             (req (let [rezzed-targets
                        (seq (filter #(and (ice? %)
                                           (rezzed? %)
                                           (not= (first (:server target)) (second (get-zone %))))
                                     (all-installed-corp state)))]
                    (continue-ability
                      state side
                      (when rezzed-targets
                        {:prompt "Choose a piece of ice protecting another server to derez"
                         :waiting-prompt true
                         :choices {:req (req (some #{target} rezzed-targets))}
                         :once :per-turn
                         :async true
                         :effect (req (wait-for
                                        (derez state side target {:msg-keys {:and-then " and gain 1 [Credits]"}})
                                        (gain-credits state side eid 1)))})
                      card nil)))}
            {:event :derez
             :req (req (and run
                            (first-run-event?
                              state side :derez
                              (fn [[context]] (some ice? (:cards context))))))
             :msg "lower strength of each installed icebreaker by 2"}]
   :leave-play (effect (update-all-icebreakers))
   :static-abilities [{:type :breaker-strength
                       :value -2
                       :req (req (and run
                                      (has-subtype? target "Icebreaker")
                                      (<= 1 (run-event-count
                                              state side :derez
                                              (fn [[context]] (some ice? (:cards context)))))))}]})

(defcard "Sting!"
  (letfn [(count-opp-stings [state side]
            (count (filter #(= (:title %) "Sting!") (get-in @state [(other-side side) :scored]))))]
    {:on-score {:msg (msg "deal " (inc (count-opp-stings state :corp)) " net damage")
                :async true
                :effect (effect (damage eid :net (inc (count-opp-stings state :corp)) {:card card}))}
     :stolen {:msg (msg "deal " (inc (count-opp-stings state :runner)) " net damage")
              :async true
              :effect (effect (damage eid :net (inc (count-opp-stings state :runner)) {:card card}))}}))

(defcard "Stoke the Embers"
  (letfn [(score-abi
            [cred-gain]
            {:msg (msg "gain " cred-gain " [Credits]")
             :interactive (req true)
             :async true
             :effect (req (wait-for
                            (gain-credits state side (make-eid state eid) cred-gain)
                            (continue-ability
                              state side
                              {:req (req (seq (all-installed-corp state)))
                               :choices {:card #(installed? %)}
                               :waiting-prompt true
                               :msg (msg "place 1 advancement counter on "
                                         (card-str state target))
                               :async true
                               :effect (effect (add-prop :corp eid target :advance-counter 1
                                                         {:placed true}))}
                              card nil)))})]
    {:on-score (score-abi 3)
     :derezzed-events [{:event :corp-install
                        :optional
                        {:prompt "Reveal this agenda to gain 2 [Credits] and place 1 advancement counter on an installed card?"
                         :req (req (and
                                     (not= [:hand] (:previous-zone card))
                                     (same-card? (:card target) card)))
                         :waiting-prompt true
                         :yes-ability
                         {:msg (msg "reveal itself from " (zone->name (:previous-zone card)))
                          :async true
                          :effect (req (wait-for
                                         (reveal state side card)
                                         (continue-ability state side (score-abi 2) (get-card state card) nil)))}}}]}))

(defcard "Successful Field Test"
  (letfn [(sft [n max-ops]
            {:prompt "Choose a card in HQ to install"
             :async true
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (in-hand? %))}
             :effect (req (wait-for
                            (corp-install state side target nil {:ignore-all-cost true
                                                                 :msg-keys {:install-source card
                                                                            :display-origin true}})
                            (continue-ability state side (when (< n max-ops) (sft (inc n) max-ops)) card nil)))})]
    {:on-score {:async true
                :msg "install cards from HQ, ignoring all costs"
                :effect (req (let [max-ops (count (filter (complement operation?) (:hand corp)))]
                               (continue-ability state side (sft 1 max-ops) card nil)))}}))

(defcard "Superconducting Hub"
  {:static-abilities [{:type :hand-size
                       :req (req (= :corp side))
                       :value 2}]
   :on-score
   {:optional
    {:prompt "Draw 2 cards?"
     :yes-ability {:msg "draw 2 cards"
                   :async true
                   :effect (effect (draw :corp eid 2))}}}})

(defcard "Superior Cyberwalls"
  (ice-boost-agenda "Barrier"))

(defcard "TGTBT"
  {:flags {:rd-reveal (req true)}
   :on-access (give-tags 1)})

(defcard "The Cleaners"
  {:prevention [{:prevents :pre-damage
                 :type :event
                 :max-uses 1
                 :mandatory true
                 :ability {:async true
                           :condition :active
                           :req (req
                                  (and (= :meat (:type context))
                                       (not= :all (:prevented context))
                                       (= :corp (:source-player context))
                                       (not (:unboostable context))))
                           :msg "increase the pending meat damage by 1"
                           :effect (req (damage-boost state side eid 1))}}]})

(defcard "The Future is Now"
  {:on-score {:interactive (req true)
              :prompt "Choose a card to add to HQ"
              :choices (req (:deck corp))
              :msg "add a card from R&D to HQ and shuffle R&D"
              :req (req (pos? (count (:deck corp))))
              :effect (effect (shuffle! :deck)
                              (move target :hand))}})

(defcard "The Future Perfect"
  {:flags {:rd-reveal (req true)}
   :on-access {:psi {:req (req (not installed))
                     :not-equal
                     {:msg "prevent itself from being stolen"
                      :async true
                      :effect (effect (register-run-flag!
                                        card :can-steal
                                        (fn [_ _ c] (not (same-card? c card))))
                                      (effect-completed eid))}}}})

(defcard "Timely Public Release"
  {:on-score (agenda-counters 1)
   :abilities [{:cost [(->c :agenda 1)]
                :keep-menu-open false ; not using :while-agenda-tokens-left as the typical use case is only one token, even if there are multiple
                :label "Install a piece of ice in any position, ignoring all costs"
                :prompt "Choose a piece of ice to install"
                :show-discard true
                :choices {:card #(and (ice? %)
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                :async true
                :msg "install an ice from HQ or Archives"
                :effect (effect
                          (continue-ability
                            (let [chosen-ice target]
                              {:prompt "Choose a server"
                               :choices (req (installable-servers state chosen-ice))
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
                                                             (corp-install state side eid chosen-ice chosen-server
                                                                           {:ignore-all-cost true :index target
                                                                            :msg-keys {:install-source card
                                                                                       :display-origin true}})))})
                                           card nil))})
                            card nil))}]})

(defcard "Tomorrow's Headline"
  {:on-score (give-tags 1)
   :stolen (give-tags 1)})

(defcard "Transport Monopoly"
  {:on-score (agenda-counters 2)
   :abilities [{:cost [(->c :agenda 1)]
                :once :per-turn
                :req (req run)
                :msg "prevent this run from becoming successful"
                :effect (effect (register-lingering-effect
                                  card
                                  {:type :block-successful-run
                                   :duration :end-of-run
                                   :value true}))}]})

(defcard "Underway Renovation"
  (letfn [(adv4? [s c] (if (>= (get-counters (get-card s c) :advancement) 4) 2 1))]
    {:install-state :face-up
     :events [{:event :advance
               :condition :faceup
               :async true
               :req (req (same-card? card (:card context)))
               :msg (msg (if (pos? (count (:deck runner)))
                           (str "trash "
                                (enumerate-cards (take (adv4? state card) (:deck runner)))
                                " from the stack")
                           "trash no cards from the stack (it is empty)"))
               :effect (effect (mill :corp eid :runner (adv4? state card)))}]}))

(defcard "Unorthodox Predictions"
  {:implementation "Prevention of subroutine breaking is not enforced"
   :on-score {:prompt "Choose an ice type"
              :choices ["Barrier" "Code Gate" "Sentry"]
              :msg (msg "prevent subroutines on " target " ice from being broken until next turn")}})

(defcard "Utopia Fragment"
  {:static-abilities
   [{:type :steal-additional-cost
     :req (req (pos? (get-counters target :advancement)))
     :value (req (->c :credit (* 2 (get-counters target :advancement))))}]})

(defcard "Vanity Project"
  ;; No special implementation
  {})

(defcard "Veterans Program"
  {:on-score {:interactive (req true)
              :msg "remove 2 bad publicity"
              :effect (effect (lose-bad-publicity 2))}})

(defcard "Viral Weaponization"
  {:on-score
   {:effect
    (effect
      (register-events
        card
        [{:event (if (= :corp (:active-player @state)) :corp-turn-ends :runner-turn-ends)
          :unregister-once-resolved true
          :duration :end-of-turn
          :msg (msg "do " (count (:hand runner)) " net damage")
          :async true
          :effect (effect (damage eid :net (count (:hand runner)) {:card card}))}]))}})

(defcard "Voting Machine Initiative"
  {:on-score (agenda-counters 3)
   :events [{:event :runner-turn-begins
             :optional
             {:req (req (pos? (get-counters card :agenda)))
              :waiting-prompt true
              :prompt "Make the Runner lose [Click]?"
              :yes-ability
              {:msg "make the Runner lose [Click]"
               :cost [(->c :agenda 1)]
               :effect (effect (lose-clicks :runner 1))}}}]})

(defcard "Vulcan Coverup"
  {:on-score {:interactive (req true)
              :msg "do 2 meat damage"
              :async true
              :effect (effect (damage eid :meat 2 {:card card}))}
   :stolen {:msg "force the Corp to take 1 bad publicity"
            :async true
            :effect (effect (gain-bad-publicity :corp eid 1))}})

(defcard "Vulnerability Audit"
  {:flags {:can-score (req (let [result (not= :this-turn (installed? card))]
                             (when-not result
                               (toast state :corp "Cannot score Vulnerability Audit the turn it was installed." "warning"))
                             result))}})

(defcard "Water Monopoly"
  {:static-abilities [{:type :install-cost
                       :req (req (and (resource? target)
                                      (not (has-subtype? target "Virtual"))
                                      (not (:facedown (second targets)))))
                       :value 1}]})
