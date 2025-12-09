(ns game.cards.assets
  (:require
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-bonus access-card get-only-card-to-access installed-access-trigger num-cards-to-access]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-runner-type get-remotes
                            installable-servers]]
   [game.core.card :refer [agenda? asset? can-be-advanced? corp? event? corp-installable-type?
                           faceup? fake-identity? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype?
                           has-any-subtype? ice? identity? in-deck? in-discard? in-hand? in-server? installed? is-type?
                           operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [play-cost]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [corp-install-up-to-n-cards corp-recur corp-rez-toast defcard do-meat-damage do-net-damage draw-abi gain-credits-ability give-tags
                                  reorder-choice spend-credits take-credits take-n-credits-ability trash-on-empty get-x-fn with-revealed-hand]]
   [game.core.drawing :refer [draw first-time-draw-bonus max-draw
                              remaining-draws]]
   [game.core.effects :refer [is-disabled-reg? register-lingering-effect update-disabled-cards]]
   [game.core.eid :refer [complete-with-result effect-completed is-basic-advance-action? make-eid get-ability-targets]]
   [game.core.engine :refer [not-used-once? pay register-events resolve-ability trigger-event-sync should-trigger?]]
   [game.core.events :refer [first-event? no-event? turn-events event-count]]
   [game.core.flags :refer [in-corp-scored? in-runner-scored? lock-zone prevent-current
                            prevent-draw
                            register-turn-flag! release-zone when-scored?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [update-all-ice update-ice-strength]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-msg]]
   [game.core.moving :refer [as-agenda mill move remove-from-currently-drawing
                             swap-agendas swap-cards swap-installed trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-value ->c]]
   [game.core.play-instants :refer [can-play-instant? play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon set-prop]]
   [game.core.prevention :refer [damage-name preventable? prevent-bad-publicity prevent-damage prevent-expose]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [can-pay-to-rez? derez rez]]
   [game.core.runs :refer [end-run]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.servers :refer [is-remote? target-server zone->name]]
   [game.core.set-aside :refer [swap-set-aside-cards]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [check-win-by-agenda win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [game.core.link :refer [get-link]]))

;;; Asset-specific helpers
(defn- advance-ambush
  "Creates advanceable ambush structure with specified ability for specified cost"
  ([cost ability] (assoc (installed-access-trigger cost ability) :advanceable :always))
  ([cost ability prompt] (assoc (installed-access-trigger cost ability prompt) :advanceable :always)))

(defn- take-n-credits-start-of-turn
  ([n] (take-n-credits-start-of-turn n :credit))
  ([n counter-type]
   (let [num-counters (fn [card] (min n (get-counters card counter-type)))]
     {:msg (msg "gain " (num-counters card) " [Credits]")
      :once :per-turn
      :automatic :gain-credits
      :req (req (and (:corp-phase-12 @state)
                     (pos? (get-counters card counter-type))))
      :label (str "Gain " n " [Credits] (start of turn)")
      :async true
      :effect (req (take-credits state side eid card counter-type n))})))

(defn campaign
  "Creates a Campaign with X counters draining Y per-turn.
  Trashes itself when out of counters"
  ([counters per-turn] (campaign counters per-turn :credit))
  ([counters per-turn counter-type]
   (let [ability (take-n-credits-start-of-turn per-turn counter-type)]
     {:data {:counter {counter-type counters}}
      :derezzed-events [corp-rez-toast]
      :events [(trash-on-empty counter-type)
               (assoc ability :event :corp-turn-begins)]
      :abilities [ability]})))

(defn creds-on-round-start
  "For perpetual campaigns, like PAD Campaign and refuge campaign"
  [per-turn]
  (let [ability {:msg (str "gain " per-turn " [Credits]")
                 :label (str "Gain " per-turn " [Credits] (start of turn)")
                 :once :per-turn
                 :async true
                 :automatic :gain-credits
                 :effect (effect (gain-credits eid per-turn))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(def executive-trash-effect
  {:when-inactive true
   :req (req (and (= side :runner)
                  (:accessed target)))
   :msg "add itself to the Runner's score area as an agenda worth 2 agenda points"
   :effect (req (as-agenda state :runner card 2))})

(defn return-to-top
  "returns a set of cards to the top of the deck"
  ([set-aside-cards] (return-to-top set-aside-cards false))
  ([set-aside-cards reveal]
   {:prompt "Choose a card to put on top of R&D"
    :req (req (not (zero? (count set-aside-cards))))
    :choices {:min 1
              :max 1
              :req (req (some #(same-card? % target) set-aside-cards))}
    :async true
    :waiting-prompt true
    :msg (msg "place " (if reveal (:title target) "a card") " on top of R&D")
    :effect (req (move state :corp target :deck {:front true})
                 (let [rem (seq (filter #(not (same-card? target %)) set-aside-cards))]
                   (if (seq rem)
                     (continue-ability state side
                                       (return-to-top rem reveal)
                                       card nil)
                     (effect-completed state side eid))))}))

(def gain-power-counter
  {:async true
   :msg "add 1 power counter to itself"
   :effect (req (add-counter state side eid card :power 1 {:placed true}))})

;; Card definitions

(defcard "Adonis Campaign"
  (campaign 12 3))

(defcard "Advanced Assembly Lines"
  {:on-rez {:async true
            :msg "gain 3 [Credits]"
            :effect (effect (gain-credits eid 3))}
   :abilities [{:label "Install a non-agenda card from HQ"
                :async true
                :prompt "Choose a non-agenda card to install from HQ"
                :change-in-game-state {:req (req (seq (:hand corp)))}
                :req (req (not (:run @state)))
                :choices {:card #(and (corp-installable-type? %)
                                      (not (agenda? %))
                                      (in-hand? %)
                                      (corp? %))}
                :cost [(->c :trash-can)]
                :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                         :display-origin true}}))}]})

(defcard "Aggressive Secretary"
  (advance-ambush 2 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :waiting-prompt true
                     :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "program") " to trash")
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card (every-pred installed? program?)}
                     :msg (msg "trash " (enumerate-cards targets))
                     :async true
                     :effect (effect (trash-cards eid targets {:cause-card card}))}))

(defcard "Alexa Belsky"
  {:abilities [{:label "Shuffle all cards in HQ into R&D"
                :async true
                :cost [(->c :trash-can)]
                :effect (effect
                          (continue-ability
                            {:waiting-prompt true
                             :prompt "How many credits do you want to pay?"
                             :choices :credit
                             :player :runner
                             :msg (msg "shuffle "
                                       (quantify (- (count (:hand corp)) (quot target 2)) "card")
                                       " in HQ into R&D")
                             :effect (req (if (pos? (quot target 2))
                                            (let [prevented (quot target 2)
                                                  unprevented (- (count (:hand corp)) prevented)]
                                              (doseq [c (take unprevented (shuffle (:hand corp)))]
                                                (move state :corp c :deck))
                                              (when (pos? unprevented)
                                                (shuffle! state :corp :deck))
                                              (system-msg state :runner
                                                          (str "pays " target " [Credits] to prevent "
                                                               (quantify prevented "random card")
                                                               " in HQ from being shuffled into R&D")))
                                            (shuffle-into-deck state :corp :hand)))}
                            card nil))}]})

(defcard "Alix T4LB07"
  {:events [{:event :corp-install
             :async true
             :effect (effect (add-counter eid card :power 1 nil))}]
   :abilities [{:action true
                :label "Gain 2 [Credits] for each counter on Alix T4LB07"
                :cost [(->c :click 1) (->c :trash-can)]
                :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (* 2 (get-counters card :power))))}]})

(defcard "Allele Repression"
  (letfn [(select-archives-cards [total]
            {:async true
             :show-discard true
             :prompt (str "Choose " (quantify total "card") " from Archives")
             :choices {:card #(and (corp? %)
                                   (in-discard? %))
                       :max total
                       :all true}
             :effect (effect (complete-with-result eid targets))})
          (select-hq-cards [total]
            {:async true
             :prompt (str "Choose " (quantify total "card") " from HQ")
             :choices {:card #(and (corp? %)
                                   (in-hand? %))
                       :max total
                       :all true}
             :effect (effect (complete-with-result eid targets))})]
    {:advanceable :always
     :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                  :cost [(->c :trash-can)]
                  :msg (msg "swap "
                         (quantify (max (count (:discard corp))
                                        (count (:hand corp))
                                        (get-counters card :advancement))
                                   "card")
                         " in HQ and Archives")
                  :async true
                  :waiting-prompt true
                  :effect (req (let [total (min (count (:discard corp))
                                                (count (:hand corp))
                                                (get-counters card :advancement))]
                                 (wait-for (resolve-ability state side (select-hq-cards total) card nil)
                                           (let [hq-cards async-result]
                                             (wait-for (resolve-ability state side (select-archives-cards total) card nil)
                                                       (let [archives-cards async-result]
                                                         (doseq [[hq-card archives-card] (map vector hq-cards archives-cards)]
                                                           (swap-cards state side hq-card archives-card)))
                                                       (effect-completed state side eid))))))}]}))

(defcard "Amani Senai"
  (letfn [(senai-ability [agenda]
            {:interactive (req true)
             :optional {:prompt "Initiate a trace?"
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability {:trace {:base (get-advancement-requirement agenda)
                                              :successful
                                              {:choices {:card #(and (installed? %)
                                                                     (runner? %))}
                                               :label "add 1 installed card to the grip"
                                               :msg (msg "add " (:title target) " to the grip")
                                               :effect (effect (move :runner target :hand))}}}}})]
    {:events [{:event :agenda-scored
               :interactive (req true)
               :async true
               :effect (effect (continue-ability (senai-ability (:card context)) card nil))}
              {:event :agenda-stolen
               :interactive (req true)
               :async true
               :effect (effect (continue-ability (senai-ability (:card context)) card nil))}]
     :abilities [(set-autoresolve :auto-fire "Amani Senai")]}))

(defcard "Anson Rose"
  (let [ability {:label "Place 1 advancement token (start of turn)"
                 :once :per-turn
                 :msg "place 1 advancement token on itself"
                 :async true
                 :effect (effect (add-prop eid card :advance-counter 1 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)
              {:event :rez
               :req (req (and (ice? (:card context))
                              (pos? (get-counters card :advancement))))
               :async true
               :effect (req (let [ice (get-card state (:card context))
                                  icename (:title ice)]
                              (continue-ability
                                state side
                                {:optional
                                 {:waiting-prompt true
                                  :prompt (str "Move advancement tokens to " icename "?")
                                  :yes-ability
                                  {:prompt "How many advancement tokens do you want to move?"
                                   :choices {:number (req (get-counters card :advancement))}
                                   :async true
                                   :effect (req (wait-for
                                                  (add-prop state :corp ice :advance-counter target {:placed true})
                                                  (wait-for
                                                    (add-prop state :corp card :advance-counter (- target) {:placed true})
                                                    (system-msg state side (str "uses " (:title card) " to move "
                                                                                (quantify target "advancement counter")
                                                                                " to " (card-str state ice)))
                                                    (effect-completed state side eid))))}}}
                                card nil)))}]
     :abilities [ability]}))

(defcard "Anthill Excavation Contract"
  (let [ability {:once :per-turn
                 :label "Take 4 [Credits] and draw a card (start of turn)"
                 :req (req (:corp-phase-12 @state))
                 :msg (msg "gain " (min 4 (get-counters card :credit)) " [Credits] and draw a card")
                 :async true
                 :automatic :draw-cards
                 :interactive (req true)
                 :effect (req (wait-for (draw state side 1 {:suppress-checkpoint true})
                                        (take-credits state side eid card :credit 4)))}]
    {:data {:counter {:credit 8}}
     :flags {:drip-economy true}
     :abilities [ability]
     :events [(assoc ability :event :corp-turn-begins)
              (trash-on-empty :credit)]}))

(defcard "API-S Keeper Isobel"
  (letfn [(counters-available? [state] (some #(pos? (get-counters % :advancement)) (all-installed state :corp)))]
    {:flags {:corp-phase-12 (req (counters-available? state))}
     :abilities [{:req (req (and (:corp-phase-12 @state)
                                 (counters-available? state)))
                  :once :per-turn
                  :label "Remove an advancement token (start of turn)"
                  :prompt "Choose a card to remove an advancement token from"
                  :choices {:card #(and (pos? (get-counters % :advancement))
                                     (installed? %))}
                  :async true
                  :effect (req (let [cnt (get-counters target :advancement)]
                                 (set-prop state side target :advance-counter (dec cnt))
                                 (system-msg state :corp (str "uses " (:title card) " to remove 1 advancement counter from "
                                                              (card-str state target) " and gains 3 [Credits]"))
                                 (gain-credits state :corp eid 3)))}]}))

(defcard "Aryabhata Tech"
  {:events [{:event :successful-trace
             :msg "gain 1 [Credit] and force the Runner to lose 1 [Credit]"
             :async true
             :effect (req (wait-for (gain-credits state side 1)
                                    (lose-credits state :runner eid 1)))}]})

(defcard "B-1001" ;; note -> run restriction added by rules as errata
  {:abilities [{:req (req (and run (not this-server)))
                :async true
                :cost [(->c :tag 1)]
                :msg "end the run"
                :label "End the run on another server"
                :effect (effect (end-run eid card))}]})

(defcard "Balanced Coverage"
  (let [name-abi
        {:prompt "Choose a card type"
         :waiting-prompt true
         :choices ["Operation" "Asset" "Upgrade" "ICE" "Agenda"]
         :async true
         :msg (msg "choose " target)
         :effect (req (let [named-type target
                            top-card (first (:deck corp))]
                        (wait-for (resolve-ability state side
                                                   {:async true
                                                    :prompt (msg "The top card of R&D is: " (:title top-card))
                                                    :waiting-prompt true
                                                    :choices ["OK"]}
                                                   card nil)
                                  (if (= (:type top-card) named-type)
                                    (continue-ability
                                      state side
                                      {:optional
                                       {:prompt "Reveal it to gain 2 [Credits]?"
                                        :waiting-prompt true
                                        :yes-ability
                                        {:async true
                                         :msg (msg "reveal " (:title top-card)
                                                   " from the top of R&D and gain 2 [Credits]")
                                         :effect (req (wait-for (reveal state side (make-eid state eid) top-card)
                                                                (gain-credits state :corp eid 2)))}
                                        :no-ability {:effect (effect (system-msg (str "declines to use " (:title card) " to reveal the top card of R&D")))}}}
                                      card nil)
                                    (do (system-msg state side (str "declines to use " (:title card) " to reveal the top card of R&D"))
                                        (effect-completed state side eid))))))}
        ability {:label "Look at the top card of R&D (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (req (continue-ability
                                state side
                                name-abi
                                card nil))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Bass CH1R180G4"
  {:abilities [{:action true
                :cost [(->c :click 1) (->c :trash-can)]
                :msg "gain [Click][Click]"
                :effect (effect (gain-clicks 2))}]})

(defcard "Behold!"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:req (req (not (in-discard? card)))
                :waiting-prompt true
                :prompt (msg "Pay 4 [Credits] to use " (:title card) " ability?")
                :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}
                :yes-ability (assoc (give-tags 2) :cost [(->c :credit 4)])}}})

(defcard "Bio-Ethics Association"
  (let [ability {:req (req unprotected)
                 :automatic :corp-damage
                 :async true
                 :label "Do 1 net damage (start of turn)"
                 :once :per-turn
                 :msg "do 1 net damage"
                 :effect (effect (damage eid :net 1 {:card card}))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Bioroid Work Crew"
  {:implementation "Timing restriction of ability use not enforced"
   :abilities [{:label "Install 1 card, paying all costs"
                :req (req (= (:active-player @state) :corp))
                :change-in-game-state {:req (req (seq (:hand corp)))}
                :prompt "Choose a card in HQ to install"
                :choices {:card #(and (not (operation? %))
                                      (in-hand? %)
                                      (corp? %))}
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                         :display-origin true}}))}]})

(defcard "Blacklist"
  {:on-rez {:effect (effect (lock-zone (:cid card) :runner :discard))}
   :leave-play (effect (release-zone (:cid card) :runner :discard))})

(defcard "Bladderwort"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :automatic :pre-gain-credits
                 :interactive (req true)
                 :async true
                 :effect (req (wait-for (gain-credits state side 1)
                                        (if (<= (:credit (:corp @state)) 4)
                                          (continue-ability
                                            state side
                                            {:msg "do 1 net damage"
                                             :async true
                                             :effect (effect (damage eid :net 1 {:card card}))}
                                            card nil)
                                          (effect-completed state side eid))))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Brain-Taping Warehouse"
  {:static-abilities [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (has-subtype? target "Bioroid")))
                       :value (req (- (:click runner)))}]})

(defcard "Breached Dome"
  {:flags {:rd-reveal (req true)}
   :poison true
   :on-access {:async true
               :effect (req (let [c (first (get-in @state [:runner :deck]))]
                              (system-msg state :corp (str "uses " (:title card) " to do 1 meat damage"
                                                           " and to trash " (:title c)
                                                           " from the top of the stack"))
                              (wait-for (mill state :corp :runner 1)
                                        (damage state side eid :meat 1 {:card card}))))}})

(defcard "Broadcast Square"
  {:prevention [{:prevents :bad-publicity
                 :type :event
                 :max-uses 1
                 :mandatory true
                 :ability {:req (req (preventable? context))
                           :trace {:base 3
                                   :successful {:msg "prevent all bad publicity"
                                                :async true
                                                :effect (req (prevent-bad-publicity state side eid :all))}}}}]})

(defcard "Byte!"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:req (req (and (not (in-discard? card))
                               (can-pay? state :corp eid card nil [(->c :credit 4)])))
                :waiting-prompt true
                :prompt (msg "Pay 4 [Credits] to use " (:title card) " ability?")
                :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}
                :yes-ability {:async true
                              :cost [(->c :credit 4)]
                              :msg "give the Runner 1 tag and do 3 net damage"
                              :effect (req (wait-for (gain-tags state :corp 1 {:suppress-checkpoint true})
                                                     (damage state side eid :net 3 {:card card})))}}}})

(defcard "C.I. Fund"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (pos? (:credit corp)))}
   :abilities [{:label "Store up to 3 [Credit] (start of turn)"
                :prompt "How many credits do you want to store?"
                :once :per-turn
                :choices {:number (req (min (:credit corp) 3))}
                :async true
                :effect (req (wait-for (add-counter state side card :credit target nil)
                                       (lose-credits state side eid target)))
                :msg (msg "store " target " [Credit]")}
               {:label "Take all hosted credits"
                :cost [(->c :credit 2) (->c :trash-can)]
                :msg (msg "trash it and gain " (get-counters card :credit) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (get-counters card :credit)))}]
   :events [{:event :corp-turn-begins
             :msg "place 2 [Credits] on itself"
             :req (req (>= (get-counters card :credit) 6))
             :async true
             :effect (req (add-counter state side eid card :credit 2 nil))}]})

(defcard "Calvin B4L3Y"
  {:abilities [(draw-abi 2 nil {:action true
                                :cost [(->c :click 1)]
                                :once :per-turn})]
   :on-trash {:interactive (req true)
              :optional
              {:req (req (= :runner side))
               :waiting-prompt true
               :prompt "Draw 2 cards?"
               :yes-ability (draw-abi 2)}}})

(defcard "Capital Investors"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :msg "gain 2 [Credits]"
                :keep-menu-open :while-clicks-left
                :async true
                :effect (effect (gain-credits eid 2))}]})

(defcard "Cerebral Overwriter"
  (advance-ambush 3 {:async true
                     :waiting-prompt true
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :msg (msg "do " (get-counters (get-card state card) :advancement) " core damage")
                     :effect (effect (damage eid :brain (get-counters (get-card state card) :advancement) {:card card}))}))

(defcard "Chairman Hiro"
  {:static-abilities [(runner-hand-size+ -2)]
   :on-trash executive-trash-effect})

(defcard "Charlotte Caçador"
  (let [choice-abi
        {:label "Gain 4 [Credits] and draw 1 card"
         :optional
         {:once :per-turn
          :req (req (and (pos? (get-counters card :advancement))
                         (:corp-phase-12 @state)))
          :prompt "Remove 1 hosted advancement counter to gain 4 [Credits] and draw 1 card?"
          :yes-ability
          {:msg "remove 1 hosted advancement counter from itself to gain 4 [Credits] and draw 1 card"
           :async true
           :effect (req
                     (wait-for
                       (add-prop state :corp card :advance-counter -1)
                       (wait-for
                         (gain-credits state side 4)
                         (draw state side eid 1))))}}}
        queue-ability {:interactive (req true)
                       :skippable true
                       :event :corp-turn-begins
                       :req (req (and (not-used-once? state {:once :per-turn} card)
                                      (:corp-phase-12 @state)))
                       :async true
                       :effect (req (continue-ability state side choice-abi card nil))}
        trash-ab {:cost [(->c :advancement 1) (->c :trash-can)]
                  :label "Gain 3 [Credits]"
                  :msg (msg "gain 3 [Credits]")
                  :async true
                  :effect (req (gain-credits state :corp eid 3))}]
    {:advanceable :always
     :flags {:corp-phase-12 (req true)}
     :derezzed-events [corp-rez-toast]
     :events [queue-ability]
     :abilities [choice-abi trash-ab]}))

(defcard "Chekist Scion"
  (advance-ambush 0 {:msg (msg "give the Runner " (quantify (inc (get-counters (get-card state card) :advancement)) "tag"))
                     :async true
                     :effect (effect (gain-tags :corp eid (inc (get-counters (get-card state card) :advancement))))}))

(defcard "Chief Slee"
  {:events [{:event :end-of-encounter
             :req (req (pos? (count (remove :broken (:subroutines (:ice context))))))
             :msg (req (let [unbroken-count (count (remove :broken (:subroutines (:ice context))))]
                         (str "place " (quantify unbroken-count "power counter") " on itself")))
             :async true
             :effect (req (add-counter state :corp eid card :power (count (remove :broken (:subroutines (:ice context)))) nil))}]
   :abilities [{:action true
                :cost [(->c :click 1) (->c :power 5)]
                :keep-menu-open :while-5-power-tokens-left
                :async true
                :msg "do 5 meat damage"
                :effect (effect (damage eid :meat 5 {:card card}))}]})

(defcard "City Surveillance"
  {:derezzed-events [corp-rez-toast]
   :flags {:runner-phase-12 (req true)}
   :events [{:event :runner-turn-begins
             :player :runner
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req [(when (can-pay? state :runner eid card nil [(->c :credit 1)])
                              "Pay 1 [Credits]")
                            "Take 1 tag"])
             :msg (msg (if (= target "Take 1 tag")
                          "give the runner 1 tag"
                          (str "force the runner to " (decapitalize target))))
             :async true
             :effect (req (if (= target "Pay 1 [Credits]")
                            (wait-for (pay state :runner (make-eid state eid) card (->c :credit 1))
                                      (system-msg state :runner (:msg async-result))
                                      (effect-completed state side eid))
                            (gain-tags state :corp eid 1)))}]})

(defcard "Clearinghouse"
  (let [ability {:once :per-turn
                 :async true
                 :label "Trash this asset to do 1 meat damage for each hosted advancement counter (start of turn)"
                 :interactive (req true)
                 :req (req (:corp-phase-12 @state))
                 :effect
                 (effect
                  (continue-ability
                   {:optional
                    {:prompt (msg "Trash this asset to do " (get-counters card :advancement) " meat damage?")
                     :yes-ability
                     {:async true
                      :msg "do 1 meat damage for each hosted advancement counter"
                      :effect (req (wait-for
                                    (trash state side card {:cause-card card})
                                    (damage state side eid :meat (get-counters card :advancement) {:card card})))}}}
                   card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :advanceable :always
     :abilities [ability]}))

(defcard "Clone Suffrage Movement"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (and (some operation? (:discard corp))
                                 unprotected))}
   :abilities [(into
                 (corp-recur operation?)
                 {:label "Add 1 operation from Archives to HQ"
                  :waiting-prompt true
                  :prompt "Choose an operation in Archives to add to HQ"
                  :once :per-turn})]})

(defcard "Clyde Van Rite"
  (let [ability {:async true
                 :req (req (or (can-pay? state :runner eid card nil [(->c :credit 1)])
                               (seq (:deck runner))))
                 :player :runner
                 :once :per-turn
                 :prompt "Choose one"
                 :waiting-prompt true
                 :choices (req [(when (can-pay? state :runner eid card nil [(->c :credit 1)])
                                  "Pay 1 [Credits]")
                                (when (or (not (can-pay? state :runner eid card nil [(->c :credit 1)]))
                                          (seq (:deck runner)))
                                  "Trash the top card of the stack")])
                 :label "make the Runner pay 1 [Credits] or trash the top card of the stack (start of turn)"
                 :msg (msg "force the Runner to " (decapitalize target))
                 :effect (req (if (= target "Pay 1 [Credits]")
                                (wait-for (pay state side (make-eid state eid) card (->c :credit 1))
                                  (system-msg state side (:msg async-result))
                                  (effect-completed state side eid))
                                (mill state :runner eid :runner 1)))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Cohort Guidance Program"
  {:flags {:corp-phase-12 (req true)}
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :skippable true
             :prompt "Choose one"
             :interactive (req true)
             :choices (req [(when (seq (:hand corp)) "Trash 1 card from HQ to gain 2 [Credits] and draw 1 card")
                            (when (some #(not (:seen %)) (:discard corp))
                              "Turn 1 facedown card in Archives faceup to place 1 advancement counter on an installed card")
                            "Done"])
             :async true
             :effect (req (if (= target "Done")
                            (effect-completed state side eid)
                            (continue-ability
                              state side
                              (if (= target "Trash 1 card from HQ to gain 2 [Credits] and draw 1 card")
                                {:prompt "Choose a card to trash"
                                 :msg "trash a card from HQ to gain 2 [Credits] and draw 1 card"
                                 :choices {:max 1
                                           :all true
                                           :card #(and (corp? %)
                                                       (in-hand? %))}
                                 :async true
                                 :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                                                        (wait-for (gain-credits state side 2)
                                                                  (draw state side eid 1))))}
                                {:prompt "Choose a card to turn faceup"
                                 :choices {:card #(and (in-discard? %)
                                                       (corp? %)
                                                       (not (:seen %)))}
                                 :msg (msg "turn " (:title target) " in Archives faceup")
                                 :show-discard true
                                 :async true
                                 :effect (req (update! state side (assoc target :seen true))
                                              (continue-ability
                                                state side
                                                {:prompt "Choose an installed card"
                                                 :choices {:card #(and (corp? %)
                                                                       (installed? %))}
                                                 :msg (msg "place 1 advancement counter on "
                                                           (card-str state target))
                                                 :async true
                                                 :effect (effect
                                                           (add-prop eid target
                                                                     :advance-counter 1
                                                                     {:placed true}))}
                                                card nil))})
                              card nil)))}]})

(defcard "Commercial Bankers Group"
  (let [ability {:req (req unprotected)
                 :automatic :gain-credits
                 :label "Gain 3 [Credits] (start of turn)"
                 :once :per-turn
                 :msg "gain 3 [Credits]"
                 :async true
                 :effect (effect (gain-credits eid 3))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Constellation Protocol"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12
           (req (let [a-token (->> (all-installed state :corp)
                                   (filter ice?)
                                   (filter #(pos? (get-counters % :advancement)))
                                   (remove empty?)
                                   first
                                   :title)]
                  (as-> (all-installed state :corp) it
                    (filter ice? it)
                    (filter #(can-be-advanced? state %) it)
                    (remove empty? it)
                    (map :title it)
                    (split-with (partial not= a-token) it)
                    (concat (first it) (-> it rest first rest))
                    (count it)
                    (pos? it))))}
   :abilities [{:label "Move an advancement counter between 2 pieces of ice"
                :once :per-turn
                :waiting-prompt true
                :choices {:card #(and (ice? %)
                                      (get-counters % :advancement))}
                :async true
                :effect (effect
                          (continue-ability
                            (let [from-ice target]
                              {:prompt "Choose a piece of ice that can be advanced"
                               :choices {:req (req (and (ice? target)
                                                        (not (same-card? from-ice target))
                                                        (can-be-advanced? state target)))}
                               :msg (msg "move an advancement token from "
                                         (card-str state from-ice)
                                         " to "
                                         (card-str state target))
                               :async true
                               :effect (req (wait-for (add-prop state :corp target :advance-counter 1 {:placed true})
                                                      (add-prop state :corp eid from-ice :advance-counter -1)))})
                            card nil))}]})

(defcard "Contract Killer"
  {:advanceable :always
   :abilities [{:action true
                :label "Trash a connection"
                :async true
                :cost [(->c :click 1) (->c :trash-can)]
                :req (req (>= (get-counters card :advancement) 2))
                :choices {:card #(has-subtype? % "Connection")}
                :msg (msg "trash " (:title target))
                :effect (effect (trash eid target {:cause-card card}))}
               {:action true
                :label "Do 2 meat damage"
                :async true
                :cost [(->c :click 1) (->c :trash-can)]
                :req (req (>= (get-counters card :advancement) 2))
                :msg "do 2 meat damage"
                :effect (effect (damage eid :meat 2 {:card card}))}]})

(defcard "Corporate Town"
  (let [ability {:label "Trash a resource"
                 :once :per-turn
                 :async true
                 :prompt "Choose a resource to trash"
                 :choices {:card resource?}
                 :msg (msg "trash " (:title target))
                 :interactive (req true)
                 :req (req (some resource? (all-installed state :runner)))
                 :effect (effect (trash eid target {:unpreventable true :cause-card card}))}]
    {:derezzed-events [corp-rez-toast]
     :additional-cost [(->c :forfeit)]
     :flags {:corp-phase-12 (req (and (rezzed? card)
                                      (->> (all-active-installed state :runner)
                                           (filter resource?)
                                           count
                                           pos?)))}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "CPC Generator"
  {:events [{:event :runner-credit-gain
             :req (req (first-event? state side :runner-credit-gain
                                     (fn [[context]]
                                       (= :runner-click-credit (:action context)))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "CSR Campaign"
  (let [ability {:once :per-turn
                 :async true
                 :label "Draw 1 card (start of turn)"
                 :automatic :draw-cards
                 :interactive (req true)
                 :effect (effect (continue-ability
                                   {:optional
                                    {:prompt "Draw 1 card?"
                                     :autoresolve (get-autoresolve :auto-fire)
                                     :yes-ability (draw-abi 1)}}
                                   card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability (set-autoresolve :auto-fire "CSR Campaign")]}))

(defcard "Cybernetics Court"
  {:static-abilities [(corp-hand-size+ 4)]})

(defcard "Cybersand Harvester"
  {:events [{:event :rez
             :req (req (ice? (:card context)))
             :msg "place 2 [Credits] on itself"
             :async true
             :effect (effect (add-counter :corp eid card :credit 2 nil))}]
   :abilities [{:label "Take all hosted credits"
                :cost [(->c :trash-can)]
                :change-in-game-state {:req (req (pos? (get-counters card :credit)))}
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (get-counters card :credit)))}
               {:async true
                :effect (req (spend-credits state side eid card :credit 1))
                :label "Take 1 hosted [Credits] (manual)"
                :msg "take 1 hosted [Credits]"}]
   :interactions {:pay-credits {:req (req (= :corp-install (:source-type eid)))
                                :type :credit}}})

(defcard "Daily Business Show"
  {:derezzed-events [corp-rez-toast]
   :events [(first-time-draw-bonus :corp 1)
            {:event :corp-draw
             :req (req (first-event? state :corp :corp-draw))
             :once :per-turn
             :once-key :daily-business-show-put-bottom
             :interactive (req true)
             :silent (req (let [dbs (filter #(and (= "Daily Business Show" (:title %))
                                                  (rezzed? %))
                                            (all-installed state :corp))]
                            (not= card (first dbs))))
             :async true
             :effect (req (let [dbs (count (filter #(and (= "Daily Business Show" (:title %))
                                                         (rezzed? %))
                                                   (all-installed state :corp)))
                                drawn corp-currently-drawing]
                            (continue-ability
                              state side
                              (when (seq drawn)
                                {:waiting-prompt true
                                 :prompt (str "Choose " (quantify dbs "card") " to add to the bottom of R&D")
                                 :choices {:max (min dbs (count drawn))
                                           :card #(some (fn [c] (same-card? c %)) drawn)
                                           :all true}
                                 :effect (req (doseq [c (reverse targets)]
                                                (system-msg state side
                                                            (str "uses " (:title card) " to add the "
                                                                 (pprint/cl-format nil "~:R" (inc (first (keep-indexed #(when (same-card? c %2) %1) drawn))))
                                                                 " card drawn to the bottom of R&D"))
                                                (move state side c :deck)
                                                (remove-from-currently-drawing state side c)))})
                              card nil)))}]})

(defcard "Daily Quest"
  (let [ability {:req (req (not (some (into #{}
                                            [(second (get-zone card))
                                             (second (get-zone (:host card)))])
                                      (:successful-run runner-reg-last))))
                 :label "gain 3 [Credits] (start of turn)"
                 :automatic :gain-credits
                 :msg "gain 3 [Credits]"
                 :async true
                 :effect (effect (gain-credits :corp eid 3))}]
    {:rez-req (req (= (:active-player @state) :corp))
     :events [{:event :successful-run
               :req (req this-server)
               :async true
               :effect (effect (system-msg :runner (str "gains 2 [Credits] for a successful run "
                                                        "on the Daily Quest server"))
                               (gain-credits :runner eid 2))}
              (assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Dedicated Response Team"
  {:events [(assoc (do-meat-damage 2)
                   :event :run-ends
                   :req (req (and tagged (:successful target))))]})

(defcard "Dedicated Server"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :rez (:source-type eid))
                                               (ice? target)))
                                :type :recurring}}})

(defcard "Director Haas"
  {:in-play [:click-per-turn 1]
   :on-trash executive-trash-effect})

(defcard "Docklands Crackdown"
  (letfn [(not-triggered? [state] (no-event? state :runner :runner-install))]
    {:abilities [{:action true
                  :cost [(->c :click 2)]
                  :keep-menu-open :while-2-clicks-left
                  :msg "place 1 power counter in itself"
                  :async true
                  :effect (effect (add-counter eid card :power 1 nil))}]
     :static-abilities [{:type :install-cost
                         :req (req (and (runner? target)
                                        (not-triggered? state)))
                         :value (req (get-counters card :power))}]
     :events [{:event :runner-install
               :silent (req true)
               :req (req (and (pos? (get-counters card :power))
                              (not-triggered? state)))
               :msg (msg "increase the install cost of " (:title (:card context))
                         " by " (get-counters card :power) " [Credits]")}]}))

(defcard "Dr. Vientiane Keeling"
  {:static-abilities [(runner-hand-size+ (req (- (get-counters card :power))))]
   :on-rez gain-power-counter
   :events [(assoc gain-power-counter :event :corp-turn-begins)]})

(defcard "Drago Ivanov"
  {:advanceable :always
   :abilities [{:cost [(->c :advancement 2)]
                :req (req (= :corp (:active-player @state)))
                :msg "give the runner a tag"
                :async true
                :effect (effect (gain-tags :corp eid 1))}]})

(defcard "Drudge Work"
  {:data {:counter {:power 3}}
   :events [(trash-on-empty :power)]
   :abilities [{:action true
                :cost [(->c :click 1) (->c :power 1)]
                :choices {:card #(and (agenda? %)
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                :label "Reveal an agenda from HQ or Archives"
                :msg (msg "reveal " (:title target) " from " (zone->name (get-zone target))
                          (let [target-agenda-points (get-agenda-points target)]
                            (str ", gain " target-agenda-points " [Credits], "))
                          " and shuffle it into R&D")
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (wait-for
                                 (gain-credits state :corp (get-agenda-points target))
                                 (move state :corp target :deck)
                                 (shuffle! state :corp :deck)
                                 (effect-completed state side eid))))}]})

(defcard "Early Premiere"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (some #(and (can-be-advanced? state %)
                                           (in-server? %))
                                     (all-installed state :corp)))}
   :abilities [{:cost [(->c :credit 1)]
                :label "Place 1 advancement token on a card that can be advanced in a server"
                :choices {:req (req (and (can-be-advanced? state target)
                                         (installed? target)
                                         (in-server? target)))} ; should be *in* a server
                :once :per-turn
                :msg (msg "place 1 advancement token on " (card-str state target))
                :async true
                :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}]})

(defcard "Echo Chamber"
  {:abilities [{:action true
                :label "Add this asset to your score area as an agenda worth 1 agenda point"
                :cost [(->c :click 3)]
                :msg (msg "add itself to [their] score area as an agenda worth 1 agenda point")
                :effect (req (as-agenda state :corp card 1))}]})

(defcard "Edge of World"
  (letfn [(ice-count [state]
            (count (get-in (:corp @state) [:servers (last (:server (:run @state))) :ices])))]
    (installed-access-trigger 3 {:msg (msg "do " (ice-count state) " core damage")
                                 :async true
                                 :effect (effect (damage eid :brain (ice-count state)
                                                         {:card card}))})))

(defcard "Eliza's Toybox"
  {:abilities [{:action true
                :cost [(->c :click 3)]
                :keep-menu-open :while-3-clicks-left
                :label "Rez a card, ignoring all costs"
                :choices {:card (every-pred corp? installed? (complement agenda?) (complement rezzed?))}
                :async true
                :effect (effect (rez eid target {:ignore-cost :all-costs :msg-keys {:include-cost-from-eid eid}}))}]})

(defcard "Elizabeth Mills"
  {:on-rez {:msg "remove 1 bad publicity"
            :effect (effect (lose-bad-publicity 1))}
   :abilities [{:action true
                :cost [(->c :click 1) (->c :trash-can)]
                :label "Trash a location and take 1 bad publicity"
                :async true
                :effect (req (if (some #(has-subtype? % "Location") (all-installed state :runner))
                               (continue-ability
                                 state side
                                 {:prompt "Trash a location and take 1 bad publicity"
                                  :msg (msg "trash " (:title target) " and take 1 bad publicity")
                                  :choices {:min 1
                                            :card #(has-subtype? % "Location")}
                                  :async true
                                  :effect (req (wait-for (trash state side target {:cause-card card})
                                                         (gain-bad-publicity state :corp eid 1)))}
                                 card nil)
                               (continue-ability
                                 state side
                                 {:msg "take 1 bad publicity"
                                  :async true
                                  :effect (effect (gain-bad-publicity eid 1))}
                                 card nil)))}]})

(defcard "Encryption Protocol"
  {:static-abilities [{:type :trash-cost
                       :req (req (installed? target))
                       :value 1}]})

(defcard "Estelle Moon"
  {:events [{:event :corp-install
             :req (req (and (or (asset? (:card context))
                                (agenda? (:card context))
                                (upgrade? (:card context)))
                            (is-remote? (second (get-zone (:card context))))))
             :msg "place 1 power counter on itself"
             :async true
             :effect (effect (add-counter eid card :power 1 nil))}]
   :abilities [{:label "Draw 1 card and gain 2 [Credits] for each hosted power counter"
                :cost [(->c :trash-can)]
                :change-in-game-state {:req (req (pos? (get-counters card :power)))}
                :async true
                :effect (req (let [counters (get-counters card :power)
                                   credits (* 2 counters)]
                               (system-msg state side (str "uses " (:title card) " to draw " (quantify counters "card")
                                                           " and gain " credits " [Credits]"))
                               (wait-for (draw state side counters)
                                         (gain-credits state side eid credits))))}]})

(defcard "Eve Campaign"
  (campaign 16 2))

(defcard "Executive Boot Camp"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (some #(not (rezzed? %)) (all-installed state :corp)))}
   ; A card rezzed by Executive Bootcamp is ineligible to receive the turn-begins event for this turn.
   :suppress [{:event :corp-turn-begins
               :req (req (= (:cid target) (:ebc-rezzed (get-card state card))))}]
   :events [{:event :corp-turn-ends
             :silent (req true)
             :req (req (:ebc-rezzed card))
             :effect (effect (update! (dissoc card :ebc-rezzed)))}]
   :abilities [{:async true
                :once :per-turn
                :choices {:req (req (and (corp? target)
                                         (not (rezzed? target))
                                         (can-pay-to-rez? state side (assoc eid :source card)
                                                          target {:cost-bonus -1})))}
                :label "Rez a card, lowering the cost by 1 [Credits] (start of turn)"
                :effect (req (wait-for (rez state side target {:no-warning true :cost-bonus -1})
                                       (update! state side (assoc card :ebc-rezzed (:cid target)))
                                       (effect-completed state side eid)))}
               {:prompt "Choose an asset to reveal and add to HQ"
                :msg (msg "reveal " (:title target) ", add it to HQ, and shuffle R&D")
                :choices (req (cancellable (filter asset?
                                                   (:deck corp))
                                           :sorted))
                :cost [(->c :credit 1) (->c :trash-can)]
                :label "Search R&D for an asset"
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (shuffle! state side :deck)
                               (move state side target :hand)
                               (effect-completed state side eid)))}]})

(defcard "Executive Search Firm"
  {:abilities [{:action true
                :prompt "Choose an Executive, Sysop, or Character to add to HQ"
                :msg (msg "reveal " (:title target) ", add it to HQ, and shuffle R&D")
                :choices (req (cancellable (filter #(has-any-subtype? % ["Executive" "Sysop" "Character"])
                                                   (:deck corp))
                                           :sorted))
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :label "Search R&D for an Executive, Sysop, or Character"
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(defcard "Exposé"
  {:advanceable :always
   :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                :msg (msg "remove " (get-counters card :advancement) " bad publicity")
                :cost [(->c :trash-can)]
                :effect (effect (lose-bad-publicity (get-counters card :advancement)))}]})

(defcard "False Flag"
  (letfn [(tag-count [false-flag]
            (int (/ (get-counters false-flag :advancement) 2)))]
    {:advanceable :always
     :on-access {:req (req (pos? (get-counters (get-card state card) :advancement)))
                 :msg (msg "give the runner " (quantify (tag-count (get-card state card)) "tag"))
                 :async true
                 :effect (effect (gain-tags :corp eid (tag-count (get-card state card))))}
     :abilities [{:action true
                  :cost [(->c :click 1) (->c :advancement 7)]
                  :label "Add this asset to your score area as an agenda worth 3 agenda points"
                  :msg (msg "add itself to [their] score area as an agenda worth 3 agenda points")
                  :effect (req (as-agenda state :corp card 3))}]}))

(defcard "Federal Fundraising"
  (let [draw-ab {:optional {:req (req unprotected)
                            :prompt "Draw 1 card?"
                            :waiting-prompt true
                            :yes-ability {:msg "draw 1 card"
                                          :async true
                                          :effect (effect (draw eid 1))}
                            :no-ability {:effect (effect (system-msg (str "declines to use " (:title card) " to draw 1 card")))}}}
        ability
        {:once :per-turn
         :req (req (and (:corp-phase-12 @state)
                        (not-empty (:deck corp))))
         :skippable true
         :interactive (req true)
         :label "Look at the top 3 cards of R&D (start of turn)"
         :async true
         :effect
         (effect
          (continue-ability
           {:optional
            {:prompt "Look at the top 3 cards of R&D?"
             :waiting-prompt true
             :no-ability
             {:async true
              :effect (effect (continue-ability draw-ab card nil))}
             :yes-ability
             {:msg "rearrange the top 3 cards of R&D"
              :async true
              :waiting-prompt true
              :effect (req (let [from (take 3 (:deck corp))]
                             (wait-for (resolve-ability
                                         state side
                                         (reorder-choice :corp :runner from '() (count from) from)
                                          card nil)
                                       (continue-ability state side draw-ab card nil))))}}}
           card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Franchise City"
  {:events [{:event :access
             :req (req (agenda? target))
             :msg "add itself to [their] score area as an agenda worth 1 agenda point"
             :effect (req (as-agenda state :corp card 1))}]})

(defcard "Front Company"
  {:static-abilities [{:type :cannot-run-on-server
                       :req (req (not (pos? (count (turn-events state side :run)))))
                       :value (req (map first (get-remotes state)))}]
   :rez-req (req (= (:active-player @state) :corp))
   :events [{:event :run
             :req (req (and (= :archives (target-server context))
                            (first-event? state :runner :run #(= :archives (target-server (first %))))
                            unprotected))
             :msg "do 2 net damage"
             :async true
             :effect (effect (damage eid :net 2))}]})

(defcard "Full Immersion RecStudio"
  {:can-host (req (and (or (asset? target) (agenda? target))
                       (> 2 (count (:hosted card)))))
   :trash-cost-bonus (req (* 3 (count (:hosted card))))
   :abilities [{:action true
                :label "Install an asset or agenda on this asset"
                :req (req (< (count (:hosted card)) 2))
                :cost [(->c :click 1)]
                :prompt "Choose an asset or agenda to install"
                :choices {:card #(and (or (asset? %)
                                          (agenda? %))
                                      (in-hand? %)
                                      (corp? %))}
                :msg "install and host an asset or agenda"
                :async true
                :effect (effect (corp-install eid target card nil))}
               {:label "Install a previously-installed asset or agenda on this asset (fixes only)"
                :req (req (< (count (:hosted card)) 2))
                :prompt "Choose an installed asset or agenda to host"
                :choices {:card #(and (or (asset? %) (agenda? %))
                                      (installed? %)
                                      (corp? %))}
                :msg "install and host an asset or agenda"
                :effect (req (host state side card target))}]})

(defcard "Fumiko Yamamori"
  {:events [{:event :reveal-spent-credits
             :async true
             :req (req (and (some? (first targets))
                            (some? (second targets))
                            (not= (first targets) (second targets))))
             :msg "do 1 meat damage"
             :effect (effect (damage eid :meat 1 {:card card}))}]})

(defcard "Gaslight"
  (let [search-for-operation {:prompt "Choose an operation to add to HQ"
                              :waiting-prompt true
                              :msg (msg (if (= target "Done")
                                          "shuffle R&D"
                                          (str "add " (get-title target) " from R&D to HQ")))
                              :choices (req (conj (vec (sort-by :title (filter operation? (:deck corp)))) "Done"))
                              :async true
                              :effect (req (if (= target "Done")
                                             (do (shuffle! state :corp :deck)
                                                 (effect-completed state side eid))
                                             (wait-for
                                               (reveal state side target)
                                               (shuffle! state :corp :deck)
                                               (move state :corp target :hand)
                                               (effect-completed state side eid))))}
        ability {:once :per-turn
                 :skippable true
                 :async true
                 :label "Search R&D for an operation (start of turn)"
                 :interactive (req true)
                 :req (req (:corp-phase-12 @state))
                 :effect
                 (effect
                   (continue-ability
                     {:optional
                      {:prompt "Trash this asset to search R&D for an operation?"
                       :yes-ability
                       {:async true
                        :effect (req (wait-for (trash state side card {:cause-card card})
                                               (continue-ability state side search-for-operation card nil)))}}}
                     card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Gene Splicer"
  {:advanceable :always
   :on-access {:req (req (pos? (get-counters (get-card state card) :advancement)))
               :msg (msg "do " (get-counters (get-card state card) :advancement) " net damage")
               :async true
               :effect (effect (damage eid :net (get-counters (get-card state card) :advancement)
                                       {:card card}))}
   :abilities [{:action true
                :cost [(->c :click 1) (->c :advancement 3)]
                :label "Add this asset to your score area as an agenda worth 1 agenda point"
                :msg "add itself to [their] score area as an agenda worth 1 agenda point"
                :effect (req (as-agenda state :corp card 1))}]})

(defcard "Genetics Pavilion"
  {:on-rez {:msg (msg "prevent the Runner from drawing more than 2 cards during [runner-pronoun] turn")
            :effect (req (max-draw state :runner 2)
                         (when (zero? (remaining-draws state :runner))
                           (prevent-draw state :runner)))}
   :events [{:event :runner-turn-begins
             :silent (req true)
             :effect (effect (max-draw :runner 2))}]
   :leave-play (req (swap! state update-in [:runner :register] dissoc :max-draw :cannot-draw))})

(defcard "Ghost Branch"
  (advance-ambush 0 {:async true
                     :waiting-prompt true
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :msg (msg "give the Runner " (quantify (get-counters (get-card state card) :advancement) "tag"))
                     :effect (effect (gain-tags :corp eid (get-counters (get-card state card) :advancement)))}))

(defcard "GRNDL Refinery"
  {:advanceable :always
   :abilities [{:action true
                :label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                :cost [(->c :click 1) (->c :trash-can)]
                :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (* 4 (get-counters card :advancement))))}]})

(defcard "Haas Arcology AI"
  {:advanceable :while-unrezzed
   :abilities [{:action true
                :label "Gain [Click][Click]"
                :once :per-turn
                :msg "gain [Click][Click]"
                :cost [(->c :click 1) (->c :advancement 1)]
                :effect (effect (gain-clicks 2))}]})

(defcard "Hearts and Minds"
  (let [political {:req (req unprotected)
                   :prompt "Choose a card you can advance to place 1 advancement counter on"
                   :choices {:req (req (and (can-be-advanced? state target)
                                            (installed? target)))}
                   :msg (msg "place 1 advancement counter on " (card-str state target))
                   :async true
                   :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}
        ability {:req (req (:corp-phase-12 @state))
                 :label "Move 1 hosted advancement counter to another card you can advance (start of turn)"
                 :skippable true
                 :once :per-turn
                 :waiting-prompt true
                 :prompt "Choose an installed card to move 1 hosted advancement counter from"
                 :choices {:card #(and (installed? %)
                                       (pos? (get-counters % :advancement)))}
                 :async true
                 :effect (effect
                           (continue-ability
                             (let [from-ice target]
                               {:prompt "Choose an installed card you can advance"
                                :choices {:req (req (and (installed? target)
                                                         (can-be-advanced? state target)
                                                         (not (same-card? from-ice target))))}
                                :msg (msg "move 1 hosted advancement counter from "
                                          (card-str state from-ice)
                                          " to "
                                          (card-str state target))
                                :async true
                                :effect (req (wait-for
                                                  (add-prop state :corp target :advance-counter 1 {:placed true})
                                                  (wait-for
                                                    (add-prop state :corp from-ice :advance-counter -1)
                                                    (continue-ability state :corp political card nil))))
                                :cancel-effect (effect (continue-ability political card nil))})
                             card nil))
                 :cancel-effect (effect (continue-ability political card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Honeyfarm"
  {:flags {:rd-reveal (req true)}
   :poison true
   :on-access {:msg "force the Runner to lose 1 [Credits]"
               :async true
               :effect (effect (lose-credits :runner eid 1))}})

(defcard "Hostile Architecture"
  (letfn [(valid-ctx?
            [evs]
            (some #(and (->> % :card corp?)
                        (->> % :card installed?))
                  evs))]
    {:events [{:event :runner-trash
               :async true
               :once-per-instance true
               :req (req (and (valid-ctx? targets)
                              (first-event? state side :runner-trash valid-ctx?)))
               :msg "do 2 meat damage"
               :effect (effect (damage :corp eid :meat 2 {:card card}))}]}))

(defcard "Hostile Infrastructure"
  {:events [{:event :runner-trash
             :async true
             :once-per-instance false
             :req (req (corp? (:card target)))
             :msg "do 1 net damage"
             :effect (effect (damage :corp eid :net 1 {:card card}))}]})

(defcard "Humanoid Resources"
  (let [play-an-instant
        {:prompt "Choose an operation"
         :choices (req (conj (filter #(and (operation? %)
                                           (should-trigger? state :corp (assoc eid :source % :source-type :play) % nil (or (:on-play (card-def %)) {}))
                                           (can-pay? state side (assoc eid :source % :source-type :play) % nil [(->c :credit (play-cost state side % nil))]))
                                     (:hand corp))
                             "Done"))
         :async true
         :effect (req (if (= target "Done")
                        (effect-completed state side eid)
                        (play-instant state side eid target nil)))}]
    {:abilities [{:cost [(->c :click 3)(->c :trash-can 1)]
                  :action true
                  :label "Gain 4 [Credits] and draw 3 cards"
                  :msg "gain 4 [Credits] and draw 3 cards"
                  :async true
                  :effect (req (play-sfx state side "professional-contacts")
                               (wait-for
                                 (gain-credits state side 4 {:suppress-checkpoint true})
                                 (wait-for (draw state side 3)
                                   (wait-for
                                     (resolve-ability
                                       state side
                                       (corp-install-up-to-n-cards 2)
                                       card nil)
                                     (continue-ability state side play-an-instant card nil)))))}]}))

(defcard "Hyoubu Research Facility"
  {:events [{:event :reveal-spent-credits
             :req (req (and (some? (first targets))
                            (first-event? state side :reveal-spent-credits)))
             :msg (msg "gain " target " [Credits]")
             :async true
             :effect (effect (gain-credits :corp eid target))}]})

(defcard "Ibrahim Salem"
  (let [trash-ability (fn [card-type]
                        (with-revealed-hand :runner {:event-side :corp}
                          {:req (req (seq (filter #(is-type? % card-type) (:hand runner))))
                           :prompt (str "Choose a " card-type " to trash")
                           :choices {:card #(and (in-hand? %)
                                                 (runner? %)
                                                 (is-type? % card-type))}
                           :async true
                           :effect (effect (trash eid target {:cause-card card}))
                           :msg (msg "trash " (:title target) " from the grip")}))
        choose-ability {:label "Trash 1 card in the grip of a named type"
                        :change-in-game-state {:req (req (seq (:hand runner))) :silent true}
                        :once :per-turn
                        :req (req (seq (:hand runner)))
                        :prompt "Choose a card type"
                        :choices ["Event" "Hardware" "Program" "Resource"]
                        :msg (msg "choose " target)
                        :async true
                        :effect (effect (continue-ability (trash-ability target) card nil))}]
    {:additional-cost [(->c :forfeit)]
     :flags {:corp-phase-12 (req (not (is-disabled-reg? state card)))}
     :derezzed-events [corp-rez-toast]
     :abilities [choose-ability]}))

(defcard "Idiosyncresis"
  (letfn [(adv  [card]        (get-counters card :advancement))
          (lose [card runner] (min (* 2 (adv card)) (:credit runner)))
          (gain [card]        (* 3 (adv card)))]
    (let [abi {:event :corp-turn-begins
               :interactive (req true)
               :skippable true
               :label "Trash Idiosyncresis"
               :optional {:prompt "Trash Idiosyncresis?"
                          :req (req (:corp-phase-12 @state))
                          :yes-ability {:async true
                                        :msg (msg "force the runner to lose " (lose card runner) " [Credits], and then gain " (gain card) " [Credits]")
                                        :cost [(->c :trash-can)]
                                        :effect (req (wait-for (lose-credits state :runner (lose card runner))
                                                               (gain-credits state side eid (gain card))))}}}]
      {:advanceable :always
       :events [abi]
       :abilities [abi]})))

(defcard "Illegal Arms Factory"
  (let [ability {:msg "gain 1 [Credits] and draw 1 card"
                 :label "Gain 1 [Credits] and draw 1 card (start of turn)"
                 :once :per-turn
                 :automatic :draw-cards
                 :async true
                 :req (req (:corp-phase-12 @state))
                 :effect (req (wait-for (gain-credits state side 1)
                                        (draw state side eid 1)))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]
     :on-trash {:req (req (= side :runner))
                :msg "take 1 bad publicity"
                :effect (effect (gain-bad-publicity :corp 1))}}))

(defcard "Indian Union Stock Exchange"
  {:events [{:event :play-operation
             :req (req (not= (:faction (:card context)) (:faction (:identity corp))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}
            {:event :rez
             :req (req (not= (:faction (:card context)) (:faction (:identity corp))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Investigator Inez Delgado A"
  {:events [{:event :agenda-scored
             :interactive (req true)
             :req (req (seq (:scored runner)))
             :async true
             :effect (req
                       (let [scored (:card context)]
                         (continue-ability
                           state side
                           {:optional
                            {:prompt (msg "Swap " (:title scored) " for an agenda in the Runner's score area?")
                             :waiting-prompt true
                             :req (req (seq (get-in @state [:runner :scored])))
                             :yes-ability
                             {:prompt (str "Choose a scored Runner agenda to swap with " (:title scored))
                              :choices {:req (req (and (in-runner-scored? state side target)
                                                       (:agendapoints target)
                                                       (pos? (:agendapoints target))))}
                              :msg (msg "swap " (card-str state scored) " for " (card-str state target))
                              :async true
                              :effect (req (let [new-scored (second (swap-agendas state side scored target))]
                                             (continue-ability
                                               state side
                                               (:on-score (card-def new-scored))
                                               new-scored nil)))}}}
                           card targets)))}]})

(defcard "Investigator Inez Delgado A 2"
  (letfn [(swap-abi [stolen]
            {:prompt (str "Swap " (:title stolen) " with an agenda in your score area?")
             :req (req (seq (:scored corp)))
             :choices {:req (req (in-corp-scored? state side target))}
             :msg (msg "swap " (card-str state stolen) " for " (card-str state target))
             :effect (req (swap-agendas state side target stolen))})]
    {:events [{:event :agenda-stolen
               :interactive (req true)
               :skippable true
               :async true
               :effect (req (let [stolen (:card context)]
                              (if (:on-score (card-def stolen))
                                (continue-ability
                                  state side
                                  {:optional {:prompt (str "Resolve the when-scored ability on " (:title stolen))
                                              :waiting-prompt true
                                              :yes-ability {:async true
                                                            :msg (msg "resolve the when-scored ability on " (:title stolen))
                                                            :effect (req (wait-for
                                                                           (resolve-ability state side (:on-score (card-def stolen)) stolen nil)
                                                                           (if (get-card state stolen)
                                                                             (continue-ability state side (swap-abi stolen) card nil)
                                                                             (effect-completed state side eid))))}
                                              :no-ability (swap-abi stolen)}}
                                  card nil)
                                (continue-ability state side (swap-abi stolen) card nil))))}]}))

(defcard "Isabel McGuire"
  {:abilities [{:action true
                :label "Add an installed card to HQ"
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :choices {:card installed?}
                :msg (msg "move " (card-str state target) " to HQ")
                :effect (effect (move target :hand))}]})

(defcard "IT Department"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :msg "place 1 power counter on itself"
                :async true
                :effect (effect (add-counter eid card :power 1 nil))}
               {:cost [(->c :power 1)]
                :keep-menu-open :while-power-tokens-left
                :label "Add strength to a rezzed piece of ice"
                :choices {:card #(and (ice? %)
                                      (rezzed? %))}
                :req (req (pos? (get-counters card :power)))
                :msg "add strength to a rezzed piece of ice"
                :effect (effect (register-lingering-effect
                                  card
                                  (let [it-target target]
                                    {:type :ice-strength
                                     :duration :end-of-turn
                                     :req (req (same-card? target it-target))
                                     :value (req (inc (get-counters card :power)))}))
                                (update-ice-strength target))}]})

(defcard "Jackson Howard"
  {:abilities [(draw-abi 2 nil {:action true
                                :cost [(->c :click 1)]
                                :keep-menu-open :while-clicks-left})
               {:label "Shuffle up to 3 cards from Archives into R&D"
                :cost [(->c :remove-from-game)]
                :async true
                :effect (effect (shuffle-into-rd-effect eid card 3))}]})

(defcard "Janaína \"JK\" Dumont Kindelán"
  (let [ability {:label "Place 3 [Credits] on this asset (start of turn)"
                 :once :per-turn
                 :msg "place 3 [Credits] on itself"
                 :async true
                 :effect (effect (add-counter eid card :credit 3 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability
                 {:action true
                  :cost [(->c :click 1)]
                  :label "Take all hosted credits and add this asset to HQ. Install 1 card from HQ"
                  :async true
                  :msg (msg "gain " (get-counters (get-card state card) :credit) " [Credits] and add itself to HQ")
                  :effect (req (when (pos? (get-counters (get-card state card) :credit))
                                 (play-sfx state side "click-credit-3"))
                               (wait-for (take-credits state side card :credit :all)
                                         (move state :corp card :hand)
                                         (continue-ability
                                           state side
                                           {:async true
                                            :prompt "Choose 1 card to install"
                                            :choices {:card #(and (corp-installable-type? %)
                                                                  (in-hand? %))}
                                            :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                                                     :display-origin true}}))}
                                           card nil)))}]}))

(defcard "Jeeves Model Bioroids"
  (let [ability {:label "Gain [Click]"
                 :msg "gain [Click]"
                 :once :per-turn
                 :effect (effect (gain-clicks 1))}
        cleanup (effect (update! (dissoc card :seen-this-turn)))]
    {:abilities [ability]
     :leave-play cleanup
     :events [{:event :corp-spent-click
               :async true
               :effect (req (let [{:keys [action value ability-idx]} context
                                  bac-cid (get-in @state [:corp :basic-action-card :cid])
                                  cause (if (keyword? action)
                                          (case action
                                            :play-instant [bac-cid 3]
                                            :corp-click-install [bac-cid 2]
                                            ; else
                                            [action ability-idx])
                                          [action ability-idx])
                                  clicks-spent (+ (get-in card [:seen-this-turn cause] 0) value)
                                  card (update! state side (assoc-in card [:seen-this-turn cause] clicks-spent))]
                              ; can be >= 3 because :once :per-turn on ability
                              (if (>= clicks-spent 3)
                                (resolve-ability state side eid ability card nil)
                                (effect-completed state side eid))))}
              {:event :corp-turn-ends
               :silent (req true)
               :effect cleanup}]}))

(defcard "Kala Ghoda Real TV"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:msg "look at the top card of the stack"
                :change-in-game-state {:req (req (seq (:deck runner)))}
                :async true
                :effect (effect (continue-ability
                                  {:prompt (req (->> runner :deck first :title (str "The top card of the stack is ")))
                                   :waiting-prompt true
                                   :choices ["OK"]}
                                  card nil))}
               {:async true
                :label "Trash the top card of the stack"
                :msg (msg "trash " (:title (first (:deck runner))) " from the stack")
                :cost [(->c :trash-can)]
                :effect (effect (mill :corp eid :runner 1))}]})

(defcard "Kuwinda K4H1U3"
  {:x-fn (req (get-counters card :power))
   :derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Trace X - do 1 core damage (start of turn)"
                :trace {:base (get-x-fn)
                        :successful
                        {:async true
                         :msg "do 1 core damage"
                         :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                (trash state side eid card {:cause-card card})))}
                        :unsuccessful
                        {:effect (effect (add-counter eid card :power 1 nil))
                         :async true
                         :msg "place 1 power counter on itself"}}}]})

(defcard "Lady Liberty"
  {:abilities [{:action true
                :cost [(->c :click 3)]
                :keep-menu-open :while-3-clicks-left
                :label "Add agenda from HQ to score area"
                :req (req (let [counters (get-counters (get-card state card) :power)]
                            (some #(and (agenda? %)
                                        (= counters (:agendapoints %)))
                                  (:hand corp))))
                :waiting-prompt true
                :prompt "Choose an Agenda in HQ to add to score area"
                :choices {:req (req (and (agenda? target)
                                         (= (:agendapoints target) (get-counters (get-card state card) :power))
                                         (in-hand? target)))}
                :msg (msg "add " (:title target) " to score area")
                :effect (req (let [c (move state :corp target :scored)]
                               (card-init state :corp c {:resolve-effect false
                                                         :init-data true}))
                             (update-all-advancement-requirements state)
                             (update-all-agenda-points state)
                             (check-win-by-agenda state side))}]
   :events [{:event :corp-turn-begins
             :automatic :last ;;so it goes after warm reception
             :async true
             :effect (effect (add-counter eid card :power 1 nil))}]})

(defcard "Lakshmi Smartfabrics"
  {:events [{:event :rez
             :async true
             :silent (req true)
             :effect (req (add-counter state side eid card :power 1))}]
   :abilities [{:req (req (seq (filter #(and (agenda? %)
                                             (>= (get-counters card :power)
                                                 (:agendapoints %)))
                                       (:hand corp))))
                :label "Reveal an agenda worth X points from HQ"
                :async true
                :cost [(->c :x-power)]
                :keep-menu-open :while-power-tokens-left
                :effect (req (let [paid-amt (cost-value eid :x-power)]
                               (continue-ability
                                 state side
                                 {:prompt "Choose an agenda in HQ to reveal"
                                  :choices {:req (req (and (agenda? target)
                                                           (<= (:agendapoints target) paid-amt)))}
                                  :msg (msg "reveal " (:title target) " from HQ")
                                  :async true
                                  :effect (req (wait-for (reveal state side target)
                                                         (let [title (:title target)]
                                                           (register-turn-flag!
                                                             state side
                                                             card :can-steal
                                                             (fn [state _side card]
                                                               (if (= (:title card) title)
                                                                 ((constantly false)
                                                                  (toast state :runner "Cannot steal due to Lakshmi Smartfabrics." "warning"))
                                                                 true)))
                                                           (effect-completed state side eid))))}
                                 card nil)))}]})

(defcard "Launch Campaign"
  (campaign 6 2))

(defcard "Levy University"
  {:abilities [{:action true
                :prompt "Choose a piece of ice"
                :msg (msg "adds " (:title target) " to HQ")
                :choices (req (cancellable (filter ice? (:deck corp)) :sorted))
                :label "Search R&D for a piece of ice"
                :cost [(->c :click 1) (->c :credit 1)]
                :keep-menu-open :while-clicks-left
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(defcard "Lily Lockwell"
  {:on-rez (draw-abi 3)
   :abilities [{:action true
                :label "Search R&D for an operation"
                :prompt "Choose an operation to add to the top of R&D"
                :waiting-prompt true
                :cost [(->c :click 1) (->c :tag 1)]
                :msg (msg (if (= target "No action")
                            "shuffle R&D"
                            (str "reveal " (:title target) " from R&D and add it to the top of R&D")))
                :choices (req (conj (vec (sort-by :title (filter operation? (:deck corp)))) "No action"))
                :async true
                :effect (req (if (= target "No action")
                               (do (shuffle! state :corp :deck)
                                   (effect-completed state side eid))
                               (wait-for
                                 (reveal state side target)
                                 (shuffle! state :corp :deck)
                                 (move state :corp target :deck {:front true})
                                 (effect-completed state side eid))))}]})

(defcard "Long-Term Investment"
  {:derezzed-events [corp-rez-toast]
   :abilities [{:action true
                :label "Move any number of hosted credits to your credit pool"
                :req (req (>= (get-counters card :credit) 8))
                :cost [(->c :click 1)]
                :prompt "How many hosted credits do you want to take?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :async true
                :effect (req
                          (play-sfx state :corp "click-credit-3")
                          (gain-credits state side eid target))}]
   :events [{:event :corp-turn-begins
             :msg "place 2 [Credit] on itself"
             :async true
             :effect (req (add-counter state side eid card :credit 2))}]})

(defcard "Lt. Todachine"
  {:events [{:event :rez
             :req (req (ice? (:card context)))
             :async true
             :msg "give the Runner 1 tag"
             :effect (req (gain-tags state :runner eid 1))}]})

(defcard "Lt. Todachine 2"
  {:events [{:event :rez
             :req (req (ice? (:card context)))
             :async true
             :msg "give the Runner 1 tag"
             :effect (req (gain-tags state :runner eid 1))}
            {:event :breach-server
             :interactive (req true)
             :req (req tagged)
             :async true
             ;; the random-access check needs to be dereffed to work correctly
             :effect (req (continue-ability
                            state side
                            {:req (req
                                    (and tagged
                                         (> (:random-access-limit (num-cards-to-access state :runner target nil)) 1)
                                         (not (get-only-card-to-access state))))
                             :msg (msg "make the runner access 1 card fewer")
                             :effect (req (access-bonus state :runner target -1))}
                            card targets))}]})

(defcard "Malia Z0L0K4"
  (let [unmark
        (req (when-let [malia-target (get-in card [:special :malia-target])]
               (update! state side (assoc-in (get-card state card) [:special :malia-target] nil))
               (remove-icon state :runner card (get-card state malia-target)))
             (update-disabled-cards state)
             (trigger-event-sync state nil eid :disabled-cards-updated))]
    {:on-rez {:msg (msg "blank the text box of " (card-str state target))
              :choices {:card #(and (runner? %)
                                    (installed? %)
                                    (resource? %)
                                    (not (has-subtype? % "Virtual")))}
              :effect (req (add-icon state side card target "MZ" (faction-label card))
                           (update! state side (assoc-in (get-card state card) [:special :malia-target] target))
                           (update-disabled-cards state))}
     :leave-play unmark
     :move-zone unmark
     :static-abilities [{:type :disable-card
                         :req (req
                                (let [malia-target (get-in (get-card state card) [:special :malia-target])]
                                  (or (same-card? target malia-target)
                                      (and (same-card? (:host target) malia-target)
                                           (= (:title malia-target) "DJ Fenris")
                                           (= (:type target) "Fake-Identity")))))
                         :value true}]}))


(defcard "Marilyn Campaign"
  (let [ability {:once :per-turn
                 :interactive (req (>= 2 (get-counters card :credit)))
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain 2 [Credits] (start of turn)")
                 :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                 :async true
                 :automatic :gain-credits
                 :effect (req (wait-for
                                (take-credits state side card :credit 2)
                                (if (not (pos? (get-counters (get-card state card) :credit)))
                                  (trash state :corp eid card {:unpreventable true :cause-card card})
                                  (effect-completed state :corp eid))))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :data {:counter {:credit 8}}
     :prevention [{:prevents :trash
                   :type :event
                   :label "Shuffle Marilyn Campaign into R&D"
                   :max-uses 1
                   :ability {:msg "shuffle itself into R&D instead of moving it to Archives"
                             :req (req (some #(same-card? % card) (map :card (get-in @state  [:prevent :trash :remaining]))))
                             :effect (req (swap! state update-in [:prevent :trash :remaining] (fn [ctx] (mapv #(if (same-card? card (:card %)) (assoc % :destination :deck :shuffle-rd true) %) ctx))))}}]}))

(defcard "Mark Yale"
  {:events [{:event :agenda-counter-spent
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]
   :abilities [{:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (gain-credits eid 2))}
               {:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [(->c :any-agenda-counter)]
                :async true
                :effect (effect (gain-credits eid 2))}]})

(defcard "Marked Accounts"
  (let [ability (take-n-credits-start-of-turn 1)]
    {:abilities [ability
                 {:action true
                  :cost [(->c :click 1)]
                  :msg "store 3 [Credits]"
                  :async true
                  :effect (effect (add-counter eid card :credit 3 nil))}]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "MCA Austerity Policy"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :once :per-turn
                :msg "force the Runner to lose a [Click] next turn and place a power counter on itself"
                :async true
                :effect (req (register-events state side card
                                              [{:event :runner-turn-begins
                                                :unregister-once-resolved true
                                                :duration :until-runner-turn-begins
                                                :effect (effect (lose-clicks :runner 1))}])
                             (add-counter state side eid card :power 1 nil))}
               {:action true
                :cost [(->c :click 1) (->c :power 3) (->c :trash-can)]
                :msg "gain 4 [Click]"
                :effect (effect (gain-clicks 4))}]})

(defcard "Melange Mining Corp."
  {:abilities [(merge (gain-credits-ability 7)
                      {:action true
                       :cost [(->c :click 3)]
                       :keep-menu-open :while-3-clicks-left})]})

(defcard "Mental Health Clinic"
  (assoc (creds-on-round-start 1) :static-abilities [(runner-hand-size+ 1)]))

(defcard "Moon Pool"
  (letfn [(moon-pool-place-advancements [x]
            {:async true
             :prompt (msg "Choose an installed card to place advancement counters on (" x " remaining)")
             :choices {:card #(installed? %)}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (wait-for (add-prop state side target :advance-counter 1 {:placed true})
                                    (if (> x 1)
                                      (continue-ability
                                        state side
                                        (moon-pool-place-advancements (dec x))
                                        card nil)
                                      (effect-completed state side eid))))
             :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to place advancement counters"))
                                   (effect-completed eid))})]
    (let [moon-pool-reveal-ability
          {:prompt "Choose up to 2 facedown cards from Archives to shuffle into R&D"
           :async true
           :show-discard true
           :choices {:card #(and (corp? %)
                                 (in-discard? %)
                                 (not (faceup? %)))
                     :max 2}
           :msg (msg "reveal " (enumerate-cards targets :sorted) " from Archives and shuffle them into R&D")
           :effect (req (wait-for (reveal state side targets)
                                  (doseq [c targets]
                                    (move state side c :deck))
                                  (shuffle! state side :deck)
                                  (let [agenda-count (count (filter agenda? targets))]
                                    (if (pos? agenda-count)
                                      (continue-ability
                                        state side
                                        (moon-pool-place-advancements agenda-count)
                                        source-card nil)
                                      (effect-completed state side eid)))))
           :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to reveal any cards in Archives"))
                                  (effect-completed eid))}
          moon-pool-discard-ability
          {:prompt "Choose up to 2 cards from HQ to trash"
           :choices {:card #(and (corp? %)
                                 (in-hand? %))
                     :max 2}
           :async true
           :msg (msg "trash " (quantify (count targets) "card") " from HQ")
           :effect (req (wait-for (trash-cards state :corp targets {:cause-card card})
                                  (continue-ability
                                    state side
                                    moon-pool-reveal-ability
                                    card nil)))
           :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to trash any cards from HQ"))
                                  (continue-ability moon-pool-reveal-ability card nil))}]
      {:abilities [{:label "Trash up to 2 cards from HQ. Shuffle up to 2 cards from Archives into R&D"
                    :cost [(->c :remove-from-game)]
                    :async true
                    :effect (effect (continue-ability
                                      moon-pool-discard-ability
                                      card nil))}]})))

(defcard "Mr. Stone"
  {:events [{:event :runner-gain-tag
             :async true
             :msg "do 1 meat damage"
             :effect (effect (damage :corp eid :meat 1 {:card card}))}]})

(defcard "Mumba Temple"
  {:recurring 2
   :interactions {:pay-credits {:req (req (= :rez (:source-type eid)))
                                :type :recurring}}})

(defcard "Mumbad City Hall"
  {:abilities [{:action true
                :label "Search R&D for an Alliance card"
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :prompt "Choose an Alliance card to play or install"
                :choices (req (cancellable (filter #(and (has-subtype? % "Alliance")
                                                         (if (operation? %)
                                                           (<= (:cost %) (:credit corp))
                                                           true))
                                                   (:deck corp))
                                           :sorted))
                :msg (msg "reveal " (:title target)
                          " from R&D and "
                          (if (= (:type target) "Operation") "play" "install")
                          " it")
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (shuffle! state side :deck)
                               (if (operation? target)
                                 (play-instant state side eid target nil)
                                 (corp-install state side eid target nil {:msg-keys {:install-source card
                                                                                     :known true
                                                                                     :display-origin true}}))))}]})

(defcard "Mumbad Construction Co."
  {:derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :silent (req true)
             :async true
             :effect (effect (add-prop eid card :advance-counter 1 {:placed true}))}]
   :abilities [{:cost [(->c :credit 2)]
                :keep-menu-open :while-advancement-tokens-left
                :req (req (and (pos? (get-counters card :advancement))
                               (not-empty (all-active-installed state :corp))))
                :label "Move an advancement token to a faceup card"
                :prompt "Choose a faceup card"
                :choices {:card faceup?}
                :msg (msg "move an advancement token to " (card-str state target))
                :async true
                :effect (req (wait-for (add-prop state side card :advance-counter -1 {:placed true})
                                       (add-prop state side eid target :advance-counter 1 {:placed true})))}]})

(defcard "Museum of History"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (pos? (count (get-in @state [:corp :discard]))))}
   :abilities [{:label "Shuffle cards in Archives into R&D"
                :prompt (msg (let [mus (count (filter #(and (= (:title card) (:title %))
                                                            (rezzed? %))
                                                      (all-installed state :corp)))]
                               (str "Choose " (quantify mus "card") " in Archives to shuffle into R&D")))
                :choices {:card #(and (corp? %)
                                      (in-discard? %))
                          :max (req (count (filter #(and (= (:title card) (:title %))
                                                         (rezzed? %))
                                                   (all-installed state :corp))))}
                :show-discard true
                :once :per-turn
                :once-key :museum-of-history
                :msg (msg "shuffle "
                          (let [seen (filter :seen targets)
                                n (count (filter #(not (:seen %)) targets))]
                            (str (enumerate-cards seen :sorted)
                                 (when (pos? n)
                                   (str (when-not (empty? seen) " and ")
                                        (quantify n "card")))))
                          " into R&D")
                :effect (req (doseq [c targets]
                               (move state side c :deck))
                             (shuffle! state side :deck))}]
   :implementation "[Erratum] Should be unique"})

(defcard "Nanoetching Matrix"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :once :per-turn
                :msg "gain 2 [Credits]"
                :async true
                :effect (effect (gain-credits eid 2))}]
   :on-trash {:optional
              {:req (req (= :runner side))
               :waiting-prompt true
               :prompt "Gain 2 [Credits]?"
               :yes-ability
               {:msg "gain 2 [Credits]"
                :async true
                :effect (effect (gain-credits :corp eid 2))}}}})

(defcard "NASX"
  (let [ability {:msg "gain 1 [Credits]"
                 :automatic :gain-credits
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:implementation "Manual - click NASX to place power counters on itself"
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability
                 {:label "Place 1 power counter"
                  :cost [(->c :credit 1)]
                  :msg "place 1 power counter on itself"
                  :async true
                  :effect (effect (add-counter eid card :power 1 nil))}
                 {:label "Place 2 power counters"
                  :cost [(->c :credit 2)]
                  :msg "place 2 power counters on itself"
                  :async true
                  :effect (effect (add-counter eid card :power 2 nil))}
                 {:action true
                  :label "Gain 2 [Credits] for each hosted power counter"
                  :cost [(->c :click 1) (->c :trash-can)]
                  :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                  :async true
                  :effect (effect (gain-credits eid (* 2 (get-counters card :power))))}]}))

(defcard "Net Analytics"
  (let [ability {:optional
                 {:autoresolve (get-autoresolve :auto-fire)
                  :waiting-prompt true
                  :player :corp
                  :prompt "Draw 1 card?"
                  :yes-ability (draw-abi 1)}}]
    {:events [(-> ability
                  (assoc :event :runner-lose-tag)
                  (assoc-in [:optional :req] (req (= (:side context) :runner))))
              (-> ability
                  (assoc :event :runner-prevent)
                  (assoc-in [:optional :req] (req (= :tag (:type context)))))]
     :abilities [(set-autoresolve :auto-fire "Net Analytics")]}))

(defcard "Net Police"
  {:x-fn (req (get-link state))
   :recurring (get-x-fn)
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "Neurostasis"
  (advance-ambush
    3
    {:req (req (pos? (get-counters (get-card state card) :advancement)))
     :waiting-prompt true
     :async true
     :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "installed card") " to shuffle into the stack")
     :choices {:card #(and (installed? %)
                           (runner? %))
               :max (req (get-counters (get-card state card) :advancement))}
     :msg (msg "shuffle " (enumerate-cards targets) " into the stack")
     :effect (req (doseq [c targets]
                    (move state :runner c :deck {:shuffled true}))
                  (shuffle! state :runner :deck)
                  (effect-completed state side eid))}))

(defcard "News Team"
  {:flags {:rd-reveal (req true)}
   :poison true
   :on-access {:async true
               :msg (msg "force the Runner to " (decapitalize target))
               :player :runner
               :prompt "Choose one"
               :waiting-prompt true
               :choices ["Take 2 tags" "Add News Team to score area"]
               :effect (req (if (= target "Take 2 tags")
                              (gain-tags state :runner eid 2)
                              (do (as-agenda state :runner card -1)
                                  (effect-completed state side eid))))}})

(defcard "NGO Front"
  (letfn [(builder [cost cred]
            {:cost [(->c :advancement cost) (->c :trash-can)]
             :async true
             :effect (effect (gain-credits eid cred))
             :label (str "Gain " cred " [Credits]")
             :msg (str "gain " cred " [Credits]")})]
    {:advanceable :always
     :abilities [(builder 1 5)
                 (builder 2 8)]}))

(defcard "Nico Campaign"
  (let [ability
        {:async true
         :interactive (req true)
         :once :per-turn
         :automatic :draw-cards
         :label "Take 3 [Credits] (start of turn)"
         :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
         :req (req (:corp-phase-12 @state))
         :effect (req (wait-for
                        (take-credits state side card :credit 3)
                        (if (pos? (get-counters (get-card state card) :credit))
                          (effect-completed state side eid)
                          (wait-for
                            (trash state :corp card {:unpreventable true :cause-card card})
                            (system-msg state :corp (str "trashes Nico Campaign"
                                                         (when (seq (:deck corp))
                                                           " and draws 1 card")))
                            (draw state :corp eid 1)))))}]
    {:data {:counter {:credit 9}}
     :derezzed-events [corp-rez-toast]
     :abilities [ability]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "Nightmare Archive"
  {:flags {:rd-reveal (req true)}
   :poison true
   :on-access {:async true
               :msg (msg (if (= target "Suffer 1 core damage")
                           "do 1 core damage"
                           (str "force the runner to " (decapitalize target))))
               :player :runner
               :prompt "Choose one"
               :choices ["Suffer 1 core damage" "Add Nightmare Archive to score area"]
               :effect (req (if (= target "Suffer 1 core damage")
                              (do (move state :corp card :rfg)
                                  (damage state :corp eid :brain 1 {:card card}))
                              (do (as-agenda state :runner card -1)
                                  (effect-completed state side eid))))}})

(defcard "Open Forum"
  {:events [{:event :corp-mandatory-draw
             :interactive (req true)
             :msg (msg (if (-> corp :deck count pos?)
                         (str "reveal "
                              (-> corp :deck first :title)
                              " from the top of R&D and add it to HQ")
                         "reveal no cards from R&D (it is empty)"))
             :async true
             :effect (req (wait-for
                            (reveal state side (-> corp :deck first))
                            (move state :corp (-> corp :deck first) :hand)
                            (continue-ability
                              state side
                              {:prompt "Choose a card in HQ to add to the top of R&D"
                               :async true
                               :choices {:card #(and (in-hand? %)
                                                     (corp? %))}
                               :msg "add 1 card from HQ to the top of R&D"
                               :effect (effect (move target :deck {:front true})
                                               (effect-completed eid))}
                              card nil)))}]})

(defcard "Otto Campaign"
  (let [ability {:once :per-turn
                 :interactive (req (>= 2 (get-counters card :credit)))
                 :event :corp-turn-begins
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain 2 [Credits] (start of turn)")
                 :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                 :async true
                 :automatic :gain-credits
                 :effect (req (wait-for
                                (take-credits state side card :credit 2)
                                (if (not (pos? (get-counters (get-card state card) :credit)))
                                  (continue-ability
                                    state side
                                    {:msg "trash itself and gain [click][click]"
                                     :async true
                                     :effect (req (wait-for
                                                    (trash state side card {:source-card card})
                                                    (gain-clicks state side 2)
                                                    (effect-completed state side eid)))}
                                    card nil)
                                  (effect-completed state side eid))))}]
    {:data {:counter {:credit 6}}
     :events [ability]
     :derezzed-events [corp-rez-toast]
     :abilities [ability]}))

(defcard "PAD Campaign"
  (creds-on-round-start 1))

(defcard "PAD Factory"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :label "Place 1 advancement token on a card"
                :choices {:card installed?}
                :msg (msg "place 1 advancement token on " (card-str state target))
                :async true
                :effect (req (wait-for
                               (add-prop state :corp target :advance-counter 1 {:placed true})
                               (let [tgtcid (:cid target)]
                                 (register-turn-flag!
                                   state side
                                   target :can-score
                                   (fn [state _ card]
                                     (if (and (= tgtcid
                                                 (:cid card))
                                              (<= (get-advancement-requirement card)
                                                  (get-counters card :advancement)))
                                       ((constantly false)
                                        (toast state :corp "Cannot score due to PAD Factory." "warning"))
                                       true))))
                               (effect-completed state side eid)))}]})

(defcard "Pālanā Agroplex"
  (let [ability {:msg "make each player draw 1 card"
                 :label "Make each player draw 1 card (start of turn)"
                 :once :per-turn
                 :automatic :draw-cards
                 :async true
                 :effect (req (wait-for (draw state :corp 1)
                                        (draw state :runner eid 1)))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Personalized Portal"
  {:special {:auto-fire :always}
   :abilities [(set-autoresolve :auto-fire "Personalized Portal (gain credits)")]
   :events [{:event :corp-turn-begins
             :interactive (req true)
             :async true
             :msg "force the runner to draw 1 card"
             :effect (req (wait-for
                            (draw state :runner 1)
                            (let [creds-to-gain (quot (count (get-in @state [:runner :hand])) 2)]
                              (continue-ability
                                state side
                                {:optional {:prompt (str "Gain " creds-to-gain " [Credits]?")
                                            :autoresolve (get-autoresolve :auto-fire)
                                            :req (req (pos? creds-to-gain))
                                            :waiting-prompt true
                                            :yes-ability {:msg (str "gain " creds-to-gain " [Credits]")
                                                          :async true
                                                          :effect (req (gain-credits state side eid creds-to-gain))}}}
                                card nil))))}]})

(defcard "Phật Gioan Baotixita"
  (let [place {:silent (req true)
               :async true
               :effect (req (add-counter state side eid card :power 1 {:placed true}))}
        opt (fn [x]
              (merge
                {:option (str "Do " x " net damage")
                 :ability {:msg (str "do " x " net damage")
                           :async true
                           :effect (req (damage state :corp eid :net x))}}
                (if (= x 1)
                  {}
                  {:cost [(->c :power (dec x))]})))
        abi (choose-one-helper
              {:req (req (and (or (and (first-event? state :corp :agenda-scored)
                                       (no-event? state :runner :agenda-stolen))
                                  (and (first-event? state :runner :agenda-stolen)
                                       (no-event? state :corp :agenda-scored)))))
               :player :corp
               :side :corp
               :interactive (req true)}
              (vec (map opt [1 2 3])))]
    {:events [(assoc abi :event :agenda-scored)
              (assoc place :event :corp-turn-ends)
              (assoc abi :event :agenda-stolen)]}))

(defcard "Plan B"
  (advance-ambush
    0
    {:req (req (pos? (get-counters (get-card state card) :advancement)))
     :waiting-prompt true
     :prompt "Choose an Agenda in HQ to score"
     :choices {:req (req (and (agenda? target)
                              (<= (get-advancement-requirement target)
                                  (get-counters (get-card state card) :advancement))
                              (in-hand? target)))}
     :msg (msg "score " (:title target))
     :async true
     :effect (effect (score eid target {:no-req true :ignore-turn true}))}))

(defcard "Plutus"
  (let [abi {:once :per-turn
             :label "Play a transaction from Archives?"
             :prompt "Play a transaction from Archives?"
             :show-discard true
             :change-in-game-state
             {:silent (req true)
              :req (req (some #(or (not (:seen %))
                                   (and (operation? %)
                                        (has-subtype? % "Transaction")
                                        (can-play-instant? state side eid % nil)))
                              (:discard corp)))}
             :choices {:req (req (and (operation? target)
                                      (has-subtype? target "Transaction")
                                      (can-play-instant? state side eid target nil)))}
             :async true
             :msg (msg "play " (:title target) " from Archives")
             :effect (req (play-instant
                            state side
                            (assoc eid :source target :source-type :play :source-info {:ability-targets [target]})
                            (assoc-in
                              (assoc target :rfg-instead-of-trashing true)
                              [:special :rfg-when-trashed] true)
                            nil))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc abi :event :corp-turn-begins)]
     :abilities [abi]
     :additional-cost [(->c :forfeit-or-trash-x-from-hand 3)]}))

(defcard "Political Dealings"
  (letfn [(pdhelper [agendas]
            (when-let [agenda (first agendas)]
              {:optional
               {:prompt (msg "Reveal and install " (:title agenda) "?")
                :yes-ability {:msg (msg "reveal they drew " (:title agenda))
                              :async true
                              :effect (req (wait-for
                                             (reveal state side agenda)
                                             (wait-for
                                               (corp-install
                                                 state side agenda nil
                                                 {:install-state (:install-state (card-def agenda) :unrezzed)
                                                  :msg-keys {:install-source card
                                                             :known true
                                                             :display-origin true}})
                                               (remove-from-currently-drawing state side agenda)
                                               (continue-ability state side (pdhelper (next agendas)) card nil))))}
                :no-ability {:async true
                             :effect (effect (continue-ability (pdhelper (next agendas)) card nil))}}}))]
    {:events [{:event :corp-draw
               :async true
               :effect (req (cond
                              ;; if agendas were drawn, do the full routine
                              (some agenda? corp-currently-drawing)
                              (let [agendas (filter #(and (agenda? %)
                                                          (get-card state %))
                                                    corp-currently-drawing)]
                                (continue-ability state side (pdhelper agendas) card nil))
                              ;; else show a fake prompt so the runner can't infer that agendas weren't drawn
                              :else
                              (continue-ability
                                state :corp
                                {:prompt "You did not draw any agenda"
                                 :choices ["Carry on!"]
                                 :prompt-type :bogus}
                                card nil)))}]}))

(defcard "Prāna Condenser"
  {:prevention [{:prevents :damage
                 :type :event
                 :max-uses 1
                 :ability {:async true
                           :msg "prevent 1 net damage, place 1 counter on itself, and gain 3 [Credits]"
                           :req (req (and (= :net (:type context))
                                          (= :corp (:source-player context))
                                          (preventable? context)))
                           :effect (req (wait-for (prevent-damage state side 1)
                                                  (wait-for (add-counter state side card :power 1 {:suppress-checkpoint true})
                                                            (gain-credits state side eid 3))))}}]
   :abilities [{:action true
                :msg (msg "deal " (get-counters card :power) " net damage")
                :label "deal net damage"
                :cost [(->c :click 2) (->c :trash-can)]
                :async true
                :effect (effect (damage eid :net (get-counters card :power) {:card card}))}]})

(defcard "Primary Transmission Dish"
  {:recurring 3
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "Private Contracts"
  {:data {:counter {:credit 14}}
   :events [(trash-on-empty :credit)]
   :abilities [(take-n-credits-ability
                 2 "asset"
                 {:action true
                  :keep-menu-open :while-clicks-left
                  :cost [(->c :click 1)]})]})

(defcard "Project Junebug"
  (advance-ambush 1 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :waiting-prompt true
                     :msg (msg "do " (* 2 (get-counters (get-card state card) :advancement)) " net damage")
                     :async true
                     :effect (effect (damage eid :net (* 2 (get-counters (get-card state card) :advancement))
                                             {:card card}))}))

(defcard "Psychic Field"
  (let [ab {:async true
            :req (req installed)
            :effect (req (let [hand (count (:hand runner))
                               message (str "do " hand " net damage")]
                           (continue-ability
                             state side
                             {:psi {:not-equal
                                    {:msg message
                                     :async true
                                     :effect (effect (damage eid :net hand {:card card}))}}}
                             card nil)))}]
    {:on-expose ab
     :on-access ab}))

(defcard "Public Access Plaza"
  (assoc (creds-on-round-start 1)
         :on-trash (assoc (give-tags 1) :req (req (and (= :runner side) (threat-level 2 state))))))

(defcard "Public Health Portal"
  (let [ability {:once :per-turn
                 :label "Reveal the top card of R&D and gain 2 [Credits] (start of turn)"
                 :interactive (req true)
                 :automatic :gain-credits
                 :msg (msg "reveal " (:title (first (:deck corp)))
                           " from the top of R&D"
                           " and gain 2 [Credits]")
                 :async true
                 :effect (req (wait-for
                                (reveal state side (first (:deck corp)))
                                (gain-credits state side eid 2)))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Public Support"
  {:data {:counter {:power 3}}
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :req (req (pos? (get-counters card :power)))
             :async true
             :effect (effect (add-counter eid card :power -1 nil))}
            {:event :counter-added
             :req (req (same-card? card (:card context))
                       (not (pos? (get-counters card :power))))
             :msg "add itself to [their] score area as an agenda worth 1 agenda point"
             :effect (effect (as-agenda card 1))}]})

(defcard "Quarantine System"
  (letfn [(rez-ice [cnt discount]
            {:prompt (str "Choose a piece of ice to rez, paying " discount " [Credits] less")
             :async true
             :choices {:req (req (and (ice? target)
                                      (can-pay-to-rez? state side (assoc eid :source card)
                                                       target {:cost-bonus (- discount)})
                                      (not (rezzed? target))))}
             :waiting-prompt true
             :effect (req (wait-for (rez state side target {:no-warning true :cost-bonus (- discount)})
                                    (if (< cnt 3)
                                      (continue-ability
                                        state side
                                        (rez-ice (inc cnt) discount)
                                        card nil)
                                      (effect-completed state side eid))))})]
    {:abilities [{:label "Forfeit agenda to rez up to 3 pieces of ice with a 2 [Credit] discount per agenda point"
                  :req (req (pos? (count (:scored corp))))
                  :cost [(->c :forfeit)]
                  :async true
                  :effect (req (continue-ability
                                 state side
                                 (rez-ice 1 (* 2 (:agendapoints (last (:rfg corp)) 0)))
                                 card nil))}]}))

(defcard "Raman Rai"
  {:events [{:event :corp-draw
             :optional
             {:prompt "Swap two cards?"
              :req (req (and (pos? (:click corp))
                             ;; don't prompt unless there's at least 1 card which type matches one among cards in Archives
                             (not-empty (set/intersection
                                          (into #{} (map :type (:discard corp)))
                                          (into #{} (map :type corp-currently-drawing))))
                             (not-empty (turn-events state side :corp-draw))))
              :yes-ability
              {:once :per-turn
               :async true
               :effect
               (effect
                 (lose-clicks :corp 1)
                 (continue-ability
                   {:prompt "Choose a card in HQ that you just drew to swap for a card of the same type in Archives"
                    :choices {:card #(some (fn [c] (same-card? c %)) corp-currently-drawing)}
                    :async true
                    :effect
                    (effect
                      (continue-ability
                        (let [set-aside-card target
                              t (:type set-aside-card)]
                          {:show-discard true
                           :prompt (msg "Choose an " t " in Archives to reveal and swap into HQ for " (:title set-aside-card))
                           :choices {:card #(and (corp? %)
                                                 (= (:type %) t)
                                                 (in-discard? %))}
                           :msg (msg "lose [Click], reveal " (:title set-aside-card)
                                     " from HQ, and swap it for " (:title target)
                                     " from Archives")
                           :async true
                           :effect (req (wait-for
                                          (reveal state side set-aside-card target)
                                          (swap-set-aside-cards state side set-aside-card target)
                                          (effect-completed state side eid)))})
                        card nil))}
                   card nil))}}}]})

(defcard "Rashida Jaheem"
  (let [ability {:once :per-turn
                 :skippable true
                 :async true
                 :label "Gain 3 [Credits] and draw 3 cards (start of turn)"
                 :req (req (:corp-phase-12 @state))
                 :effect
                 (effect
                   (continue-ability
                     {:optional
                      {:prompt "Trash this asset to gain 3 [Credits] and draw 3 cards?"
                       :yes-ability
                       {:async true
                        :msg "gain 3 [Credits] and draw 3 cards"
                        :effect (req (wait-for
                                       (trash state side card {:cause-card card})
                                       (swap! state update-in [:stats side :rashida-count] (fnil + 0) 1)
                                       (wait-for
                                         (gain-credits state side 3)
                                         (draw state side eid 3))))}}}
                     card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Reality Threedee"
  (let [ability {:effect (effect (gain-credits eid (if tagged 2 1)))
                 :async true
                 :label "Gain credits (start of turn)"
                 :automatic :gain-credits
                 :once :per-turn
                 :msg (msg (if tagged "gain 2 [Credits]" "gain 1 [Credits]"))}]
    {:on-rez {:msg "take 1 bad publicity"
              :effect (effect (gain-bad-publicity :corp 1))}
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Reaper Function"
  (let [ability {:async true
                 :once :per-turn
                 :label "Trash this asset to do 2 net damage (start of turn)"
                 :automatic :corp-damage
                 :interactive (req true)
                 :req (req (:corp-phase-12 @state))
                 :effect
                 (effect
                  (continue-ability
                    {:optional
                     {:prompt "Trash Reaper Function to do 2 net damage?"
                      :yes-ability
                      {:msg "do 2 net damage"
                       :async true
                       :effect (req (wait-for (trash state side card {:cause-card card})
                                              (damage state side eid :net 2 {:card card})))}}}
                    card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Reconstruction Contract"
  {:events [{:event :damage
             :req (req (and (pos? (:amount context))
                            (= :meat (:damage-type context))))
             :msg "place 1 advancement token on itself"
             :async true
             :effect (effect (add-counter eid card :advancement 1 {:placed true}))}]
   :abilities [{:label "Move hosted advancement tokens to another card"
                :cost [(->c :trash-can)]
                :async true
                :prompt "How many hosted advancement tokens do you want to move?"
                :choices {:number (req (get-counters card :advancement))
                          :default (req (get-counters card :advancement))}
                :effect (req (let [num-counters target]
                               (continue-ability
                                 state side
                                 {:async true
                                  :prompt "Choose a card that can be advanced"
                                  :choices {:req (req (can-be-advanced? state target))}
                                  :effect (effect (system-msg (str "uses " (:title card) " to move " (quantify num-counters "hosted advancement token") " to " (card-str state target)))
                                                  (add-counter eid target :advancement num-counters {:placed true}))}
                                 card nil)))}]})

(defcard "Refuge Campaign"
  (creds-on-round-start 2))

(defcard "Regolith Mining License"
  {:data {:counter {:credit 15}}
   :events [(trash-on-empty :credit)]
   :abilities [(take-n-credits-ability
                 3 "asset"
                 {:action true
                  :keep-menu-open :while-clicks-left
                  :cost [(->c :click 1)]})]})

(defcard "Reversed Accounts"
  {:advanceable :always
   :abilities [{:action true
                :cost [(->c :click 1) (->c :trash-can)]
                :label "Force the Runner to lose 4 [Credits] per advancement"
                :msg (msg "force the Runner to lose " (min (* 4 (get-counters card :advancement)) (:credit runner)) " [Credits]")
                :async true
                :effect (effect (lose-credits :runner eid (* 4 (get-counters card :advancement))))}]})

(defcard "Rex Campaign"
  (let [payout-ab {:prompt "Choose one"
                   :waiting-prompt true
                   :choices ["Remove 1 bad publicity" "Gain 5 [Credits]"]
                   :msg (msg (decapitalize target))
                   :async true
                   :effect (req (if (= target "Remove 1 bad publicity")
                                  (lose-bad-publicity state side eid 1)
                                  (gain-credits state side eid 5)))}
        ability {:once :per-turn
                 :req (req (:corp-phase-12 @state))
                 :label "Remove 1 counter (start of turn)"
                 :async true
                 :effect (req (wait-for (add-counter state side card :power -1 nil)
                                        (if (zero? (get-counters (get-card state card) :power))
                                          (wait-for (trash state side card {:cause-card card})
                                                    (continue-ability state side payout-ab card nil))
                                          (effect-completed state side eid))))}]
    {:data {:counter {:power 3}}
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :ability [ability]}))

(defcard "Ronald Five"
  (let [ability {:event :runner-trash
                 :once-per-instance false
                 :req (req (and (corp? (:card target))
                                (pos? (:click runner))))
                 :msg "force the runner to lose [Click]"
                 :effect (effect (lose-clicks :runner 1))}]
    {:events [ability]
     :on-trash ability}))

(defcard "Ronin"
  {:advanceable :always
   :abilities [(assoc (do-net-damage 3)
                      :action true
                      :cost [(->c :click 1) (->c :trash-can)]
                      :req (req (>= (get-counters card :advancement) 4)))]})

(defcard "Roughneck Repair Squad"
  {:abilities [{:action true
                :label "Gain 6 [Credits], may remove 1 bad publicity"
                :cost [(->c :click 3)]
                :keep-menu-open :while-3-clicks-left
                :msg "gain 6 [Credits]"
                :async true
                :effect (req (wait-for
                               (gain-credits state side 6)
                               (continue-ability
                                 state side
                                 {:optional
                                  {:req (req (pos? (count-bad-pub state)))
                                   :prompt "Remove 1 bad publicity?"
                                   :yes-ability
                                   {:msg "remove 1 bad publicity"
                                    :effect (effect (lose-bad-publicity 1))}}}
                                 card nil)))}]})

(defcard "Sandburg"
  {:on-rez {:effect (effect (update-all-ice))}
   :static-abilities [{:type :ice-strength
                       :req (req (<= 10 (:credit corp)))
                       :value (req (quot (:credit corp) 5))}]
   :events [{:event :corp-gain
             :req (req (= :credit (:type context)))
             :effect (effect (update-all-ice))}
            {:event :corp-lose
             :req (req (= :credit (:type context)))
             :effect (effect (update-all-ice))}]
   :leave-play (effect (update-all-ice))})

(defcard "Sealed Vault"
  {:abilities [{:label "Store any number of credits"
                :cost [(->c :credit 1)]
                :prompt "How many credits do you want to move?"
                :choices {:number (req (- (:credit corp) 1))}
                :msg (msg "store " target " [Credits]")
                :async true
                :effect (req (wait-for (add-counter state side card :credit target)
                                       (lose-credits state side eid target)))}
               {:action true
                :label "Move any number of credits to your credit pool"
                :cost [(->c :click 1)]
                :prompt "How many credits do you want to move?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :async true
                :effect (effect (gain-credits eid target))}
               {:label "Move any number of credits to your credit pool"
                :prompt "How many credits do you want to move?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (gain-credits eid target))}]})

(defcard "Security Subcontract"
  {:abilities [(merge (gain-credits-ability 4)
                      {:action true
                       :cost [(->c :click 1) (->c :ice 1)]
                       :keep-menu-open :while-clicks-left})]})

(defcard "Sensie Actors Union"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req unprotected)}
   :abilities [{:label "Draw 3 cards and add 1 card in HQ to the bottom of R&D"
                :once :per-turn
                :msg "draw 3 cards"
                :async true
                :effect (req (wait-for (draw state side 3)
                                       (continue-ability
                                         state side
                                         {:prompt "Choose a card in HQ to add to the bottom of R&D"
                                          :choices {:card #(and (corp? %)
                                                                (in-hand? %))}
                                          :msg "add 1 card from HQ to the bottom of R&D"
                                          :effect (effect (move target :deck))}
                                         card nil)))}]})

(defcard "Server Diagnostics"
  (let [ability {:effect (effect (gain-credits eid 2))
                 :async true
                 :once :per-turn
                 :automatic :gain-credits
                 :label "Gain 2 [Credits] (start of turn)"
                 :msg "gain 2 [Credits]"}]
    {:derezzed-events [corp-rez-toast]
     :abilities [ability]
     :events [(assoc ability :event :corp-turn-begins)
              {:event :corp-install
               :req (req (ice? (:card context)))
               :async true
               :msg "trash itself"
               :effect (effect (trash eid card {:cause-card card}))}]}))

(defcard "Shannon Claire"
  {:abilities [{:action true
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :msg "draw 1 card from the bottom of R&D"
                :effect (effect (play-sfx "click-card")
                                (move (last (:deck corp)) :hand))}
               {:label "Search R&D for an agenda"
                :prompt "Choose an agenda to add to the bottom of R&D"
                :msg (msg "reveal " (:title target) " from R&D and add it to the bottom of R&D")
                :choices (req (cancellable (filter agenda? (:deck corp)) :sorted))
                :cost [(->c :trash-can)]
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (shuffle! state side :deck)
                               (move state side target :deck)
                               (effect-completed state side eid)))}
               {:label "Search Archives for an agenda"
                :prompt "Choose an agenda to add to the bottom of R&D"
                :msg (msg "reveal " (:title target) " from Archives and add it to the bottom of R&D")
                :choices (req (cancellable (filter agenda? (:discard corp)) :sorted))
                :cost [(->c :trash-can)]
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (move state side target :deck)
                               (effect-completed state side eid)))}]})

(defcard "Shattered Remains"
  (advance-ambush 1 {:async true
                     :waiting-prompt true
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "piece") " of hardware to trash")
                     :msg (msg "trash " (enumerate-cards targets))
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card #(and (installed? %)
                                           (hardware? %))}
                     :effect (effect (trash-cards eid targets {:cause-card card}))}))

(defcard "Shi.Kyū"
  {:poison true
   :on-access
   {:optional
    {:req (req (not (in-deck? card)))
     :waiting-prompt true
     :prompt (msg "Pay credits to use " (:title card) " ability?")
     :yes-ability
     {:prompt "How many credits do you want to pay?"
      :choices :credit
      :msg (msg "attempt to do " target " net damage")
      :async true
      :effect (effect
                (continue-ability
                  (let [dmg target]
                    {:player :runner
                     :prompt "Choose one"
                     :waiting-prompt true
                     :choices [(str "Take " dmg " net damage") "Add Shi.Kyū to score area"]
                     :async true
                     :effect (req (if (str/starts-with? target "Add")
                                    (do (system-msg state :runner (str "adds " (:title card)
                                                                       " to [their] score area as an agenda worth "
                                                                       (quantify -1 "agenda point")))
                                        (as-agenda state :runner card -1)
                                        (effect-completed state side eid))
                                    (do (system-msg state :runner (str "takes " dmg " net damage from " (:title card)))
                                        (damage state :corp eid :net dmg {:card card}))))})
                  card targets))}}}})

(defcard "Shock!"
  {:flags {:rd-reveal (req true)}
   :poison true
   :on-access {:msg "do 1 net damage"
               :async true
               :effect (effect (damage eid :net 1 {:card card}))}})

(defcard "SIU"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Trace 3 - Give the Runner 1 tag"
                :req (req (:corp-phase-12 @state))
                :async true
                :cost [(->c :trash-can)]
                :effect (effect (continue-ability
                                  {:trace {:base 3
                                           :label "Trace 3 - Give the Runner 1 tag"
                                           :successful (give-tags 1)}}
                                  card nil))}]})

(defcard "Snare!"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:req (req (and (not (in-discard? card))
                               (can-pay? state :corp eid card nil [(->c :credit 4)])))
                :waiting-prompt true
                :prompt (msg "Pay 4 [Credits] to use " (:title card) " ability?")
                :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}
                :yes-ability {:async true
                              :cost [(->c :credit 4)]
                              :msg "give the Runner 1 tag and do 3 net damage"
                              :effect (req (wait-for (gain-tags state :corp 1 {:suppress-checkpoint true})
                                                     (damage state side eid :net 3 {:card card})))}}}})

(defcard "Space Camp"
  {:flags {:rd-reveal (req true)}
   :poison true
   :on-access {:optional
               {:waiting-prompt true
                :prompt "Place 1 advancement token on a card that can be advanced?"
                :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                              :prompt "Choose a card to place an advancement token on"
                              :choices {:req (req (can-be-advanced? state target))}
                              :async true
                              :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}}}})

(defcard "Spin Doctor"
  {:on-rez {:async true
            :msg "draw 2 cards"
            :effect (effect (draw eid 2))}
   :abilities [{:label "Shuffle up to 2 cards from Archives into R&D"
                :cost [(->c :remove-from-game)]
                :async true
                :effect (effect (shuffle-into-rd-effect eid source-card 2))}]})

(defcard "Storgotic Resonator"
  {:abilities [{:action true
                :cost [(->c :click 1) (->c :power 1)]
                :keep-menu-open :while-power-tokens-left
                :label "Do 1 net damage"
                :msg "do 1 net damage"
                :async true
                :effect (effect (damage eid :net 1 {:card card}))}]
   :events [{:event :corp-trash
             :once-per-instance true
             :req (req (and (some #(= (:faction (:identity runner)) (:faction (:card %))) targets)
                            (first-event?
                              state side :corp-trash
                              (fn [targets]
                                (some #(= (:faction (:identity runner)) (:faction (:card %))) targets)))))
             :msg "place 1 power counter on itself"
             :async true
             :effect (effect (add-counter eid card :power 1 nil))}]})

(defcard "Student Loans"
  {:static-abilities [{:type :play-additional-cost
                       :req (req (and (event? target)
                                      (seq (filter #(= (:title %) (:title target)) (:discard runner)))))
                       :value [(->c :credit 2)]}]})

(defcard "Superdeep Borehole"
  ;; the "when it is empty" text is reliant on the card being loaded
  {:on-rez {:async true
            :effect (req (update! state side (assoc-in (get-card state card) [:special :borehole-valid] true))
                         (add-counter state side eid card :bad-publicity 6 nil))}
   :events [{:event :corp-turn-begins
             :msg (msg "take 1 bad publicity from " (:title card))
             :async true
             :effect (req (wait-for (add-counter state side card :bad-publicity -1 nil)
                                    (gain-bad-publicity state :corp eid 1)))}
            {:event :counter-added
             :req (req (and (same-card? card (:card context))
                            (not (pos? (get-counters (get-card state card) :bad-publicity)))
                            (:borehole-valid (:special card))))
             :msg "win the game"
             :effect (req (win state :corp (:title card)))}]})

(defcard "Sundew"
  {:events [{:event :runner-spent-click
             :req (req (first-event? state side :runner-spent-click))
             :msg "gain 2 [Credits]"
             :async true
             :effect (req (update! state side (assoc-in card [:special :spent-click] true))
                          (gain-credits state :corp eid 2))}
            {:event :run
             :req (req (and (first-event? state side :runner-spent-click)
                            run
                            this-server
                            (get-in card [:special :spent-click])))
             :msg "lose 2 [Credits]"
             :async true
             :effect (req (update! state side (dissoc-in card [:special :spent-click]))
                          (lose-credits state :corp eid 2))}]})

(defcard "Svyatogor Excavator"
  (let [ability {:async true
                 :label "trash a card to gain 3 [Credits]"
                 :once :per-turn
                 :req (req (>= (count (all-installed state :corp)) 2))
                 :choices {:not-self true
                           :req (req (and (corp? target)
                                          (installed? target)))}
                 :msg (msg "trash " (card-str state target) " and gain 3 [Credits]")
                 :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                        (effect-completed eid))
                 :effect (req (wait-for (trash state side target {:unpreventable true
                                                                  :cause-card card})
                                        (gain-credits state side eid 3)))}]
    {:flags {:corp-phase-12 (req (>= (count (all-installed state :corp)) 2))}
     :events [(assoc ability
                     :event :corp-turn-begins
                     :interactive (req true))]
     :abilities [ability]}))

(defcard "Synth DNA Modification"
  {:events [{:event :subroutines-broken
             :req (req (and (has-subtype? (:ice context) "AP")
                            (first-event? state side :subroutines-broken
                                          #(has-subtype? (:ice (first %)) "AP"))))
             :msg "do 1 net damage"
             :async true
             :effect (effect (damage eid :net 1 {:card card}))}]})

(defcard "Team Sponsorship"
  {:events [{:event :agenda-scored
             :prompt "Choose a card from Archives or HQ to install"
             :show-discard true
             :interactive (req true)
             :async true
             :choices {:card #(and (not (operation? %))
                                   (corp? %)
                                   (or (in-hand? %)
                                       (in-discard? %)))}
             :effect (effect (corp-install eid target nil {:ignore-install-cost true
                                                           :msg-keys {:install-source card
                                                                      :display-origin true}}))}]})

(defcard "Tech Startup"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Search R&D for an asset to install"
                :prompt "Choose an asset"
                :msg (msg "reveal " (:title target) " from R&D and install it")
                :req (req (seq (filter asset? (:deck corp))))
                :choices (req (filter asset? (:deck corp)))
                :async true
                :effect (req (wait-for
                               (trash state side card {:cause-card card})
                               (wait-for
                                 (reveal state side target)
                                 (shuffle! state side :deck)
                                 (corp-install state side eid target nil {:msg-keys {:install-source card
                                                                                     :known true
                                                                                     :display-origin true}}))))}]})

(defcard "TechnoCo"
  (letfn [(is-techno-target [card]
            (or (program? card)
                (hardware? card)
                (and (resource? card)
                     (has-subtype? card "Virtual"))))]
    {:special {:auto-fire :always}
     :abilities [(set-autoresolve :auto-fire "TechnoCo")]
     :static-abilities [{:type :install-cost
                         :req (req (and (is-techno-target target)
                                        (not (:facedown (second targets)))))
                         :value 1}]
     :events [{:event :runner-install
               :optional {:req (req (and (is-techno-target (:card context))
                                         (not (:facedown context))))
                          :prompt "Gain 1 [Credit]?"
                          :waiting-prompt true
                          :autoresolve (get-autoresolve :auto-fire)
                          :yes-ability {:msg "gain 1 [Credits]"
                                        :async true
                                        :effect (effect (gain-credits :corp eid 1))}}}]}))

(defcard "Tenma Line"
  {:abilities [{:action true
                :label "Swap 2 installed pieces of ice"
                :cost [(->c :click)]
                :keep-menu-open :while-clicks-left
                :prompt "Choose 2 pieces of ice to swap positions"
                :req (req (<= 2 (count (filter ice? (all-installed state :corp)))))
                :choices {:card #(and (installed? %)
                                      (ice? %))
                          :max 2
                          :all true}
                :msg (msg "swap the positions of "
                          (card-str state (first targets))
                          " and "
                          (card-str state (second targets)))
                :effect (req (apply swap-installed state side targets))}]})

(defcard "Test Ground"
  {:advanceable :always
   :abilities [{:label "Derez 1 card for each advancement token"
                :req (req (pos? (get-counters card :advancement)))
                :cost [(->c :trash-can)]
                :async true
                :effect (req (let [cards-to-pick (min (count (filter #(and (rezzed? %) (not (agenda? %))) (all-installed state :corp)))
                                                      (get-counters card :advancement))
                                   payment-eid eid]
                               (continue-ability
                                 state side
                                 {:prompt (str "derez " cards-to-pick " cards")
                                  :waiting-prompt true
                                  :choices {:card (every-pred installed? rezzed? (complement agenda?))
                                            :max cards-to-pick
                                            :all true}
                                  :async true
                                  :effect (req (derez state side eid targets {:msg-keys {:include-cost-from-eid payment-eid}}))}
                                 card nil)))}]})

(defcard "The Board"
  {:on-trash executive-trash-effect
   :static-abilities [{:type :agenda-value
                       :req (req (= :runner (:scored-side target)))
                       :value -1}]})

(defcard "The News Now Hour"
  {:events [{:event :runner-turn-begins
             :silent (req true)
             :effect (req (prevent-current state side))}]
   :on-rez {:effect (req (prevent-current state side))}
   :leave-play (req (swap! state assoc-in [:runner :register :cannot-play-current] false))})

(defcard "The Powers That Be"
  {:events [{:event :agenda-scored
             :prompt "Choose a card from Archives or HQ to install, ignoring all costs"
             :show-discard true
             :interactive (req true)
             :async true
             :choices {:card #(and (corp-installable-type? %)
                                   (or (in-hand? %)
                                       (in-discard? %)))}
             :effect (effect (corp-install eid target nil {:ignore-install-cost true
                                                           :msg-keys {:install-source card
                                                                      :display-origin true}}))}]})

(defcard "The Root"
  {:recurring 3
   :interactions {:pay-credits {:req (req (or (#{:advance :corp-install :rez} (:source-type eid))
                                              (is-basic-advance-action? eid)))
                                :type :recurring}}})

(defcard "Thomas Haas"
  {:advanceable :always
   :abilities [{:label "Gain credits"
                :msg (msg "gain " (* 2 (get-counters card :advancement)) " [Credits]")
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (gain-credits eid (* 2 (get-counters card :advancement))))}]})

(defcard "Tiered Subscription"
  {:events [{:event :run
             :req (req (first-event? state side :run))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Toshiyuki Sakai"
  (advance-ambush
    0
    {:async true
     :waiting-prompt true
     :prompt "Choose an asset or agenda in HQ"
     :choices {:card #(and (or (agenda? %)
                               (asset? %))
                           (in-hand? %))}
     :msg "swap itself for an asset or agenda from HQ"
     :effect (req (let [counters (get-counters card :advancement)
                        [moved-card moved-target] (swap-cards state side card target)]
                    (set-prop state side moved-target :advance-counter counters)
                    (continue-ability
                      state :runner
                      {:optional
                       {:prompt "Access the newly installed card?"
                        :yes-ability {:async true
                                      :effect (effect (access-card eid (get-card state moved-target)))}}}
                      moved-card nil)))}))

(defcard "Trieste Model Bioroids"
  {:on-rez {:msg (msg "prevent " (card-str state target)
                      " from being broken by runner card abilities")
            :choices {:card #(and (ice? %)
                                  (rezzed? %)
                                  (has-subtype? % "Bioroid"))}
            :effect (effect (add-icon card target "TMB" (faction-label card))
                            (update! (assoc-in (get-card state card) [:special :trieste-target] target)))}
   :leave-play (effect (remove-icon card))
   :static-abilities [{:type :prevent-paid-ability
                       :req (req
                              (let [[break-card break-ability] targets]
                                (and
                                  (same-card? current-ice (get-in card [:special :trieste-target]))
                                  (runner? break-card)
                                  (or (not (identity? break-card))
                                      (fake-identity? break-card))
                                  (or (contains? break-ability :break)
                                      (contains? break-ability :breaks)
                                      (contains? break-ability :heap-breaker-break)
                                      (contains? break-ability :break-cost)))))
                       :value true}]})

(defcard "Trojan"
  {:flags {:rd-reveal (req true)}
   :poison true
   :on-access {:async true
               :req (req (not (in-discard? card)))
               :msg (msg "lose 2 [Credits], destroy itself, and trash 1 card from HQ at random")
               :effect (req (wait-for
                              (lose-credits state :corp 2)
                              (move state side card :destroyed)
                              (let [trash-target (first (shuffle (get-in @state [:corp :hand])))]
                                (if trash-target
                                  (trash state :corp eid trash-target {:cause-card card})
                                  (effect-completed state side eid)))))}})

(defcard "Turtlebacks"
  {:events [{:event :server-created
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Ubiquitous Vig"
  (let [ability {:msg (msg "gain " (get-counters card :advancement)  " [Credits]")
                 :label "Gain 1 [Credits] for each advancement counter (start of turn)"
                 :automatic :corp-gain-credits
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid (get-counters card :advancement)))}]
    {:derezzed-events [corp-rez-toast]
     :advanceable :always
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Urban Renewal"
  {:data {:counter {:power 3}}
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :automatic :corp-damage
             :async true
             :interactive (req true)
             :effect (req (wait-for
                            (add-counter state side card :power -1 nil)
                            (if (not (pos? (get-counters (get-card state card) :power)))
                              (wait-for (trash state side card {:cause-card card})
                                        (system-msg state :corp (str "uses " (:title card) " to do 4 meat damage"))
                                        (damage state side eid :meat 4 {:card card}))
                              (effect-completed state side eid))))}]})

(defcard "Urtica Cipher"
  (advance-ambush 0 {:msg (msg "do " (+ 2 (get-counters (get-card state card) :advancement)) " net damage")
                     :async true
                     :effect (effect (damage eid :net (+ 2 (get-counters (get-card state card) :advancement)) {:card card}))}))

(defcard "Vaporframe Fabricator"
  {:on-trash {:req (req (= :runner side))
              :async true
              :choices {:card #(and (corp? %)
                                    (in-hand? %)
                                    (not (operation? %)))}
              :effect
              (effect
                (continue-ability
                  (let [card-to-install target]
                    {:async true
                     :prompt "Choose a server"
                     :choices (req (remove (set (zone->name (get-zone card)))
                                           (installable-servers state card-to-install)))
                     :effect (effect (corp-install eid card-to-install target {:ignore-all-cost true
                                                                               :msg-keys {:install-source card
                                                                                          :display-origin true}}))})
                  card nil))}
   :abilities [{:action true
                :label "Install 1 card"
                :async true
                :cost [(->c :click 1)]
                :once :per-turn
                :choices {:card #(and (corp? %)
                                      (in-hand? %)
                                      (not (operation? %)))}
                :msg (msg (corp-install-msg target))
                :effect (effect (corp-install eid target nil {:ignore-all-cost true
                                                              :msg-keys {:install-source card
                                                                         :display-origin true}}))}]})

(defcard "Vera Ivanovna Shuyskaya"
  (let [ability {:interactive (req true)
                 :optional {:prompt "Reveal the grip and trash a card?"
                            :autoresolve (get-autoresolve :auto-fire)
                            :yes-ability (with-revealed-hand :runner {:event-side :corp}
                                           {:prompt "Choose a card to trash"
                                            :req (req (seq (:hand runner)))
                                            :choices {:card (every-pred in-hand? runner?)}
                                            :async true
                                            :msg (msg "trash " (:title target) " from the Grip")
                                            :effect (req (trash state side eid target {:cause-card card}))})
                            :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
    {:events [(assoc ability :event :agenda-scored)
              (assoc ability :event :agenda-stolen)]
     :abilities [(set-autoresolve :auto-fire "Vera Ivanovna Shuyskaya")]}))

(defcard "Victoria Jenkins"
  {:on-rez {:effect (req (lose state :runner :click-per-turn 1))}
   :leave-play (req (gain state :runner :click-per-turn 1))
   :on-trash executive-trash-effect})

(defcard "Wage Workers"
  (let [payoff {:msg "gain [Click]"
                :req (req (not (get-in @state [side :register :terminal])))
                :effect (effect (gain-clicks 1))}
        relevant-keys (fn [context] {:cid (get-in context [:card :cid])
                                     :idx (:ability-idx context)})]
    {:events [{:event :action-resolved
               :req (req (= :corp side))
               :async true
               :effect (req (let [similar-actions
                                  (event-count state side :action-resolved
                                               (fn [[ctx]] (= (relevant-keys ctx)
                                                              (relevant-keys context))))]
                              (continue-ability
                                state side
                                (when (= 3 similar-actions) payoff)
                                card nil)))}]}))

(defcard "Wall to Wall"
  (let [all [{:msg "gain 1 [Credits]"
              :async true
              :effect (effect (gain-credits eid 1))}
             {:msg "draw 1 card"
              :async true
              :effect (effect (draw eid 1))}
             {:label "place 1 advancement counter on a piece of ice"
              :msg (msg "place 1 advancement counter on " (card-str state target))
              :prompt "Choose a piece of ice to place 1 advancement counter on"
              :async true
              :choices {:card #(and (ice? %)
                                    (installed? %))}
              :cancel-effect (effect (effect-completed eid))
              :effect (effect (add-prop eid target :advance-counter 1 {:placed true}))}
             {:label "add this asset to HQ"
              :msg "add itself to HQ"
              :effect (effect (move card :hand))}]
        choice (fn choice [abis n]
                 (let [choices (concat (mapv make-label abis) ["Done"])]
                   {:prompt "Choose an ability to resolve"
                    :choices choices
                    :async true
                    :effect (req (let [chosen (first (filter #(= % target) choices))
                                       chosen-ability (first (filter #(= target (make-label %)) abis))]
                                   (wait-for (resolve-ability state side chosen-ability card nil)
                                     (if (and (pos? (dec n))
                                              (not= "Done" chosen))
                                       (continue-ability state side (choice (remove-once #(= % chosen-ability) abis) (dec n)) card nil)
                                       (effect-completed state side eid)))))}))
        ability {:async true
                 :automatic :last ;; so it can go after rashida
                 :interactive (req true)
                 :label "resolve an ability (start of turn)"
                 :once :per-turn
                 :effect (effect (continue-ability (choice all (if (< 1 (count (filter asset? (all-active-installed state :corp))))
                                                                 1
                                                                 3)) card nil))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Warden Fatuma"
  {:static-abilities [{:type :additional-subroutines
                       :req (req (and (ice? target) (rezzed? target) (has-subtype? target "Bioroid")))
                       :value {:position :front
                               :subroutines [{:label "[Warden Fatuma] Force the Runner to lose [Click], if able"
                                              :msg "force the Runner to lose [Click], if able"
                                              :effect (effect (lose-clicks :runner 1))}]}}]})

(defcard "Warm Reception"
  (let [install {:prompt "Choose a card to install"
                 :async true
                 :choices {:card #(and (corp-installable-type? %)
                                       (in-hand? %))}
                 :effect
                 (req (wait-for (corp-install state side (make-eid state eid) target nil {:msg-keys {:install-source card
                                                                                                     :display-origin true}})
                                (let [installed-card async-result]
                                  (register-turn-flag!
                                    state side
                                    card :can-score
                                    (fn [state _ card]
                                      (if (same-card? card installed-card)
                                        ((constantly false) (toast state :corp "Cannot score due to Warm Reception." "warning"))
                                        true)))
                                  (effect-completed state side eid))))}
        derez-abi {:label "Derez another card (start of turn)"
                   :req (req unprotected)
                   :prompt "Choose another card to derez"
                   :choices {:not-self true
                             :card #(rezzed? %)}
                   :async true
                   :effect (req (derez state side eid [card target]))}]
    {:derezzed-events [corp-rez-toast]
     :events [{:event :corp-turn-begins
               :interactive (req true)
               :async true
               :effect (req (wait-for (resolve-ability state side install card nil)
                                      (continue-ability state side derez-abi card nil)))}]}))

(defcard "Watchdog"
  (letfn [(not-triggered? [state]
            (no-event? state :runner :rez #(ice? (:card (first %)))))]
    {:static-abilities [{:type :rez-cost
                         :req (req (and (ice? target)
                                        (not-triggered? state)))
                         :value (req (- (count-tags state)))}]
     :events [{:event :rez
               :req (req (and (ice? (:card context))
                              (not-triggered? state)))
               :msg (msg "reduce the rez cost of " (:title (:card context))
                      " by " (count-tags state) " [Credits]")}]}))

(defcard "Whampoa Reclamation"
  {:abilities [{:label "Add 1 card from Archives to the bottom of R&D"
                :once :per-turn
                :req (req (and (pos? (count (:hand corp)))
                               (pos? (count (:discard corp)))))
                :async true
                :cost [(->c :trash-from-hand 1)]
                :effect (effect (continue-ability
                                  {:waiting-prompt true
                                   :prompt "Choose a card in Archives to add to the bottom of R&D"
                                   :show-discard true
                                   :choices {:card #(and (in-discard? %)
                                                         (corp? %))}
                                   :msg (msg "trash 1 card from HQ and add "
                                             (if (:seen target) (:title target) "a card")
                                             " from Archives to the bottom of R&D")
                                   :effect (effect (move target :deck))}
                                  card nil))}]})

(defcard "Working Prototype"
  {:events [{:event :rez
             :silent (req true)
             :async true
             :effect (effect (add-counter eid card :power 1 nil))}]
   :abilities [{:action true
                :cost [(->c :click 1) (->c :power 1)]
                :label "Gain 3 [Credits]"
                :msg "gain 3 [Credits]"
                :keep-menu-open :while-power-tokens-left
                :async true
                :effect (req (gain-credits state side eid 3))}
               {:action true
                :cost [(->c :click 1) (->c :power 5)]
                :label "Gain 6 [Credits]. Add 1 resource to the top of the stack"
                :keep-menu-open :while-5-power-tokens-left
                :msg "gain 6 [Credits]"
                :async true
                :effect
                (req (wait-for (gain-credits state side 6)
                               (continue-ability
                                 state side
                                 {:prompt "Choose a resource"
                                  :req (req (seq (all-installed-runner-type state :resource)))
                                  :choices {:card #(resource? %)}
                                  :msg (msg (str "add " (:title target) " to the top of the stack"))
                                  :effect (req (move state :runner target :deck {:front true}))}
                                 card nil)
                               (effect-completed state side eid)))}]})

(defcard "Worlds Plaza"
  {:abilities [{:action true
                :label "Install an asset on this asset"
                :req (req (< (count (:hosted card)) 3))
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :prompt "Choose an asset to install"
                :choices {:card #(and (asset? %)
                                      (in-hand? %)
                                      (corp? %))}
                :msg (msg "host " (:title target))
                :async true
                :effect (req (wait-for (corp-install state side target card nil) ;; install target onto card
                                       (rez state side eid (last (:hosted (get-card state card))) {:cost-bonus -2})))}]})

(defcard "Zaibatsu Loyalty"
  {:prevention [{:prevents :expose
                 :type :ability
                 :label "1 [Credit]: Zaibatsu Loyalty"
                 :ability {:cost [(->c :credit 1)]
                           :req (req (preventable? context))
                           :msg "prevent a card from being exposed"
                           :async true
                           :effect (req (prevent-expose state side eid card))}}
                {:prevents :expose
                 :type :ability
                 :label "[trash]: Zaibatsu Loyalty"
                 :ability {:cost [(->c :trash-can)]
                           :req (req (preventable? context))
                           :msg "prevent a card from being exposed"
                           :async true
                           :effect (req (prevent-expose state side eid card))}}]
   :derezzed-events [{:event :expose-interrupt
                      :async true
                      :effect (req (let [ctx context]
                                     (continue-ability
                                       state side
                                       {:optional
                                        {:req (req (not (rezzed? card)))
                                         :prompt (msg "The Runner is about to expose " (enumerate-str (map #(card-str state % {:visible true}) (:cards ctx))) ". Rez Zaibatsu Loyalty?")
                                         :yes-ability {:async true
                                                       :effect (effect (rez eid card))}}}
                                       card nil)))}]})

(defcard "Zealous Judge"
  {:rez-req (req tagged)
   :abilities [{:action true
                :async true
                :label "Give the Runner 1 tag"
                :cost [(->c :click 1) (->c :credit 1)]
                :keep-menu-open :while-clicks-left
                :msg "give the Runner 1 tag"
                :effect (effect (gain-tags eid 1))}]})
