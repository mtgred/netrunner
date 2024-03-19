(ns game.cards.assets
  (:require
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card installed-access-trigger]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [bad-publicity-prevent gain-bad-publicity
                                    lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed get-remotes
                            installable-servers]]
   [game.core.card :refer [agenda? asset? can-be-advanced? corp? event? corp-installable-type?
                           faceup? fake-identity? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype? ice?
                           identity? in-deck? in-discard? in-hand? in-server? installed? is-type?
                           operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [corp-recur corp-rez-toast defcard
                                  reorder-choice trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw first-time-draw-bonus max-draw
                              remaining-draws]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed is-basic-advance-action? make-eid]]
   [game.core.engine :refer [pay register-events resolve-ability]]
   [game.core.events :refer [first-event? no-event? turn-events]]
   [game.core.expose :refer [expose-prevent]]
   [game.core.flags :refer [lock-zone prevent-current
                            prevent-draw
                            register-turn-flag! release-zone]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-extra-sub! remove-extra-subs! update-all-ice
                          update-ice-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-msg]]
   [game.core.moving :refer [as-agenda mill move remove-from-currently-drawing
                             swap-cards swap-installed trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-value]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon set-prop]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [end-run]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-central? is-remote? target-server zone->name]]
   [game.core.set-aside :refer [get-set-aside set-aside-for-me swap-set-aside-cards]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
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

(defn campaign
  "Creates a Campaign with X counters draining Y per-turn.
  Trashes itself when out of counters"
  ([counters per-turn] (campaign counters per-turn :credit))
  ([counters per-turn counter-type]
   (let [num-counters (fn [card] (min per-turn (get-counters card counter-type)))
         ability {:msg (msg "gain " (num-counters card) " [Credits]")
                  :once :per-turn
                  :req (req (:corp-phase-12 @state))
                  :label (str "Gain " per-turn " [Credits] (start of turn)")
                  :async true
                  :effect (req (wait-for (gain-credits state side (num-counters card))
                                         (add-counter state side eid card counter-type (- (num-counters card)) nil)))}]
     {:on-rez {:effect (req (add-counter state side card counter-type counters))}
      :derezzed-events [corp-rez-toast]
      :events [(trash-on-empty counter-type)
               (assoc ability :event :corp-turn-begins)]
      :abilities [ability]})))

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
                   (if (not (empty? rem))
                     (continue-ability state side
                                       (return-to-top rem reveal)
                                       card nil)
                     (effect-completed state side eid))))}))

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
                :req (req (not (:run @state)))
                :choices {:card #(and (corp-installable-type? %)
                                      (not (agenda? %))
                                      (in-hand? %)
                                      (corp? %))}
                :msg (msg (corp-install-msg target))
                :cost [:trash-can]
                :effect (effect (corp-install eid target nil nil))}]})

(defcard "Aggressive Secretary"
  (advance-ambush 2 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :waiting-prompt true
                     :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "program") " to trash")
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card #(and (installed? %)
                                           (program? %))}
                     :msg (msg "trash " (enumerate-str (map :title targets)))
                     :async true
                     :effect (effect (trash-cards eid targets {:cause-card card}))}))

(defcard "Alexa Belsky"
  {:abilities [{:label "Shuffle all cards in HQ into R&D"
                :async true
                :cost [:trash-can]
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
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:label "Gain 2 [Credits] for each counter on Alix T4LB07"
                :cost [:click 1 :trash-can]
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
                  :cost [:trash-can]
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
                        :player :corp
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
                 :effect (effect (add-prop card :advance-counter 1 {:placed true}))}]
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
                                   :effect (effect (add-prop :corp ice :advance-counter target {:placed true})
                                                   (add-prop :corp card :advance-counter (- target) {:placed true})
                                                   (system-msg (str "uses " (:title card) " to move "
                                                                    (quantify target "advancement counter")
                                                                    " to " (card-str state ice))))}}}
                                card nil)))}]
     :abilities [ability]}))

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

(defcard "B-1001"
  {:abilities [{:req (req (not this-server))
                :cost [:tag 1]
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
  {:abilities [{:cost [:click 1 :trash-can]
                :msg "gain [Click][Click]"
                :effect (effect (gain-clicks 2))}]})

(defcard "Behold!"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:req (req (not (in-discard? card)))
                :waiting-prompt true
                :prompt (msg "Pay 4 [Credits] to use " (:title card) " ability?")
                :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}
                :yes-ability {:async true
                              :cost [:credit 4]
                              :msg "give the runner 2 tags"
                              :effect (req (gain-tags state :corp eid 2))}}}})

(defcard "Bio-Ethics Association"
  (let [ability {:req (req unprotected)
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
                :prompt "Choose a card in HQ to install"
                :choices {:card #(and (not (operation? %))
                                      (in-hand? %)
                                      (corp? %))}
                :cost [:trash-can]
                :async true
                :effect (effect (corp-install eid target nil nil))
                :msg (msg (corp-install-msg target))}]})

(defcard "Blacklist"
  {:on-rez {:effect (effect (lock-zone (:cid card) :runner :discard))}
   :leave-play (effect (release-zone (:cid card) :runner :discard))})

(defcard "Bladderwort"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
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
   :on-access {:async true
               :effect (req (let [c (first (get-in @state [:runner :deck]))]
                              (system-msg state :corp (str "uses " (:title card) " to do 1 meat damage"
                                                           " and to trash " (:title c)
                                                           " from the top of the stack"))
                              (wait-for (mill state :corp :runner 1)
                                        (damage state side eid :meat 1 {:card card}))))}})

(defcard "Broadcast Square"
  {:events [{:event :pre-bad-publicity
             :async true
             :trace {:base 3
                     :successful {:msg "prevents all bad publicity"
                                  :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}]})

(defcard "C.I. Fund"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (pos? (:credit corp)))}
   :abilities [{:label "Store up to 3 [Credit] (start of turn)"
                :prompt "How many credits do you want to store?"
                :once :per-turn
                :choices {:number (req (min (:credit corp) 3))}
                :async true
                :effect (effect (add-counter card :credit target)
                                (lose-credits eid target))
                :msg (msg "store " target " [Credit]")}
               {:label "Take all hosted credits"
                :cost [:credit 2 :trash-can]
                :msg (msg "trash it and gain " (get-counters card :credit) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (get-counters card :credit)))}]
   :events [{:event :corp-turn-begins
             :msg "place 2 [Credits] on itself"
             :req (req (>= (get-counters card :credit) 6))
             :effect (effect (add-counter card :credit 2))}]})

(defcard "Calvin B4L3Y"
  {:abilities [{:cost [:click 1]
                :msg "draw 2 cards"
                :once :per-turn
                :async true
                :effect (effect (draw eid 2))}]
   :on-trash {:interactive (req true)
              :optional
              {:req (req (= :runner side))
               :waiting-prompt true
               :prompt "Draw 2 cards?"
               :player :corp
               :yes-ability {:msg "draw 2 cards"
                             :effect (effect (draw eid 2))}}}})

(defcard "Capital Investors"
  {:abilities [{:cost [:click 1]
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
  (let [ability {:label "Gain 4 [Credits] and draw 1 card"
                 :interactive (req true)
                 :optional
                 {:once :per-turn
                  :prompt "Remove 1 hosted advancement counter to gain 4 [Credits] and draw 1 card?"
                  :req (req (pos? (get-counters card :advancement)))
                  :yes-ability
                  {:msg "remove 1 hosted advancement counter from itself to gain 4 [Credits] and draw 1 card"
                   :async true
                   :effect (req
                             (add-prop state :corp card :advance-counter -1)
                             (wait-for
                               (gain-credits state side 4)
                               (draw state side eid 1)))}}}
        trash-ab {:cost [:advancement 1 :trash-can]
                  :label "Gain 3 [Credits]"
                  :msg (msg "gain 3 [Credits]")
                  :async true
                  :effect (req (gain-credits state :corp eid 3))}]
    {:advanceable :always
     :flags {:corp-phase-12 (req true)}
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability trash-ab]}))

(defcard "Chekist Scion"
  (advance-ambush 0 {:msg (msg "give the Runner " (quantify (inc (get-counters (get-card state card) :advancement)) "tag"))
                     :async true
                     :effect (effect (gain-tags :corp eid (inc (get-counters (get-card state card) :advancement))))}))

(defcard "Chief Slee"
  {:events [{:event :end-of-encounter
             :req (req (pos? (count (remove :broken (:subroutines (:ice context))))))
             :msg (req (let [unbroken-count (count (remove :broken (:subroutines (:ice context))))]
                        (str "place " (quantify unbroken-count "power counter") " on itself")))
             :effect (effect (add-counter :corp card :power (count (remove :broken (:subroutines (:ice context))))))}]
   :abilities [{:cost [:click 1 :power 5]
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
             :choices (req [(when (can-pay? state :runner eid card nil [:credit 1])
                              "Pay 1 [Credits]")
                            "Take 1 tag"])
             :msg (msg (if (= target "Take 1 tag")
                          "give the runner 1 tag"
                          (str "force the runner to " (decapitalize target))))
             :async true
             :effect (req (if (= target "Pay 1 [Credits]")
                            (wait-for (pay state :runner (make-eid state eid) card :credit 1)
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
                 :req (req (or (can-pay? state :runner eid card nil [:credit 1])
                               (seq (:deck runner))))
                 :player :runner
                 :once :per-turn
                 :prompt "Choose one"
                 :waiting-prompt true
                 :choices (req [(when (can-pay? state :runner eid card nil [:credit 1])
                                  "Pay 1 [Credits]")
                                (when (or (not (can-pay? state :runner eid card nil [:credit 1]))
                                          (seq (:deck runner)))
                                  "Trash the top card of the stack")])
                 :label "make the Runner pay 1 [Credits] or trash the top card of the stack (start of turn)"
                 :msg (msg "force the Runner to " (decapitalize target))
                 :effect (req (if (= target "Pay 1 [Credits]")
                                (wait-for (pay state side (make-eid state eid) card :credit 1)
                                  (system-msg state side (:msg async-result))
                                  (effect-completed state side eid))
                                (mill state :runner eid :runner 1)))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Cohort Guidance Program"
  (let [abi {:prompt "Choose one"
             :interactive (req true)
             :choices (req [(when (seq (:hand corp)) "Trash 1 card from HQ to gain 2 [Credits] and draw 1 card")
                            (when (some #(not (:seen %)) (:discard corp))
                              "Turn 1 facedown card in Archives faceup to place 1 advancement counter")
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
                                                 :effect (effect
                                                           (add-prop target
                                                                     :advance-counter 1
                                                                     {:placed true}))}
                                                card nil))})
                              card nil)))}]
    {:flags {:corp-phase-12 (req true)}
     :derezzed-events [corp-rez-toast]
     :events [(assoc abi :event :corp-turn-begins)]}))

(defcard "Commercial Bankers Group"
  (let [ability {:req (req unprotected)
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
                    (filter can-be-advanced? it)
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
                :effect (effect
                          (continue-ability
                            (let [from-ice target]
                              {:prompt "Choose a piece of ice that can be advanced"
                               :choices {:card #(and (ice? %)
                                                     (not (same-card? from-ice %))
                                                     (can-be-advanced? %))}
                               :msg (msg "move an advancement token from "
                                         (card-str state from-ice)
                                         " to "
                                         (card-str state target))
                               :effect (effect (add-prop :corp target :advance-counter 1)
                                               (add-prop :corp from-ice :advance-counter -1))})
                            card nil))}]})

(defcard "Contract Killer"
  {:advanceable :always
   :abilities [{:label "Trash a connection"
                :async true
                :cost [:click 1 :trash-can]
                :req (req (>= (get-counters card :advancement) 2))
                :choices {:card #(has-subtype? % "Connection")}
                :msg (msg "trash " (:title target))
                :effect (effect (trash eid target {:cause-card card}))}
               {:label "Do 2 meat damage"
                :async true
                :cost [:click 1 :trash-can]
                :req (req (>= (get-counters card :advancement) 2))
                :msg "do 2 meat damage"
                :effect (effect (damage eid :meat 2 {:card card}))}]})

(defcard "Corporate Town"
  {:derezzed-events [corp-rez-toast]
   :additional-cost [:forfeit]
   :flags {:corp-phase-12 (req (and (rezzed? card)
                                    (->> (all-active-installed state :runner)
                                         (filter resource?)
                                         count
                                         pos?)))}
   :abilities [{:label "Trash a resource"
                :once :per-turn
                :async true
                :prompt "Choose a resource to trash"
                :choices {:card resource?}
                :msg (msg "trash " (:title target))
                :effect (effect (trash eid target {:unpreventable true :cause-card card}))}]})

(defcard "CPC Generator"
  {:events [{:event :runner-click-credit
             :req (req (first-event? state side :runner-click-credit))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "CSR Campaign"
  (let [ability {:once :per-turn
                 :async true
                 :label "Draw 1 card (start of turn)"
                 :interactive (req true)
                 :effect (effect (continue-ability
                                   {:optional
                                    {:prompt "Draw 1 card?"
                                     :autoresolve (get-autoresolve :auto-fire)
                                     :yes-ability {:async true
                                                   :msg "draw 1 card"
                                                   :effect (effect (draw eid 1))}}}
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
             :effect (effect (add-counter :corp card :credit 2))}]
   :abilities [{:label "Take all hosted credits"
                :cost [:trash-can]
                :req (req (pos? (get-counters card :credit)))
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (get-counters card :credit)))}
               {:async true
                :effect (effect (add-counter card :credit -1)
                                (gain-credits eid 1))
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
  {:events [{:event :run-ends
             :req (req (and tagged (:successful target)))
             :msg "do 2 meat damage"
             :async true
             :effect (effect (damage eid :meat 2 {:card card}))}]})

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
    {:abilities [{:cost [:click 2]
                  :keep-menu-open :while-2-clicks-left
                  :msg "place 1 power counter in itself"
                  :effect (effect (add-counter card :power 1))}]
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
   :events [{:event :rez
             :req (req (same-card? card (:card context)))
             :msg "place 1 power counter on itself"
             :effect (effect (add-counter card :power 1))}
            {:event :corp-turn-begins
             :msg "place 1 power counter on itself"
             :effect (effect (add-counter card :power 1))}]})

(defcard "Drago Ivanov"
  {:advanceable :always
   :abilities [{:cost [:advancement 2]
                :req (req (= :corp (:active-player @state)))
                :msg "give the runner a tag"
                :async true
                :effect (effect (gain-tags :corp eid 1))}]})

(defcard "Drudge Work"
  {:on-rez {:effect (effect (add-counter card :power 3))}
   :events [(trash-on-empty :power)]
   :abilities [{:cost [:click 1 :power 1]
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
   :flags {:corp-phase-12 (req (some #(and (can-be-advanced? %)
                                           (in-server? %))
                                     (all-installed state :corp)))}
   :abilities [{:cost [:credit 1]
                :label "Place 1 advancement token on a card that can be advanced in a server"
                :choices {:card #(and (can-be-advanced? %)
                                      (installed? %)
                                      (in-server? %))} ; should be *in* a server
                :once :per-turn
                :msg (msg "place 1 advancement token on " (card-str state target))
                :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]})

(defcard "Echo Chamber"
  {:abilities [{:label "Add this asset to your score area as an agenda worth 1 agenda point"
                :cost [:click 3]
                :msg "add itself to their score area as an agenda worth 1 agenda point"
                :effect (req (as-agenda state :corp card 1))}]})

(defcard "Edge of World"
  (letfn [(ice-count [state]
            (count (get-in (:corp @state) [:servers (last (:server (:run @state))) :ices])))]
    (installed-access-trigger 3 {:msg (msg "do " (ice-count state) " core damage")
                                 :async true
                                 :effect (effect (damage eid :brain (ice-count state)
                                                         {:card card}))})))

(defcard "Eliza's Toybox"
  {:abilities [{:cost [:click 3]
                :keep-menu-open :while-3-clicks-left
                :choices {:card #(not (:rezzed %))}
                :label "Rez a card at no cost" :msg (msg "rez " (:title target) " at no cost")
                :async true
                :effect (effect (rez eid target {:ignore-cost :all-costs}))}]})

(defcard "Elizabeth Mills"
  {:on-rez {:msg "remove 1 bad publicity"
            :effect (effect (lose-bad-publicity 1))}
   :abilities [{:cost [:click 1 :trash-can]
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
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:label "Draw 1 card and gain 2 [Credits] for each hosted power counter"
                :cost [:trash-can]
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
             :req (req (:ebc-rezzed card))
             :effect (effect (update! (dissoc card :ebc-rezzed)))}]
   :abilities [{:async true
                :once :per-turn
                :choices {:card (complement rezzed?)}
                :label "Rez a card, lowering the cost by 1 [Credits] (start of turn)"
                :msg (msg "rez " (:title target))
                :effect (req (wait-for (rez state side target {:no-warning true :cost-bonus -1})
                                       (update! state side (assoc card :ebc-rezzed (:cid target)))
                                       (effect-completed state side eid)))}
               {:prompt "Choose an asset to reveal and add to HQ"
                :msg (msg "reveal " (:title target) ", add it to HQ, and shuffle R&D")
                :choices (req (cancellable (filter asset?
                                                   (:deck corp))
                                           :sorted))
                :cost [:credit 1 :trash-can]
                :label "Search R&D for an asset"
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (shuffle! state side :deck)
                               (move state side target :hand)
                               (effect-completed state side eid)))}]})

(defcard "Executive Search Firm"
  {:abilities [{:prompt "Choose an Executive, Sysop, or Character to add to HQ"
                :msg (msg "reveal " (:title target) ", add it to HQ, and shuffle R&D")
                :choices (req (cancellable (filter #(or (has-subtype? % "Executive")
                                                        (has-subtype? % "Sysop")
                                                        (has-subtype? % "Character"))
                                                   (:deck corp))
                                           :sorted))
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :label "Search R&D for an Executive, Sysop, or Character"
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(defcard "Exposé"
  {:advanceable :always
   :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                :msg (msg "remove " (get-counters card :advancement) " bad publicity")
                :cost [:trash-can]
                :effect (effect (lose-bad-publicity (get-counters card :advancement)))}]})

(defcard "False Flag"
  (letfn [(tag-count [false-flag]
            (int (/ (get-counters false-flag :advancement) 2)))]
    {:advanceable :always
     :on-access {:req (req (pos? (get-counters (get-card state card) :advancement)))
                 :msg (msg "give the runner " (quantify (tag-count (get-card state card)) "tag"))
                 :async true
                 :effect (effect (gain-tags :corp eid (tag-count (get-card state card))))}
     :abilities [{:cost [:click 1 :advancement 7]
                  :label "Add this asset to your score area as an agenda worth 3 agenda points"
                  :msg "add itself to their score area as an agenda worth 3 agenda points"
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
             :msg "add itself to their score area as an agenda worth 1 agenda point"
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
   :abilities [{:label "Install an asset or agenda on this asset"
                :req (req (< (count (:hosted card)) 2))
                :cost [:click 1]
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
   :abilities [{:cost [:click 1 :advancement 3]
                :label "Add this asset to your score area as an agenda worth 1 agenda point"
                :msg "add itself to their score area as an agenda worth 1 agenda point"
                :effect (req (as-agenda state :corp card 1))}]})

(defcard "Genetics Pavilion"
  {:on-rez {:msg "prevent the Runner from drawing more than 2 cards during their turn"
            :effect (req (max-draw state :runner 2)
                         (when (zero? (remaining-draws state :runner))
                           (prevent-draw state :runner)))}
   :events [{:event :runner-turn-begins
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
   :abilities [{:label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                :cost [:click 1 :trash-can]
                :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (* 4 (get-counters card :advancement))))}]})

(defcard "Haas Arcology AI"
  {:advanceable :while-unrezzed
   :abilities [{:label "Gain [Click][Click]"
                :once :per-turn
                :msg "gain [Click][Click]"
                :cost [:click 1 :advancement 1]
                :effect (effect (gain-clicks 2))}]})

(defcard "Hearts and Minds"
  (let [political {:req (req unprotected)
                   :prompt "Choose a card you can advance to place 1 advancement counter on"
                   :choices {:card #(and (can-be-advanced? %)
                                         (installed? %))}
                   :msg (msg "place 1 advancement counter on " (card-str state target))
                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}
        ability {:req (req (:corp-phase-12 @state))
                 :label "Move 1 hosted advancement counter to another card you can advance (start of turn)"
                 :once :per-turn
                 :waiting-prompt true
                 :prompt "Choose an installed card to move 1 hosted advancement counter from"
                 :choices {:card #(and (installed? %)
                                       (get-counters % :advancement))}
                 :async true
                 :effect (effect
                           (continue-ability
                             (let [from-ice target]
                               {:prompt "Choose an installed card you can advance"
                                :choices {:card #(and (installed? %)
                                                      (can-be-advanced? %)
                                                      (not (same-card? from-ice %)))}
                                :msg (msg "move 1 hosted advancement counter from "
                                          (card-str state from-ice)
                                          " to "
                                          (card-str state target))
                                :async true
                                :effect (effect (add-prop :corp target :advance-counter 1)
                                                (add-prop :corp from-ice :advance-counter -1)
                                                (continue-ability political card nil))
                                :cancel-effect (effect (continue-ability political card nil))})
                             card nil))
                 :cancel-effect (effect (continue-ability political card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Honeyfarm"
  {:flags {:rd-reveal (req true)}
   :on-access {:msg "force the Runner to lose 1 [Credits]"
               :async true
               :effect (effect (lose-credits :runner eid 1))}})

(defcard "Hostile Architecture"
  (letfn [(valid-trash [target]
            (and (corp? (:card target))
                 (installed? (:card target))))]
    {:events [{:event :runner-trash
               :async true
               :once :per-turn
               :once-per-instance false
               :req (req (and (valid-trash target)
                              (first-event? state side :runner-trash #(valid-trash (first %)))))
               :msg "do 2 meat damage"
               :effect (effect (damage :corp eid :meat 2 {:card card}))}]}))

(defcard "Hostile Infrastructure"
  {:events [{:event :runner-trash
             :async true
             :once-per-instance false
             :req (req (corp? (:card target)))
             :msg "do 1 net damage"
             :effect (effect (damage :corp eid :net 1 {:card card}))}]})

(defcard "Hyoubu Research Facility"
  {:events [{:event :reveal-spent-credits
             :req (req (some? (first targets)))
             :once :per-turn
             :msg (msg "gain " target " [Credits]")
             :async true
             :effect (effect (gain-credits :corp eid target))}]})

(defcard "Ibrahim Salem"
  (let [trash-ability (fn [card-type]
                        {:req (req (seq (filter #(is-type? % card-type) (:hand runner))))
                         :prompt (str "Choose a " card-type " to trash")
                         :choices (req (filter #(is-type? % card-type) (:hand runner)))
                         :async true
                         :effect (effect (trash eid target {:cause-card card}))
                         :msg (msg "trash " (:title target) " from the grip")})
        choose-ability {:label "Trash 1 card in the grip of a named type"
                        :once :per-turn
                        :req (req (seq (:hand runner)))
                        :prompt "Choose a card type"
                        :choices ["Event" "Hardware" "Program" "Resource"]
                        :msg (msg "reveal " (enumerate-str (map :title (:hand runner))))
                        :async true
                        :effect (effect (continue-ability (trash-ability target) card nil))}]
    {:additional-cost [:forfeit]
     :flags {:corp-phase-12 (constantly true)}
     :derezzed-events [corp-rez-toast]
     :abilities [choose-ability]}))

(defcard "Illegal Arms Factory"
  (let [ability {:msg "gain 1 [Credits] and draw 1 card"
                 :label "Gain 1 [Credits] and draw 1 card (start of turn)"
                 :once :per-turn
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

(defcard "Isabel McGuire"
  {:abilities [{:label "Add an installed card to HQ"
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :choices {:card installed?}
                :msg (msg "move " (card-str state target) " to HQ")
                :effect (effect (move target :hand))}]})

(defcard "IT Department"
  {:abilities [{:cost [:click 1]
                :keep-menu-open :while-clicks-left
                :msg "place 1 power counter on itself"
                :effect (effect (add-counter card :power 1))}
               {:cost [:power 1]
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
  {:abilities [{:cost [:click 1]
                :keep-menu-open :while-clicks-left
                :msg "draw 2 cards"
                :async true
                :effect (effect (draw eid 2))}
               {:label "Shuffle up to 3 cards from Archives into R&D"
                :cost [:remove-from-game]
                :async true
                :effect (effect (shuffle-into-rd-effect eid card 3))}]})

(defcard "Janaína \"JK\" Dumont Kindelán"
  (let [ability {:label "Place 3 [Credits] on this asset (start of turn)"
                 :once :per-turn
                 :msg "place 3 [Credits] on itself"
                 :effect (effect (add-counter card :credit 3 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability
                 {:cost [:click 1]
                  :label "Take all hosted credits and add this asset to HQ. Install 1 card from HQ"
                  :async true
                  :msg (msg "take " (get-counters (get-card state card) :credit) " [Credits] and add itself to HQ")
                  :effect (req (wait-for (gain-credits state side (make-eid state eid)
                                                       (get-counters (get-card state card) :credit))
                                         (move state :corp card :hand)
                                         (continue-ability
                                           state side
                                           {:async true
                                            :prompt "Choose 1 card to install"
                                            :choices {:card #(and (corp-installable-type? %)
                                                                  (in-hand? %))}
                                            :msg (msg (corp-install-msg target))
                                            :effect (effect (corp-install eid target nil nil))}
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
               :effect (req (let [cid (first target)
                                  ability-idx (:ability-idx (:source-info eid))
                                  bac-cid (get-in @state [:corp :basic-action-card :cid])
                                  cause (if (keyword? (first target))
                                          (case (first target)
                                            :play-instant [bac-cid 3]
                                            :corp-click-install [bac-cid 2]
                                            (first target)) ; in clojure there's: (= [1 2 3] '(1 2 3))
                                          [cid ability-idx])
                                  clicks-spent (+ (get-in card [:seen-this-turn cause] 0) (second targets))]
                              (update! state side (assoc-in card [:seen-this-turn cause] clicks-spent))
                              (when (>= clicks-spent 3) ; can be >= 3 because :once :per-turn on ability
                                (resolve-ability state side ability (get-card state card) nil))))}
              {:event :corp-turn-ends
               :effect cleanup}]}))

(defcard "Kala Ghoda Real TV"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:msg "look at the top card of the stack"
                :effect (effect (continue-ability
                                  {:prompt (req (->> runner :deck first :title (str "The top card of the stack is ")))
                                   :choices ["OK"]}
                                  card nil))}
               {:async true
                :label "Trash the top card of the stack"
                :msg (msg "trash " (:title (first (:deck runner))) " from the stack")
                :cost [:trash-can]
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
                        {:effect (effect (add-counter card :power 1))
                         :msg "place 1 power counter on itself"}}}]})

(defcard "Lady Liberty"
  {:abilities [{:cost [:click 3]
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
             :effect (effect (add-counter card :power 1))}]})

(defcard "Lakshmi Smartfabrics"
  {:events [{:event :rez
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:req (req (seq (filter #(and (agenda? %)
                                             (>= (get-counters card :power)
                                                 (:agendapoints %)))
                                       (:hand corp))))
                :label "Reveal an agenda worth X points from HQ"
                :async true
                :cost [:x-power]
                :keep-menu-open :while-power-tokens-left
                :effect
                (effect
                  (continue-ability
                    {:prompt "Choose an agenda in HQ to reveal"
                     :choices {:req (req (and (agenda? target)
                                              (<= (:agendapoints target) (cost-value eid :x-power))))}
                     :msg (msg "reveal " (:title target) " from HQ")
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
                    card nil))}]})

(defcard "Launch Campaign"
  (campaign 6 2))

(defcard "Levy University"
  {:abilities [{:prompt "Choose a piece of ice"
                :msg (msg "adds " (:title target) " to HQ")
                :choices (req (cancellable (filter ice? (:deck corp)) :sorted))
                :label "Search R&D for a piece of ice"
                :cost [:click 1 :credit 1]
                :keep-menu-open :while-clicks-left
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(defcard "Lily Lockwell"
  {:on-rez {:async true
            :effect (effect (draw eid 3))
            :msg "draw 3 cards"}
   :abilities [{:label "Search R&D for an operation"
                :prompt "Choose an operation to add to the top of R&D"
                :waiting-prompt true
                :cost [:click 1 :tag 1]
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
   :abilities [{:label "Move any number of hosted credits to your credit pool"
                :req (req (>= (get-counters card :credit) 8))
                :cost [:click 1]
                :prompt "How many hosted credits do you want to take?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :async true
                :effect (effect (gain-credits eid target))}]
   :events [{:event :corp-turn-begins
             :msg "place 2 [Credit] on itself"
             :effect (effect (add-counter card :credit 2))}]})

(defcard "Lt. Todachine"
  {:events [{:event :rez
             :req (req (ice? (:card context)))
             :async true
             :msg "give the Runner 1 tag"
             :effect (req (gain-tags state :runner eid 1))}]})

(defcard "Malia Z0L0K4"
  (let [re-enable-target
        (req (if-let [malia-target (:malia-target card)]
               (if (:disabled (get-card state malia-target))
                 (do (system-msg state side (str "uses " (:title card) " to unblank "
                                                 (card-str state malia-target)))
                     (enable-card state :runner (get-card state malia-target))
                     (remove-icon state :runner card (get-card state malia-target))
                     (if-let [reactivate-effect (:reactivate (card-def malia-target))]
                       (resolve-ability state :runner eid reactivate-effect (get-card state malia-target) nil)
                       (effect-completed state nil eid)))
                 (effect-completed state nil eid))
               (effect-completed state nil eid)))]
    {:on-rez {:msg (msg "blank the text box of " (card-str state target))
              :choices {:card #(and (runner? %)
                                    (installed? %)
                                    (resource? %)
                                    (not (has-subtype? % "Virtual")))}
              :effect (effect (add-icon card target "MZ" (faction-label card))
                              (update! (assoc (get-card state card) :malia-target target))
                              (disable-card :runner (get-card state target)))}
     :leave-play re-enable-target
     :move-zone re-enable-target}))

(defcard "Marilyn Campaign"
  (let [ability {:once :per-turn
                 :interactive (req (>= 2 (get-counters card :credit)))
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain 2 [Credits] (start of turn)")
                 :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                 :async true
                 :effect (req (let [credits (min 2 (get-counters card :credit))]
                                (add-counter state side card :credit (- credits))
                                (wait-for (gain-credits state :corp credits)
                                          (if (not (pos? (get-counters (get-card state card) :credit)))
                                            (trash state :corp eid card {:unpreventable true :cause-card card})
                                            (effect-completed state :corp eid)))))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :on-rez {:effect (req (add-counter state side card :credit 8))}
     :abilities [(set-autoresolve :auto-reshuffle "Marilyn Campaign shuffling itself back into R&D")]
     :on-trash {:interactive (req true)
                :optional
                {:waiting-prompt true
                 :prompt (msg "Shuffle " (:title card) " into R&D?")
                 :autoresolve (get-autoresolve :auto-reshuffle)
                 :player :corp
                 :yes-ability {:msg "shuffle itself back into R&D"
                               :effect (effect (move :corp card :deck)
                                               (shuffle! :corp :deck))}}}}))

(defcard "Mark Yale"
  {:events [{:event :agenda-counter-spent
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]
   :abilities [{:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [:trash-can]
                :async true
                :effect (effect (gain-credits eid 2))}
               {:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [:any-agenda-counter]
                :async true
                :effect (effect (gain-credits eid 2))}]})

(defcard "Marked Accounts"
  (let [ability {:msg "take 1 [Credits]"
                 :label "Take 1 [Credits] (start of turn)"
                 :once :per-turn
                 :req (req (pos? (get-counters card :credit)))
                 :async true
                 :effect (effect (add-counter card :credit -1)
                                 (gain-credits eid 1))}]
    {:abilities [ability
                 {:cost [:click 1]
                  :msg "store 3 [Credits]"
                  :effect (effect (add-counter card :credit 3))}]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "MCA Austerity Policy"
  {:abilities [{:cost [:click 1]
                :once :per-turn
                :msg "force the Runner to lose a [Click] next turn and place a power counter on itself"
                :effect (req (register-events state side card
                                              [{:event :runner-turn-begins
                                                :unregister-once-resolved true
                                                :duration :until-runner-turn-begins
                                                :effect (effect (lose-clicks :runner 1))}])
                             (add-counter state side card :power 1))}
               {:cost [:click 1 :power 3 :trash-can]
                :msg "gain 4 [Click]"
                :effect (effect (gain-clicks 4))}]})

(defcard "Melange Mining Corp."
  {:abilities [{:cost [:click 3]
                :keep-menu-open :while-3-clicks-left
                :async true
                :effect (effect (gain-credits eid 7))
                :msg "gain 7 [Credits]"}]})

(defcard "Mental Health Clinic"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:static-abilities [(runner-hand-size+ 1)]
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

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
           :msg (msg "reveal " (enumerate-str (map :title targets)) " from Archives and shuffle them into R&D")
           :effect (req (wait-for (reveal state side targets)
                                  (doseq [c targets]
                                    (move state side c :deck))
                                  (shuffle! state side :deck)
                                  (let [agenda-count (count (filter agenda? targets))]
                                    (if (pos? agenda-count)
                                      (continue-ability
                                        state side
                                        (moon-pool-place-advancements agenda-count)
                                        card nil)
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
                    :cost [:remove-from-game]
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
  {:abilities [{:label "Search R&D for an Alliance card"
                :cost [:click 1]
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
                                 (corp-install state side eid target nil nil))))}]})

(defcard "Mumbad Construction Co."
  {:derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :effect (effect (add-prop card :advance-counter 1 {:placed true}))}]
   :abilities [{:cost [:credit 2]
                :keep-menu-open :while-advancement-tokens-left
                :req (req (and (pos? (get-counters card :advancement))
                               (not-empty (all-active-installed state :corp))))
                :label "Move an advancement token to a faceup card"
                :prompt "Choose a faceup card"
                :choices {:card faceup?}
                :msg (msg "move an advancement token to " (card-str state target))
                :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                (add-prop target :advance-counter 1 {:placed true}))}]})

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
                            (str (enumerate-str (map :title seen))
                                 (when (pos? n)
                                   (str (when-not (empty? seen) " and ")
                                        (quantify n "card")))))
                          " into R&D")
                :effect (req (doseq [c targets]
                               (move state side c :deck))
                             (shuffle! state side :deck))}]
   :implementation "[Erratum] Should be unique"})

(defcard "Nanoetching Matrix"
  {:abilities [{:cost [:click 1]
                :once :per-turn
                :msg "gain 2 [Credits]"
                :async true
                :effect (effect (gain-credits eid 2))}]
   :on-trash {:optional
              {:req (req (= :runner side))
               :player :corp
               :waiting-prompt true
               :prompt "Gain 2 [Credits]?"
               :yes-ability
               {:msg "gain 2 [Credits]"
                :async true
                :effect (effect (gain-credits :corp eid 2))}}}})

(defcard "NASX"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:implementation "Manual - click NASX to place power counters on itself"
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability
                 {:label "Place 1 power counter"
                  :cost [:credit 1]
                  :msg "place 1 power counter on itself"
                  :effect (effect (add-counter card :power 1))}
                 {:label "Place 2 power counters"
                  :cost [:credit 2]
                  :msg "place 2 power counters on itself"
                  :effect (effect (add-counter card :power 2))}
                 {:label "Gain 2 [Credits] for each hosted power counter"
                  :cost [:click 1 :trash-can]
                  :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                  :async true
                  :effect (effect (gain-credits eid (* 2 (get-counters card :power))))}]}))

(defcard "Net Analytics"
  (let [ability {:optional
                 {:player :corp
                  :autoresolve (get-autoresolve :auto-fire)
                  :waiting-prompt true
                  :prompt "Draw 1 card?"
                  :yes-ability
                  {:msg "draw 1 card"
                   :effect (effect (draw :corp eid 1))}}}]
    {:events [(-> ability
                  (assoc :event :runner-lose-tag)
                  (assoc-in [:optional :req] (req (= side :runner))))
              (-> ability
                  (assoc :event :runner-prevent)
                  (assoc-in [:optional :req] (req (seq (filter #(some #{:tag} %) targets)))))]
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
     :msg (msg "shuffle " (enumerate-str (map :title targets)) " into the stack")
     :effect (req (doseq [c targets]
                    (move state :runner c :deck))
                  (shuffle! state :runner :deck)
                  (effect-completed state side eid))}))

(defcard "News Team"
  {:flags {:rd-reveal (req true)}
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
            {:cost [:advancement cost :trash-can]
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
         :label "Take 3 [Credits] (start of turn)"
         :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
         :req (req (:corp-phase-12 @state))
         :effect (req (let [credits (min 3 (get-counters card :credit))]
                        (add-counter state side card :credit (- credits))
                        (wait-for
                          (gain-credits state :corp credits)
                          (if (pos? (get-counters (get-card state card) :credit))
                            (effect-completed state side eid)
                            (wait-for
                              (trash state :corp card {:unpreventable true :cause-card card})
                              (system-msg state :corp (str "trashes Nico Campaign"
                                                           (when (seq (:deck corp))
                                                             " and draws 1 card")))
                              (draw state :corp eid 1))))))}]
    {:data {:counter {:credit 9}}
     :derezzed-events [corp-rez-toast]
     :abilities [ability]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "Nightmare Archive"
  {:flags {:rd-reveal (req true)}
   :on-access {:async true
               :msg (msg (if (= target "Suffer 1 core damage")
                           "do 1 core damage"
                           (str "force the runner to " (decapitalize target))))
               :player :runner
               :prompt "Choose one"
               :choices ["Suffer 1 core damage" "Add Nightmare Archive to score area"]
               :effect (req (if (= target "Suffer 1 core damage")
                              (wait-for (damage state :corp :brain 1 {:card card})
                                        (move state :corp card :rfg)
                                        (effect-completed state side eid))
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

(defcard "PAD Campaign"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "PAD Factory"
  {:abilities [{:cost [:click 1]
                :keep-menu-open :while-clicks-left
                :label "Place 1 advancement token on a card"
                :choices {:card installed?}
                :msg (msg "place 1 advancement token on " (card-str state target))
                :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                             (let [tgtcid (:cid target)]
                               (register-turn-flag!
                                 state side
                                 target :can-score
                                 (fn [state side card]
                                   (if (and (= tgtcid
                                               (:cid card))
                                            (<= (get-advancement-requirement card)
                                                (get-counters card :advancement)))
                                     ((constantly false)
                                      (toast state :corp "Cannot score due to PAD Factory." "warning"))
                                     true)))))}]})

(defcard "Pālanā Agroplex"
  (let [ability {:msg "make each player draw 1 card"
                 :label "Make each player draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (req (wait-for (draw state :corp 1)
                                        (draw state :runner eid 1)))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Personalized Portal"
  {:events [{:event :corp-turn-begins
             :interactive (req true)
             :async true
             :effect (req (wait-for (draw state :runner 1)
                                    (let [cnt (count (get-in @state [:runner :hand]))
                                          credits (quot cnt 2)]
                                      (system-msg state :corp
                                                  (str "uses " (:title card) " to force the runner to draw "
                                                       "1 card and gain " credits " [Credits]"))
                                      (gain-credits state :corp eid credits))))}]})

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
     :effect (effect (score eid target {:no-req true}))}))

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
                                                 {:install-state (:install-state (card-def agenda) :unrezzed)})
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
  {:interactions {:prevent [{:type #{:net}
                             :req (req (= :corp (:side target)))}]}
   :abilities [{:label "Prevent 1 net damage to place power counter on Prāna Condenser"
                :msg "prevent 1 net damage, place 1 power counter, and gain 3 [Credits]"
                :async true
                :req (req true)
                :effect (req (add-counter state side card :power 1)
                             (gain-credits state :corp eid 3)
                             (damage-prevent state :corp :net 1))}
               {:msg (msg "deal " (get-counters card :power) " net damage")
                :label "deal net damage"
                :cost [[:click 2] [:trash-can]]
                :effect (effect (damage eid :net (get-counters card :power) {:card card}))}]})

(defcard "Primary Transmission Dish"
  {:recurring 3
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "Private Contracts"
  {:on-rez {:effect (effect (add-counter card :credit 14))}
   :events [(trash-on-empty :credit)]
   :abilities [{:cost [:click 1]
                :keep-menu-open :while-clicks-left
                :label "Take hosted credits"
                :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 2 (get-counters card :credit))]
                               (add-counter state side card :credit (- credits))
                               (gain-credits state :corp eid credits)))}]})

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

(defcard "Public Health Portal"
  (let [ability {:once :per-turn
                 :label "Reveal the top card of R&D and gain 2 [Credits] (start of turn)"
                 :interactive (req true)
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
  {:on-rez {:effect (effect (add-counter card :power 3))}
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :req (req (pos? (get-counters card :power)))
             :effect (effect (add-counter card :power -1))}
            {:event :counter-added
             :req (req (same-card? card target)
                       (not (pos? (get-counters card :power))))
             :msg "add itself to their score area as an agenda worth 1 agenda point"
             :effect (effect (as-agenda card 1))}]})

(defcard "Quarantine System"
  (letfn [(rez-ice [cnt] {:prompt "Choose a piece of ice to rez"
                          :async true
                          :choices {:card #(and (ice? %)
                                                (not (rezzed? %)))}
                          :msg (msg "rez " (:title target))
                          :effect (req (let [agenda (last (:rfg corp))
                                             ap (:agendapoints agenda 0)]
                                         (wait-for (rez state side target {:no-warning true :cost-bonus (* ap -2)})
                                                   (if (< cnt 3)
                                                     (continue-ability state side (rez-ice (inc cnt)) card nil)
                                                     (effect-completed state side eid)))))})]
    {:abilities [{:label "Forfeit agenda to rez up to 3 pieces of ice with a 2 [Credit] discount per agenda point"
                  :req (req (pos? (count (:scored corp))))
                  :cost [:forfeit]
                  :effect (req (continue-ability state side (rez-ice 1) card nil))}]}))

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
             :effect (effect (add-counter card :advancement 1))}]
   :abilities [{:label "Move hosted advancement tokens to another card"
                :trash-icon true
                :cost [:trash-can]
                :async true
                :prompt "How many hosted advancement tokens do you want to move?"
                :choices {:number (req (get-counters card :advancement))
                          :default (req (get-counters card :advancement))}
                :effect (req (let [num-counters target]
                               (continue-ability
                                 state side
                                 {:async true
                                  :prompt "Choose a card that can be advanced"
                                  :choices {:card can-be-advanced?}
                                  :effect (effect (add-counter target :advancement num-counters {:placed true})
                                                  (system-msg (str "uses " (:title card) " to move " (quantify num-counters "hosted advancement token") " to " (card-str state target)))
                                                  (effect-completed eid))}
                                 card nil)))}]})

(defcard "Refuge Campaign"
  (let [ability {:msg "gain 2 [Credits]"
                 :label "Gain 2 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 2))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Regolith Mining License"
  {:data {:counter {:credit 15}}
   :events [(trash-on-empty :credit)]
   :abilities [{:label "Take 3 [Credits] from this asset"
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 3 (get-counters card :credit))]
                               (wait-for (gain-credits state :corp (make-eid state eid) credits)
                                         (add-counter state side card :credit (- credits) {:placed true})
                                         (effect-completed state side eid))))}]})

(defcard "Reversed Accounts"
  {:advanceable :always
   :abilities [{:cost [:click 1 :trash-can]
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
                 :effect (req (add-counter state side card :power -1)
                              (if (zero? (get-counters (get-card state card) :power))
                                (wait-for (trash state side card {:cause-card card})
                                          (continue-ability state side payout-ab card nil))
                                (effect-completed state side eid)))}]
    {:on-rez {:effect (effect (add-counter card :power 3))}
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
   :abilities [{:cost [:click 1 :trash-can]
                :req (req (>= (get-counters card :advancement) 4))
                :msg "do 3 net damage"
                :async true
                :effect (effect (damage eid :net 3 {:card card}))}]})

(defcard "Roughneck Repair Squad"
  {:abilities [{:label "Gain 6 [Credits], may remove 1 bad publicity"
                :cost [:click 3]
                :keep-menu-open :while-3-clicks-left
                :msg "gain 6 [Credits]"
                :async true
                :effect (req (wait-for
                               (gain-credits state side 6)
                               (continue-ability
                                 state side
                                 {:optional
                                  {:req (req (pos? (count-bad-pub state)))
                                   :player :corp
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
             :req (req (= :credit (first target)))
             :effect (effect (update-all-ice))}
            {:event :corp-lose
             :req (req (= :credit (first target)))
             :effect (effect (update-all-ice))}]
   :leave-play (effect (update-all-ice))})

(defcard "Sealed Vault"
  {:abilities [{:label "Store any number of credits"
                :cost [:credit 1]
                :prompt "How many credits do you want to move?"
                :choices {:number (req (- (:credit corp) 1))}
                :msg (msg "store " target " [Credits]")
                :async true
                :effect (effect (add-counter card :credit target)
                                (lose-credits eid target))}
               {:label "Move any number of credits to your credit pool"
                :cost [:click 1]
                :prompt "How many credits do you want to move?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :async true
                :effect (effect (gain-credits eid target))}
               {:label "Move any number of credits to your credit pool"
                :prompt "How many credits do you want to move?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :cost [:trash-can]
                :async true
                :effect (effect (gain-credits eid target))}]})

(defcard "Security Subcontract"
  {:abilities [{:cost [:click 1 :ice 1]
                :keep-menu-open :while-clicks-left
                :msg "gain 4 [Credits]"
                :label "Gain 4 [Credits]"
                :async true
                :effect (effect (gain-credits eid 4))}]})

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
  {:abilities [{:cost [:click 1]
                :keep-menu-open :while-clicks-left
                :msg "draw 1 card from the bottom of R&D"
                :effect (effect (move (last (:deck corp)) :hand))}
               {:label "Search R&D for an agenda"
                :prompt "Choose an agenda to add to the bottom of R&D"
                :msg (msg "reveal " (:title target) " from R&D and add it to the bottom of R&D")
                :choices (req (cancellable (filter agenda? (:deck corp)) :sorted))
                :cost [:trash-can]
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
                :cost [:trash-can]
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
                     :msg (msg "trash " (enumerate-str (map :title targets)))
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card #(and (installed? %)
                                           (hardware? %))}
                     :effect (effect (trash-cards eid targets {:cause-card card}))}))

(defcard "Shi.Kyū"
  {:on-access
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
                                                                       " to their score area as an agenda worth "
                                                                       (quantify -1 "agenda point")))
                                        (as-agenda state :runner card -1)
                                        (effect-completed state side eid))
                                    (do (system-msg state :runner (str "takes " dmg " net damage from " (:title card)))
                                        (damage state :corp eid :net dmg {:card card}))))})
                  card targets))}}}})

(defcard "Shock!"
  {:flags {:rd-reveal (req true)}
   :on-access {:msg "do 1 net damage"
               :async true
               :effect (effect (damage eid :net 1 {:card card}))}})

(defcard "SIU"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Trace 3 - Give the Runner 1 tag"
                :req (req (:corp-phase-12 @state))
                :async true
                :cost [:trash-can]
                :effect (effect (continue-ability
                                  {:trace {:base 3
                                           :label "Trace 3 - Give the Runner 1 tag"
                                           :successful {:msg "give the Runner 1 tag"
                                                        :async true
                                                        :effect (effect (gain-tags :runner eid 1))}}}
                                  card nil))}]})

(defcard "Snare!"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:req (req (not (in-discard? card)))
                :waiting-prompt true
                :prompt (msg "Pay 4 [Credits] to use " (:title card) " ability?")
                :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}
                :yes-ability {:async true
                              :cost [:credit 4]
                              :msg "do 3 net damage and give the Runner 1 tag"
                              :effect (req (wait-for (damage state side :net 3 {:card card})
                                                     (gain-tags state :corp eid 1)))}}}})

(defcard "Space Camp"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:waiting-prompt true
                :prompt "Place 1 advancement token on a card that can be advanced?"
                :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                              :prompt "Choose a card to place an advancement token on"
                              :choices {:card can-be-advanced?}
                              :effect (effect (add-prop target :advance-counter 1 {:placed true}))}}}})

(defcard "Spin Doctor"
  {:on-rez {:async true
            :msg "draw 2 cards"
            :effect (effect (draw eid 2))}
   :abilities [{:label "Shuffle up to 2 cards from Archives into R&D"
                :cost [:remove-from-game]
                :async true
                :effect (effect (shuffle-into-rd-effect eid card 2))}]})

(defcard "Storgotic Resonator"
  {:abilities [{:cost [:click 1 :power 1]
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
             :effect (effect (add-counter card :power 1))}]})

(defcard "Student Loans"
  {:static-abilities [{:type :play-additional-cost
                       :req (req (and (event? target)
                                      (seq (filter #(= (:title %) (:title target)) (:discard runner)))))
                       :value [:credit 2]}]})

(defcard "Superdeep Borehole"
  ;; the "when it is empty" text is reliant on the card being loaded
  {:on-rez {:effect (req (add-counter state side card :bad-publicity 6)
                         (update! state side (assoc-in (get-card state card) [:special :borehole-valid] true)))}
   :events [{:event :corp-turn-begins
             :msg (msg "take 1 bad publicity from " (:title card))
             :async true
             :effect (req (add-counter state side card :bad-publicity -1 nil)
                          (gain-bad-publicity state :corp eid 1))}
            {:event :counter-added
             :req (req (and (same-card? card target)
                            (not (pos? (get-counters (get-card state card) :bad-publicity)))
                            (:borehole-valid (:special card))))
             :msg "win the game"
             :effect (req (win state :corp (:title card)))}]})

(defcard "Sundew"
  ; If this a run event then handle in :begin-run as we do not know the server
  ; being run on in :runner-spent-click.
  {:events [{:event :runner-spent-click
             :req (req (first-event? state side :runner-spent-click))
             :msg (req (when-not (= :run (get-in @state [:runner :register :click-type]))
                         "gain 2 [Credits]"))
             :async true
             :effect (req (if (not= :run (get-in @state [:runner :register :click-type]))
                            (gain-credits state :corp eid 2)
                            (effect-completed state side eid)))}
            {:event :run
             :once :per-turn
             :req (req (first-event? state side :runner-spent-click))
             :msg (req (when (and (= :run (get-in @state [:runner :register :click-type]))
                                  (not this-server))
                         "gain 2 [Credits]"))
             :async true
             :effect (req (if (and (= :run (get-in @state [:runner :register :click-type]))
                                   (not this-server))
                            (gain-credits state :corp eid 2)
                            (effect-completed state side eid)))}]})

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
                 :effect (req (wait-for (trash state side target {:unpreventable true :cause-card card})
                                        (gain-credits state side eid 3)))}]
    {:flags {:corp-phase-12 (req (>= (count (all-installed state :corp)) 2))}
     :events [(assoc ability
                     :event :corp-turn-begins
                     :interactive (req true))]
     :abilities [ability]}))

(defcard "Synth DNA Modification"
  {:events [{:event :subroutines-broken
             :req (req (and (has-subtype? (first targets) "AP")
                            (first-event? state side :subroutines-broken
                                          (fn [targets] (has-subtype? (first targets) "AP")))))
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
             :msg (msg (corp-install-msg target))
             :effect (effect (corp-install eid target nil {:ignore-install-cost true}))}]})

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
                                 (corp-install state side eid target nil nil))))}]})

(defcard "TechnoCo"
  (letfn [(is-techno-target [card]
            (or (program? card)
                (hardware? card)
                (and (resource? card)
                     (has-subtype? card "Virtual"))))]
    {:static-abilities [{:type :install-cost
                         :req (req (and (is-techno-target target)
                                        (not (:facedown (second targets)))))
                         :value 1}]
     :events [{:event :runner-install
               :req (req (and (is-techno-target (:card context))
                              (not (:facedown context))))
               :msg "gain 1 [Credits]"
               :async true
               :effect (effect (gain-credits :corp eid 1))}]}))

(defcard "Tenma Line"
  {:abilities [{:label "Swap 2 installed pieces of ice"
                :cost [:click]
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
  (letfn [(derez-card [advancements]
            (when (pos? advancements)
              {:async true
               :waiting-prompt true
               :prompt "Derez a card"
               :choices {:card #(and (installed? %)
                                     (rezzed? %))}
               :effect (req (derez state side target)
                            (continue-ability state side (derez-card (dec advancements)) card nil))}))]
    {:advanceable :always
     :abilities [{:label "Derez 1 card for each advancement token"
                  :req (req (pos? (get-counters card :advancement)))
                  :msg (msg "derez " (quantify (get-counters card :advancement) "card"))
                  :cost [:trash-can]
                  :async true
                  :effect (req (continue-ability state side (derez-card (get-counters card :advancement)) card nil))}]}))

(defcard "The Board"
  {:on-trash executive-trash-effect
   :static-abilities [{:type :agenda-value
                       :req (req (= :runner (:scored-side target)))
                       :value -1}]})

(defcard "The News Now Hour"
  {:events [{:event :runner-turn-begins
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
             :msg (msg (corp-install-msg target))
             :effect (effect (corp-install eid target nil {:ignore-install-cost true}))}]})

(defcard "The Root"
  {:recurring 3
   :interactions {:pay-credits {:req (req (or (#{:advance :corp-install :rez} (:source-type eid))
                                              (is-basic-advance-action? eid)))
                                :type :recurring}}})

(defcard "Thomas Haas"
  {:advanceable :always
   :abilities [{:label "Gain credits"
                :msg (msg "gain " (* 2 (get-counters card :advancement)) " [Credits]")
                :cost [:trash-can]
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

(defcard "Turtlebacks"
  {:events [{:event :server-created
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Ubiquitous Vig"
  (let [ability {:msg (msg "gain " (get-counters card :advancement)  " [Credits]")
                 :label "Gain 1 [Credits] for each advancement counter (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid (get-counters card :advancement)))}]
    {:derezzed-events [corp-rez-toast]
     :advanceable :always
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Urban Renewal"
  {:on-rez {:effect (effect (add-counter card :power 3))}
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :async true
             :interactive (req true)
             :effect (req (add-counter state side card :power -1)
                          (if (not (pos? (get-counters (get-card state card) :power)))
                            (wait-for (trash state side card {:cause-card card})
                                      (system-msg state :corp (str "uses " (:title card) " to do 4 meat damage"))
                                      (damage state side eid :meat 4 {:card card}))
                            (effect-completed state side eid)))}]})

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
              :msg (msg (corp-install-msg target))
              :effect
              (effect
                (continue-ability
                  (let [card-to-install target]
                    {:async true
                     :prompt "Choose a server"
                     :choices (req (remove (set (zone->name (get-zone card)))
                                           (installable-servers state card-to-install)))
                     :effect (effect (corp-install eid card-to-install target {:ignore-all-cost true}))})
                  card nil))}
   :abilities [{:label "Install 1 card"
                :async true
                :cost [:click 1]
                :once :per-turn
                :choices {:card #(and (corp? %)
                                      (in-hand? %)
                                      (not (operation? %)))}
                :msg (msg (corp-install-msg target))
                :effect (effect (corp-install eid target nil {:ignore-all-cost true}))}]})

(defcard "Vera Ivanovna Shuyskaya"
  (let [select-and-trash {:async true
                          :prompt "Choose a card to trash"
                          :waiting-prompt true
                          :choices (req (cancellable (:hand runner) :sorted))
                          :msg (msg "trash " (:title target) " from the grip")
                          :effect (effect (trash eid target {:cause-card card}))}
        ability {:interactive (req true)
                 :optional {:prompt "Reveal the grip and trash a card?"
                            :player :corp
                            :autoresolve (get-autoresolve :auto-fire)
                            :yes-ability
                            {:async true
                             :effect (req (wait-for (reveal state side (:hand runner))
                                                    (system-msg state :corp (str "reveal "
                                                                (quantify (count (:hand runner)) "card")
                                                                " from grip: "
                                                                (enumerate-str (map :title (:hand runner)))))
                                                    (continue-ability state side select-and-trash card nil)))}
                            :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
    {:events [{:event :agenda-scored
               :interactive (req true)
               :async true
               :effect (effect (continue-ability ability card nil))}
              {:event :agenda-stolen
               :interactive (req true)
               :async true
               :effect (effect (continue-ability ability card nil))}]
     :abilities [(set-autoresolve :auto-fire "Vera Ivanovna Shuyskaya")]}))

(defcard "Victoria Jenkins"
  {:on-rez {:effect (req (lose state :runner :click-per-turn 1))}
   :leave-play (req (gain state :runner :click-per-turn 1))
   :on-trash executive-trash-effect})

(defcard "Wage Workers"
  ;; note - for some reason, clicking for a credit and drawing a card register the same on
  ;; the all-events (corp-spent-click)
  ;; this means we need to do some of this the hard way
  (let [payoff {:msg "gain [Click]"
                :effect (effect (gain-clicks 1))}
        event-builder (fn [key]
                        {:event key
                         :async true
                         :req (req (= 3 (count (turn-events state side key))))
                         :effect (req (continue-ability
                                        state side
                                        payoff
                                        card nil))})
        all-events (fn [state side] (turn-events state side :corp-spent-click))
        three-of (fn [cid idx state side]
                   (= 3 (count (filter #(and (= (first (first %)) cid)
                                             (= (last %) idx))
                                       (all-events state side)))))]
    {:events [{:event :corp-spent-click
               :async true
               :effect (req (let [cid (first target)
                                  ability-idx (:ability-idx (:source-info eid))]
                              (if (three-of cid ability-idx state side)
                                (continue-ability state side payoff card nil)
                                (effect-completed state side eid))))}]}))

(defcard "Wall to Wall"
  (let [all [{:msg "gain 1 [Credits]"
              :async true
              :effect (effect (gain-credits eid 1))}
             {:msg "draw 1 card"
              :async true
              :effect (effect (draw eid 1))}
             {:label "place 1 advancement token on a piece of ice"
              :msg (msg "place 1 advancement token on " (card-str state target))
              :prompt "Choose a piece of ice to place 1 advancement token on"
              :async true
              :choices {:card #(and (ice? %)
                                    (installed? %))}
              :cancel-effect (effect (effect-completed eid))
              :effect (effect (add-prop target :advance-counter 1 {:placed true})
                              (effect-completed eid))}
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
                 :label "resolve an ability (start of turn)"
                 :once :per-turn
                 :effect (effect (continue-ability (choice all (if (< 1 (count (filter asset? (all-active-installed state :corp))))
                                                                 1
                                                                 3)) card nil))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Warden Fatuma"
  (let [new-sub {:label "[Warden Fatuma] Force the Runner to lose [Click], if able"}]
    (letfn [(all-rezzed-bios [state]
              (filter #(and (ice? %)
                            (has-subtype? % "Bioroid")
                            (rezzed? %))
                      (all-installed state :corp)))
            (remove-one [cid state ice]
              (remove-extra-subs! state :corp ice cid))
            (add-one [cid state ice]
              (add-extra-sub! state :corp ice new-sub cid {:front true}))
            (update-all [state func]
              (doseq [i (all-rezzed-bios state)]
                (func state i)))]
      {:on-rez {:msg "add \"[Subroutine] The Runner loses [Click], if able\" before all other subroutines"
                :effect (req (update-all state (partial add-one (:cid card))))}
       :leave-play (req (system-msg state :corp "loses Warden Fatuma additional subroutines")
                     (update-all state (partial remove-one (:cid card))))
       :sub-effect {:msg "force the Runner to lose [Click], if able"
                    :effect (req (lose-clicks state :runner 1))}
       :events [{:event :rez
                 :req (req (and (ice? (:card context))
                                (has-subtype? (:card context) "Bioroid")))
                 :effect (req (add-one (:cid card) state (get-card state (:card context))))}]})))

(defcard "Warm Reception"
  (let [install {:prompt "Choose a card to install"
                 :async true
                 :choices {:card #(and (corp-installable-type? %)
                                       (in-hand? %))}
                 :msg (msg (corp-install-msg target))
                 :effect
                 (req (wait-for (corp-install state side (make-eid state eid) target nil nil)
                                (let [installed-card async-result]
                                  (register-turn-flag!
                                    state side
                                    card :can-score
                                    (fn [state _ card]
                                      (if (same-card? card installed-card)
                                        ((constantly false) (toast state :corp "Cannot score due to Warm Reception." "Warning"))
                                        true)))
                                  (effect-completed state side eid))))}
        derez {:label "Derez another card (start of turn)"
               :req (req unprotected)
               :prompt "Choose another card to derez"
               :choices {:not-self true
                         :card #(rezzed? %)}
               :msg (msg "derez itself to derez " (card-str state target))
               :effect (effect (derez card)
                               (derez target))}]
    {:derezzed-events [corp-rez-toast]
     :events [{:event :corp-turn-begins
               :async true
               :effect (req (wait-for (resolve-ability state side install card nil)
                                      (continue-ability state side derez card nil)))}]}))

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
                :cost [:trash-from-hand 1]
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
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:cost [:click 1 :power 1]
                :label "Gain 3 [Credits]"
                :msg "gain 3 [Credits]"
                :effect (req (gain-credits state side eid 3))}
               {:cost [:click 1 :power 5]
                :label "Gain 6 [Credits] and add a resource to the top of the stack"
                :prompt "Choose a resource"
                :choices {:card #(resource? %)}
                :async true
                :msg (msg "gain 6 [Credits] and add " (:title target) " to the top of the stack")
                :effect (req
                          (wait-for (gain-credits state side 6)
                                    (move state :runner target :deck {:front true})
                                    (effect-completed state side eid)))}]})

(defcard "Worlds Plaza"
  {:abilities [{:label "Install an asset on this asset"
                :req (req (< (count (:hosted card)) 3))
                :cost [:click 1]
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
  {:interactions {:prevent [{:type #{:expose}
                             :req (req true)}]}
   :derezzed-events [{:event :pre-expose
                      :async true
                      :effect (req (let [etarget target]
                                     (continue-ability
                                       state side
                                       {:optional
                                        {:req (req (not (rezzed? card)))
                                         :player :corp
                                         :prompt (msg "The Runner is about to expose " (:title etarget) ". Rez Zaibatsu Loyalty?")
                                         :yes-ability {:async true
                                                       :effect (effect (rez eid card))}}}
                                       card nil)))}]
   :abilities [{:msg "prevent 1 card from being exposed"
                :cost [:credit 1]
                :effect (effect (expose-prevent 1))}
               {:msg "prevent 1 card from being exposed"
                :label "Prevent 1 card from being exposed"
                :cost [:trash-can]
                :effect (effect (expose-prevent 1))}]})

(defcard "Zealous Judge"
  {:rez-req (req tagged)
   :abilities [{:async true
                :label "Give the Runner 1 tag"
                :cost [:click 1 :credit 1]
                :keep-menu-open :while-clicks-left
                :msg "give the Runner 1 tag"
                :effect (effect (gain-tags eid 1))}]})
