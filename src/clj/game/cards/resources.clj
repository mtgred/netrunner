(ns game.cards.resources
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [game.core.access :refer [access-bonus access-n-cards breach-server steal
                             num-cards-to-access steal-cost-bonus]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active all-active-installed all-installed
                            all-installed-runner card->server get-all-cards server->zone]]
   [game.core.card :refer [agenda? asset? assoc-host-zones card-index corp? condition-counter?
                           event? facedown? get-agenda-points get-card get-counters
                           get-title get-zone hardware? has-subtype? ice? identity? in-discard? in-hand? in-scored?
                           installed? is-type? program? resource? rezzed? runner? upgrade? virus-program?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.charge :refer [can-charge charge-ability]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.cost-fns :refer [has-trash-ability? install-cost rez-cost
                               trash-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out
                                  reorder-choice trash-on-empty do-net-damage]]
   [game.core.drawing :refer [draw click-draw-bonus]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [not-used-once? pay register-events
                             register-once register-suppress resolve-ability
                             trigger-event trigger-event-sync unregister-events unregister-suppress-by-uuid checkpoint]]
   [game.core.events :refer [event-count first-event?
                             first-installed-trash-own? first-run-event?
                             first-successful-run-on-server? get-turn-damage no-event? second-event? turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [card-flag? clear-persistent-flag!
                            has-flag? in-corp-scored?
                            register-persistent-flag! register-turn-flag! zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [break-sub break-subroutine! get-strength ice-strength pump pump-ice
                          unbroken-subroutines-choice update-all-ice
                          update-all-icebreakers update-breaker-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.installing :refer [install-locked? runner-can-install? runner-can-pay-and-install?
                                 runner-install]]
   [game.core.link :refer [get-link link+]]
   [game.core.mark :refer [identify-mark-ability mark-changed-event is-mark?]]
   [game.core.memory :refer [available-mu]]
   [game.core.moving :refer [as-agenda flip-faceup forfeit mill move
                             remove-from-currently-drawing trash trash-cards
                             trash-prevent]]
   [game.core.optional :refer [get-autoresolve never? set-autoresolve]]
   [game.core.payment :refer [build-spend-msg can-pay? ->c]]
   [game.core.pick-counters :refer [pick-virus-counters-to-spend]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [active-encounter? bypass-ice can-run-server? get-runnable-zones
                           gain-run-credits get-current-encounter
                           update-current-encounter
                           make-run set-next-phase
                           successful-run-replace-breach total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote?
                              protecting-same-server? remote->name target-server unknown->kw
                              zone->name zones->sorted-names]]
   [game.core.set-aside :refer [set-aside get-set-aside set-aside-for-me]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags tag-prevent]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.threat :refer [threat-level]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters number-of-runner-virus-counters]]
   [game.core.winning :refer [check-win-by-agenda]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [jinteki.validator :refer [legal?]]
   [medley.core :refer [find-first]]))

(defn- genetics-trigger?
  "Returns true if Genetics card should trigger - does not work with Adjusted Chronotype"
  [state side event]
  (or (first-event? state side event)
      (and (has-flag? state side :persistent :genetics-trigger-twice)
           (second-event? state side event))))

(defn- shard-constructor
  "Function for constructing a Shard card"
  [title target-server message effect-fn]
  {:events [(assoc
              (successful-run-replace-breach
                {:target-server target-server
                 :ability
                 {:async true
                  :msg (str "install " title ", ignoring all costs")
                  :effect (effect (runner-install eid card {:ignore-all-cost true}))}})
              :location :hand)]
   :abilities [{:async true
                :cost [(->c :trash-can)]
                :msg message
                :effect (effect (effect-fn eid card targets))}]})

(defn companion-builder
  "pay-credits-req says when it can be used. turn-ends-ability defines what happens,
  and requires `effect-completed`."
  [pay-credits-req turn-ends-ability ability]
  (let [place-credit {:msg "add 1 [Credits] to itself"
                      :effect (effect (add-counter card :credit 1))}]
    {:interactions {:pay-credits {:req pay-credits-req
                                  :type :credit}}
     :events [(assoc place-credit :event :runner-turn-begins)
              (assoc place-credit :event :agenda-stolen)
              {:event :runner-turn-ends
               :req (req (<= 3 (get-counters (get-card state card) :credit)))
               :async true
               :effect (effect (continue-ability turn-ends-ability card targets))}]
     :abilities [ability]}))

(defn bitey-boi
  [f]
  (let [selector (resolve f)
        descriptor (str f)]
    {:abilities [{:req (req (and (get-current-encounter state)
                                 (rezzed? current-ice)
                                 (not (:broken (selector (:subroutines current-ice))))))
                  :break 1
                  :breaks "All"
                  :break-cost [(->c :trash-can)]
                  :cost [(->c :trash-can)]
                  :label (str "Break the " descriptor " subroutine")
                  :msg (msg "break the " descriptor " subroutine on " (:title current-ice)
                            " (\"[subroutine] " (:label (selector (:subroutines current-ice))) "\")")
                  :effect (req (break-subroutine! state current-ice (selector (:subroutines current-ice))))}]}))

;; Card definitions

(defcard "Aaron Marrón"
  (let [am {:msg "place 2 power counters on itself"
            :effect (effect (add-counter card :power 2))}]
    {:abilities [{:cost [(->c :power 1)]
                  :keep-menu-open :while-power-tokens-left
                  :msg "remove 1 tag and draw 1 card"
                  :async true
                  :effect (req (wait-for (lose-tags state side 1)
                                         (draw state side eid 1)))}]
     :events [(assoc am :event :agenda-scored)
              (assoc am :event :agenda-stolen)]}))

(defcard "Access to Globalsec"
  {:static-abilities [(link+ 1)]})

(defcard "Activist Support"
  {:events [{:event :corp-turn-begins
             :async true
             :effect (req (if (zero? (count-tags state))
                            (do (gain-tags state :runner eid 1)
                                (system-msg state :runner (str "uses " (:title card) " to take 1 tag")))
                            (effect-completed state :runner eid)))}
            {:event :runner-turn-begins
             :async true
             :effect (req (if (not (has-bad-pub? state))
                            (do (gain-bad-publicity state :corp eid 1)
                                (system-msg state :runner
                                            (str "uses " (:title card) " to give the corp 1 bad publicity")))
                            (effect-completed state :runner eid)))}]})

(defcard "Adjusted Chronotype"
  {:events [{:event :runner-click-loss
             :req (req (let [click-losses (->> (turn-events state side :runner-lose)
                                               (filter #(= :click (:type (first %))))
                                               (count))]
                            (or (= 1 click-losses)
                                (and (= 2 click-losses)
                                     (has-flag? state side :persistent :genetics-trigger-twice)))))
             :msg "gain [Click]"
             :effect (effect (gain-clicks :runner 1))}]})

(defcard "Aeneas Informant"
  {:events [{:event :post-access-card
             :optional
             {:autoresolve (get-autoresolve :auto-fire)
              :req (req (and (:trash (second targets))
                             (not (in-discard? target))))
              :prompt "Gain 1 [Credits] and reveal accessed card?"
              :yes-ability {:msg (msg (str "gain 1 [Credits]"
                                           (when-not (installed? target)
                                             (str " and reveal " (:title target)))))
                            :async true
                            :effect (effect (gain-credits eid 1))}}}]
   :abilities [(set-autoresolve :auto-fire "Aeneas Informant")]})

(defcard "Aesop's Pawnshop"
  (let [ability {:async true
                 :label "trash a card to gain 3 [Credits]"
                 :once :per-turn
                 :req (req (>= (count (all-installed state :runner)) 2))
                 :choices {:not-self true
                           :req (req (and (runner? target)
                                          (installed? target)))}
                 :msg (msg "trash " (:title target) " and gain 3 [Credits]")
                 :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                        (effect-completed eid))
                 :effect (req (wait-for (trash state side target {:unpreventable true :cause-card card})
                                        (gain-credits state side eid 3)))}]
    {:flags {:runner-phase-12 (req (>= (count (all-installed state :runner)) 2))}
     :events [(assoc ability
                     :event :runner-turn-begins
                     :interactive (req true))]
     :abilities [ability]}))

(defcard "Akshara Sareen"
  {:in-play [:click-per-turn 1]
   :on-install {:msg "give each player 1 additional [Click] to spend during their turn"
                :effect (effect (gain :corp :click-per-turn 1))}
   :leave-play (effect (lose :corp :click-per-turn 1))})

(defcard "Algo Trading"
  {:flags {:runner-phase-12 (req (pos? (:credit runner)))}
   :abilities [{:label "Store up to 3 [Credit]"
                :prompt "How many credits do you want to store?"
                :once :per-turn
                :choices {:number (req (min 3 (total-available-credits state :runner eid card)))}
                :async true
                :effect (effect (add-counter card :credit target)
                                (lose-credits eid target))
                :msg (msg "store " target " [Credit]")}
               {:label "Take all hosted credits"
                :cost [(->c :click 1) (->c :trash-can)]
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (get-counters card :credit)))}]
   :events [{:event :runner-turn-begins
             :req (req (>= (get-counters card :credit) 6))
             :msg "place 2 [Credit] on itself"
             :effect (effect (add-counter card :credit 2))}]})

(defcard "All-nighter"
  {:abilities [{:cost [(->c :click 1) (->c :trash-can)]
                :effect (effect (gain-clicks 2))
                :msg "gain [Click][Click]"}]})

(defcard "Always Be Running"
  {:implementation "Run requirement not enforced"
   :events [{:event :runner-turn-begins
             :effect (req (toast state :runner "Reminder: Always Be Running requires a run on the first click" "info"))}]
   :abilities [(assoc (break-sub [(->c :lose-click 2)] 1 "All" {:req (req true)}) :once :per-turn)]})

(defcard "Amelia Earhart"
  {:flags {:runner-phase-12 (req true)}
   :events [{:event :run-ends
             :req (req (and (#{:hq :rd} (target-server context))
                            (>= (total-cards-accessed context) 3)))
             :msg "add 1 power counter to itself"
             :effect (effect (add-counter (get-card state card) :power 1))}
            {:event :runner-turn-begins
             :optional
             {:prompt "Trash this resource to force the Corp to lose 10 [Credits]?"
              :req (req (>= (get-counters (get-card state card) :power) 3))
              :yes-ability
              {:msg "trash itself and force the Corp to lose 10 [Credits]"
               :async true
               :effect (req (wait-for
                              (trash state side card {:cause-card card})
                              (lose-credits state :corp eid (min 10 (:credit corp)))))}}}]})

(defcard "Angel Arena"
  {:on-install {:prompt "How many credits do you want to spend?"
                :choices :credit
                :msg (msg "place " (quantify target "power counter") " on itself")
                :effect (effect (add-counter card :power target))}
   :events [(trash-on-empty :power)]
   :abilities [{:cost [(->c :power 1)]
                :keep-menu-open :while-power-tokens-left
                :msg "look at the top card of Stack"
                :optional
                {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                 :yes-ability
                 {:msg "add the top card of Stack to the bottom"
                  :effect (req (move state side (first (:deck runner)) :deck))}}}]})

(defcard "Armitage Codebusting"
  {:data {:counter {:credit 12}}
   :events [(trash-on-empty :credit)]
   :abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :label "gain 2 [Credits]"
                :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 2 (get-counters card :credit))]
                               (add-counter state side card :credit (- credits))
                               (gain-credits state :runner eid credits)))}]})

(defcard "Artist Colony"
  {:abilities [{:prompt "Choose a card to install"
                :label "install a card"
                :msg (msg "install " (:title target))
                :req (req (not (install-locked? state side)))
                :cost [(->c :forfeit)]
                :choices (req (cancellable (filter #(not (event? %)) (:deck runner)) :sorted))
                :async true
                :effect (effect (trigger-event :searched-stack)
                                (shuffle! :deck)
                                (runner-install eid target nil))}]})

(defcard "Arruaceiras Crew"
  {:abilities [{:req (req (active-encounter? state))
                :cost [(->c :gain-tag 1)]
                :once :per-turn
                :label "Give encountered ice -2 strength"
                :msg (msg "give " (card-str state current-ice) " -2 strength for the remainder of the encounter")
                :effect (effect (pump-ice current-ice -2 :end-of-encounter))}
               {:label "Trash encountered ice"
                :async true
                :req (req (and (active-encounter? state)
                               (not (pos? (ice-strength state side current-ice)))))
                :cost [(->c :credit 2) (->c :trash-can)]
                :effect (effect (trash eid current-ice {:cause-card card}))
                :msg (msg "trash " (card-str state current-ice))}]})

(defcard "Asmund Pudlat"
  (letfn [(search-and-host [x]
            {:prompt (msg "Choose a virus or weapon card (" x " remaining)")
             :choices
             (req (let [hosted-cards (:hosted (get-card state card))
                        not-hosted? (fn [c] (not-any? #(= (:title %) (:title c)) hosted-cards))]
                    (cancellable (filter #(and (not-hosted? %)
                                               (or (has-subtype? % "Virus")
                                                   (has-subtype? % "Weapon")))
                                         (:deck runner)) :sorted)))
             :async true
             :waiting-prompt true
             :msg (msg "host " (get-title target) " on itself")
             :effect (req (host state side card target)
                          (if (> x 1)
                            (continue-ability state side (search-and-host (dec x)) card nil)
                            (effect-completed state side eid)))})
          (trash-if-empty [state side eid card]
            (if-not (empty? (:hosted (get-card state card)))
              (effect-completed state side eid)
              (do (system-msg state side (str "trashes " (get-title card)))
                  (trash state side eid card {:unpreventable true :source-card card}))))]
    {:on-install {:msg "shuffle the stack"
                  :async true
                  :effect (req (wait-for (resolve-ability state side
                                                          (make-eid state eid) 
                                                          (search-and-host 2)
                                                          card nil)
                                         (trigger-event state side :searched-stack)
                                         (shuffle! state side :deck)
                                         (effect-completed state side eid)))}
      :events [{:event :runner-turn-begins
                :label "Add a hosted card to the grip (start of turn)"
                :prompt "Choose a hosted card to move to the grip"
                :choices {:req (req (same-card? card (:host target)))}
                :msg (msg "add " (get-title target) " to the grip")
                :once :per-turn
                :cancel-effect (req (system-msg state side (str "declines to use " (get-title card)))
                                    (trash-if-empty state side eid card))
                :async true
                :waiting-prompt true
                :effect (req (move state side target :hand)
                             (trash-if-empty state side eid card))}]}))

(defcard "Assimilator"
  {:abilities [{:label "Turn a facedown card faceup"
                :cost [(->c :click 2)]
                :keep-menu-open :while-2-clicks-left
                :prompt "Choose a facedown installed card"
                :choices {:card #(and (facedown? %)
                                      (installed? %)
                                      (runner? %))}
                :async true
                :msg (msg "turn " (:title target) " faceup")
                :effect (req (if (event? target)
                               (trash state side eid target {:unpreventable true})
                               ;; Other cards are moved to rig and have events wired.
                               (do (flip-faceup state side target)
                                   (checkpoint state nil eid))))}]})

(defcard "Avgustina Ivanovskaya"
  {:events [{:event :runner-install
             :req (req (and (virus-program? (:card context))
                            (first-event? state side :runner-install #(virus-program? (:card (first %))))))
             :async true
             :effect (effect
                       (continue-ability
                         (sabotage-ability 1)
                         card nil))}]})

(defcard "Backstitching"
  ;; only fire the event for one backstitching per encounter (otherwise you have to press no 3x)
  (letfn [(is-min-index [state card]
            (let [stitches (filterv #(= (:title card) (:title %))
                                    (all-active-installed state :runner))
                  ordered (sort-by :index stitches)]
              (= (:index (first ordered)) (:index card))))]
    {:events [mark-changed-event
              (assoc identify-mark-ability :event :runner-turn-begins)
              {:event :encounter-ice
               :async true
               :interactive (req true)
               :optional
               {:prompt (msg "Trash " (:title card) " to bypass " (:title current-ice) "?")
                :req (req (and (is-min-index state card)
                               (= (:mark @state) (first (:server run)))))
                :yes-ability {:msg (msg "bypass " (:title current-ice))
                              :async true
                              :effect (req (wait-for (trash state side card {:cause-card card :cause :runner-ability})
                                                     (bypass-ice state)
                                                     (effect-completed state side eid)))}}}]}))

(defcard "\"Baklan\" Bochkin"
  {:events [{:event :encounter-ice
             :req (req (first-run-event? state side :encounter-ice))
             :msg "place 1 power counter on itself"
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:label "Derez a piece of ice currently being encountered"
                :msg "derez a piece of ice currently being encountered and take 1 tag"
                :req (req (and (get-current-encounter state)
                               (rezzed? current-ice)
                               (<= (get-strength current-ice) (get-counters (get-card state card) :power))))
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (derez current-ice)
                                (gain-tags eid 1))}]})

(defcard "Bank Job"
  {:data {:counter {:credit 8}}
   :events [(trash-on-empty :credit)
            (successful-run-replace-breach
              {:target-server :remote
               :ability
               {:async true
                :effect (req (letfn [(select-credits-ability [bj]
                                       {:prompt "How many hosted credits do you want to take?"
                                        :choices {:number (req (get-counters (get-card state bj) :credit))}
                                        :msg (msg "gain " target " [Credits]")
                                        :async true
                                        :effect (effect (add-counter :runner bj :credit (- target))
                                                        (gain-credits :runner eid target))})]
                               (if (< 1 (count (filter #(= (:title %) "Bank Job") (all-active-installed state :runner))))
                                 (continue-ability
                                   state side
                                   {:async true
                                    :prompt (msg "Choose a copy of " (:title card) " to use")
                                    :choices {:card #(and (installed? %)
                                                          (= (:title %) (:title card)))}
                                    :effect (effect (continue-ability (select-credits-ability target) target nil))}
                                   card nil)
                                 (continue-ability state side (select-credits-ability card) card nil))))}})]})

(defcard "Bazaar"
  (letfn [(hardware-and-in-hand? [target runner]
            (and (hardware? target)
                 (some #(same-card? :title % target) (:hand runner))))]
    {:events [{:event :runner-install
               :interactive (req (hardware-and-in-hand? (:card context) runner))
               :silent (req (not (hardware-and-in-hand? (:card context) runner)))
               :async true
               :req (req (and (hardware? (:card context))
                              (= [:hand] (:previous-zone (:card context)))))
               :effect (effect (continue-ability
                                 (let [hw (:title (:card context))]
                                   {:optional
                                    {:req (req (some #(when (= (:title %) hw) %) (:hand runner)))
                                     :prompt (msg "Install another copy of " hw "?")
                                     :yes-ability
                                     {:async true
                                      :msg (msg "install another copy of " hw)
                                      :effect (req (if-let [c (some #(when (= (:title %) hw) %) (:hand runner))]
                                                     (runner-install state side eid c nil)
                                                     (effect-completed state side eid)))}}})
                                 card nil))}]}))

(defcard "Beach Party"
  {:static-abilities [(runner-hand-size+ 5)]
   :events [{:event :runner-turn-begins
             :msg "lose [Click]"
             :effect (effect (lose-clicks 1))}]})

(defcard "Beatriz Friere Gonzalez"
  {:abilities [{:cost [(->c :click 2)]
                :msg "Make a run on HQ"
                :makes-run true
                :async true
                :effect (req (make-run state :runner eid :hq card))}]
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability {:msg "breach R&D, accessing 1 additional card"
                         :async true
                         :effect (req (register-events
                                        state side
                                        card [(breach-access-bonus :rd 1 {:duration :end-of-run})])
                                      (breach-server state :runner eid [:rd] nil))}})]})

(defcard "Beth Kilrain-Chang"
  (let [ability {:once :per-turn
                 :label "Gain 1 [Credits], draw 1 card, or gain [Click] (start of turn)"
                 :req (req (:runner-phase-12 @state))
                 :async true
                 :effect (req (let [c (:credit corp)
                                    b (:title card)]
                                (cond
                                  ;; gain 1 credit
                                  (<= 5 c 9)
                                  (do (system-msg state side (str "uses " b " to gain 1 [Credits]"))
                                      (gain-credits state side eid 1))
                                  ;; draw 1 card
                                  (<= 10 c 14)
                                  (do (system-msg state side (str "uses " b " to draw 1 card"))
                                      (draw state side eid 1))
                                  ;; gain 1 click
                                  (<= 15 c)
                                  (do (system-msg state side (str "uses " b " to gain [Click]"))
                                      (gain-clicks state side 1)
                                      (effect-completed state side eid))
                                  :else (effect-completed state side eid))))}]
    {:flags {:drip-economy true}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)]}))

(defcard "Bhagat"
  {:events [{:event :successful-run
             :async true
             :req (req (and (= :hq (target-server context))
                            (first-successful-run-on-server? state :hq)))
             :msg "force the Corp to trash the top card of R&D"
             :effect (req (mill state :corp eid :corp 1))}]})

(defcard "Bio-Modeled Network"
  {:interactions {:prevent [{:type #{:net}
                             :req (req true)}]}
   :events [{:event :pre-damage
             :req (req (= (:type context) :net))
             :effect (effect (update! (assoc card :dmg-amount (:card context))))}]
   :abilities [{:msg (msg "prevent " (dec (:dmg-amount card)) " net damage")
                :label "prevent net damage"
                :cost [(->c :trash-can)]
                :effect (effect (damage-prevent :net (dec (:dmg-amount card))))}]})

(defcard "Biometric Spoofing"
  {:interactions {:prevent [{:type #{:net :brain :meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent 2 damage"
                :msg "prevent 2 damage"
                :cost [(->c :trash-can)]
                :effect (effect (damage-prevent :brain 2)
                                (damage-prevent :net 2)
                                (damage-prevent :meat 2))}]})

(defcard "Blockade Runner"
  {:abilities [{:cost [(->c :click 2)]
                :keep-menu-open :while-2-clicks-left
                :msg "draw 3 cards and shuffle 1 card from the grip back into the stack"
                :async true
                :effect (req (wait-for (draw state side 3)
                                       (continue-ability
                                         state side
                                         {:prompt "Choose a card in the grip to shuffle back into the stack"
                                          :choices {:card #(and (in-hand? %)
                                                                (runner? %))}
                                          :effect (effect (move target :deck)
                                                          (shuffle! :deck))}
                                         card nil)))}]})

(defcard "Bloo Moose"
  (let [ability {:req (req (not (zone-locked? state :runner :discard)))
                :label "Remove a card in the Heap from the game to gain 2 [Credits]"
                :once :per-turn
                :prompt "Choose a card in the Heap"
                :show-discard true
                :choices {:card #(and (in-discard? %)
                                      (runner? %))}
                :msg (msg "remove " (:title target) " from the game and gain 2 [Credits]")
                :async true
                :effect (effect (move target :rfg)
                                (gain-credits eid 2))}]
  {:flags {:runner-phase-12 (req (not (zone-locked? state :runner :discard)))}
   :events [(assoc ability
                   :event :runner-turn-begins
                   :interactive (req true))]
   :abilities [ability]}))

(defcard "Borrowed Satellite"
  {:static-abilities [(link+ 1)
                      (runner-hand-size+ 1)]})

(defcard "Bug Out Bag"
  {:on-install {:prompt "How many credits do you want to spend?"
                :choices :credit
                :msg (msg "place " (quantify target "power counter") " on itself")
                :effect (effect (add-counter card :power target))}
   :events [{:event :runner-turn-ends
             :req (req (zero? (count (:hand runner))))
             :msg (msg "draw " (quantify (get-counters card :power) "card"))
             :async true
             :effect (req (wait-for (draw state side (get-counters card :power))
                                    (trash state side eid card {:cause-card card})))}]})

(defcard "Caldera"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [(->c :credit 3)]
                :msg "prevent 1 net damage"
                :effect (effect (damage-prevent :net 1))}
               {:cost [(->c :credit 3)]
                :msg "prevent 1 core damage"
                :effect (effect (damage-prevent :brain 1))}]})

(defcard "Charlatan"
  {:abilities [{:cost [(->c :click 2)]
                :label "Make a run"
                :prompt "Choose a server"
                :choices (req runnable-servers)
                :msg (msg "make a run on " target)
                :async true
                :effect
                (effect
                  (register-events
                    card
                    [{:event :approach-ice
                      :unregister-once-resolved true
                      :duration :end-of-run
                      :optional
                      {:prompt (msg "Pay " (get-strength (:ice context)) " [Credits] to bypass encountered piece of ice?")
                       :req (req (and (rezzed? (:ice context))
                                      (first-run-event? state side :approach-ice
                                                        (fn [targets]
                                                          (let [context (first targets)]
                                                            (rezzed? (:ice context)))))
                                      (can-pay? state :runner eid card nil
                                                [(->c :credit (get-strength (:ice context)))])))
                       :yes-ability
                       {:async true
                        :effect (req (let [ice (:ice context)]
                                       (wait-for (pay state :runner (make-eid state eid) card [(->c :credit (get-strength ice))])
                                                 (if-let [payment-str (:msg async-result)]
                                                   (do (system-msg state :runner
                                                                   (str (build-spend-msg payment-str "use")
                                                                        (:title card) " to bypass " (:title ice)))
                                                       (register-events
                                                         state :runner card
                                                         [{:event :encounter-ice
                                                           :req (req (and (same-card? ice (:ice context))
                                                                          (rezzed? (:ice context))))
                                                           :effect (req (bypass-ice state))}])
                                                       (effect-completed state side eid))
                                                   (do (system-msg state :runner
                                                                   (str "can't afford to pay to bypass " (:title ice)))
                                                       (effect-completed state side eid))))))}}}])
                  (make-run eid target card))}]})

(defcard "Chatterjee University"
  {:abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :label "Place 1 power counter"
                :msg "place 1 power counter on itself"
                :effect (effect (add-counter card :power 1))}
               {:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :label "Install a program from the grip"
                :prompt "Choose a program to install"
                :choices
                {:async true
                 :req (req (and (program? target)
                                (in-hand? target)
                                (can-pay? state :runner (assoc eid :source card :source-type :runner-install) target nil
                                          [(->c :credit (install-cost state side target
                                                                      {:cost-bonus (- (get-counters card :power))}))])))}
                :msg (msg "install " (:title target) " from the grip")
                :effect (req (wait-for (runner-install state side target {:cost-bonus (- (get-counters card :power))})
                                       (when (pos? (get-counters card :power))
                                         (add-counter state side card :power -1))
                                       (effect-completed state side eid)))}]})

(defcard "Chrome Parlor"
  {:events [{:event :pre-damage
             :req (req (has-subtype? (:card context) "Cybernetic"))
             :effect (effect (damage-prevent (:type context) Integer/MAX_VALUE))}]})

(defcard "Citadel Sanctuary"
  {:interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent all meat damage"
                :msg "prevent all meat damage"
                :cost [(->c :trash-can) (->c :trash-entire-hand)]
                :effect (effect (damage-prevent :meat Integer/MAX_VALUE))}]
   :events [{:event :runner-turn-ends
             :interactive (req true)
             :msg "force the Corp to initiate a trace"
             :label "Trace 1 - If unsuccessful, Runner removes 1 tag"
             :trace {:base 1
                     :req (req tagged)
                     :unsuccessful {:msg "remove 1 tag"
                                    :async true
                                    :effect (effect (lose-tags :runner eid 1))}}}]})

(defcard "Clan Vengeance"
  {:events [{:event :damage
             :req (req (pos? (:amount context)))
             :msg "place 1 power counter on itself"
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:label "Trash 1 random card from HQ for each hosted power counter"
                :async true
                :req (req (pos? (get-counters card :power)))
                :cost [(->c :trash-can)]
                :msg (msg "trash " (quantify (min (get-counters card :power) (count (:hand corp))) "card")
                          " from HQ")
                :effect (effect (trash-cards eid (take (min (get-counters card :power) (count (:hand corp)))
                                                       (shuffle (:hand corp))) {:cause-card card}))}]})

(defcard "Climactic Showdown"
  (letfn [(iced-servers [state side eid card]
            (filter #(-> (get-in @state (cons :corp (server->zone state %))) :ices count pos?)
                    (zones->sorted-names (get-runnable-zones state side eid card nil))))
          (trash-or-bonus [chosen-server]
            {:player :corp
             :prompt "Choose a piece of ice to trash"
             :choices {:card #(and (= (last (get-zone %)) :ices)
                                   (= chosen-server (rest (butlast (get-zone %)))))}
             :async true
             :effect (effect (system-msg (str "trashes " (card-str state target)))
                             (trash :corp eid target {:unpreventable true :cause-card card :cause :forced-to-trash}))
             :cancel-effect (effect (system-msg (str "declines to trash a piece of ice protecting " (zone->name chosen-server)))
                                    (register-events
                                      :runner card
                                      [{:event :breach-server
                                        :duration :until-runner-turn-ends
                                        :req (req (#{:hq :rd} target))
                                        :once :per-turn
                                        :msg (msg "access 2 additional cards from " (zone->name target))
                                        :effect (effect (access-bonus :runner target 2))}]))})]
    {:events [{:event :runner-turn-begins
               :async true
               :effect (req (let [card (move state side card :rfg)]
                              (continue-ability
                                state side
                                (if (pos? (count (iced-servers state side eid card)))
                                  {:prompt "Choose a server"
                                   :choices (req (iced-servers state side eid card))
                                   :msg (msg "choose " (zone->name (unknown->kw target))
                                             " and remove itself from the game")
                                   :effect (effect (continue-ability
                                                     :corp
                                                     (trash-or-bonus (rest (server->zone state target)))
                                                     card nil))}
                                  {:msg "remove itself from the game"})
                                card nil)))}]}))

(defcard "Compromised Employee"
  {:recurring 1
   :events [{:event :rez
             :req (req (ice? (:card context)))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}]
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "Cookbook"
  {:events [{:event :runner-install
             :interactive (req true)
             :optional {:prompt "Place 1 virus counter?"
                        :req (req (has-subtype? (:card context) "Virus"))
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability {:msg (msg "place 1 virus counter on " (card-str state (:card context)))
                                      :effect (effect (add-counter (:card context) :virus 1))}}}]
   :abilities [(set-autoresolve :auto-fire "Cookbook")]})

(defcard "Corporate Defector"
  {:events [{:event :corp-click-draw
             :msg (msg "force the Corp to reveal that they drew " (:title (:card context)))
             :async true
             :effect (effect (reveal eid (:card context)))}]})

(defcard "Councilman"
  {:events [{:event :rez
             :req (req (and (or (asset? (:card context))
                                (upgrade? (:card context)))
                            (can-pay? state :runner (assoc eid :source card :source-type :ability)
                                      card nil
                                      [(->c :credit (rez-cost state :corp (:card context)))])))
             :async true
             :effect
             (effect
               (continue-ability
                 {:optional
                  {:player :runner
                   :waiting-prompt true
                   :prompt (msg "Trash " (:title card) " and pay " (rez-cost state :corp (:card context))
                                " [Credits] to derez " (:title (:card context)) "?")
                   :yes-ability
                   {:cost [(->c :credit (rez-cost state :corp (:card context)))]
                    :msg (msg "derez " (:title (:card context))
                              " and prevent it from being rezzed this turn")
                    :async true
                    :effect (req (wait-for (trash state side card {:cause-card card})
                                           (when-not (get-card state card)
                                             (derez state :runner (:card context))
                                             (register-turn-flag!
                                               state side card :can-rez
                                               (fn [state _ card]
                                                 (if (same-card? card (:card context))
                                                   ((constantly false)
                                                    (toast state :corp "Cannot rez the rest of this turn due to Councilman"))
                                                   true))))
                                           (effect-completed state side eid)))}}}
                 card targets))}]})

(defcard "Counter Surveillance"
  (let [ability (successful-run-replace-breach
                  {:mandatory true
                   :duration :end-of-run
                   :ability
                   {:async true
                    :effect (req (let [tags (count-tags state)]
                                   (if (<= tags (total-available-credits state :runner eid card))
                                     ;; Can pay, do access
                                     (continue-ability
                                       state :runner
                                       {:async true
                                        :cost [(->c :credit tags)]
                                        :msg (msg "access up to " (quantify tags "card") " from " (zone->name (:server run)))
                                        :effect (req (continue-ability
                                                      state :runner
                                                      {:async true
                                                       :prompt "How many cards do you want to access?"
                                                       :waiting-prompt true
                                                       :choices {:number (req tags)
                                                                 :default (req tags)}
                                                       :effect (effect (access-n-cards eid (:server run) target))}
                                                      card nil))}
                                       card targets)
                                     ;; Can't pay, don't access cards
                                     (do (system-msg state :runner (str "could not afford to use " (:title card)))
                                         (effect-completed state nil eid)))))}})]
    {:abilities [{:cost [(->c :click 1) (->c :trash-can)]
                  :label "Run a server"
                  :makes-run true
                  :prompt "Choose a server"
                  :msg (msg "make a run on " target)
                  :choices (req runnable-servers)
                  :async true
                  :effect (effect (register-events card [ability])
                                  (make-run eid target card))}]}))

(defcard "Crash Space"
  {:interactions {:prevent [{:type #{:meat}
                             :req (req true)}]
                  :pay-credits {:req (req (or (= :remove-tag (:source-type eid))
                                              (and (same-card? (:source eid) (:basic-action-card runner))
                                                   (= 5 (:ability-idx (:source-info eid))))))
                                :type :recurring}}
   :recurring 2
   :abilities [{:label "Trash to prevent up to 3 meat damage"
                :msg "prevent up to 3 meat damage"
                :cost [(->c :trash-can)]
                :effect (effect (damage-prevent :meat 3))}]})

(defcard "Crowdfunding"
  (let [ability {:async true
                 :once :per-turn
                 :label "Take 1 [Credits] (start of turn)"
                 :msg "gain 1 [Credits]"
                 :req (req (:runner-phase-12 @state))
                 :effect (req (add-counter state side card :credit -1)
                              (wait-for (gain-credits state :runner 1)
                                        (if (not (pos? (get-counters (get-card state card) :credit)))
                                          (wait-for (trash state :runner card {:unpreventable true :cause-card card})
                                                    (system-msg state :runner (str "trashes Crowdfunding"
                                                                                   (when (seq (:deck runner))
                                                                                     " and draws 1 card")))
                                                    (draw state :runner eid 1))
                                          (effect-completed state side eid))))}]
    {:data {:counter {:credit 3}}
     :flags {:drip-economy true
             :runner-turn-draw (req (= 1 (get-counters (get-card state card) :credit)))
             :runner-phase-12 (req (= 1 (get-counters (get-card state card) :credit)))}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-turn-ends
               :async true
               :location :discard
               :req (req (runner-can-install? state side card nil))
               :effect (effect
                         (continue-ability
                           {:optional
                            {:req (req (and (<= 3 (count (:successful-run runner-reg)))
                                            (not (get-in @state [:runner :register :crowdfunding-prompt]))))
                             :player :runner
                             :prompt "Install Crowdfunding from the Heap?"
                             :yes-ability {:async true
                                           :effect (effect (runner-install :runner eid card {:ignore-all-cost true}))}
                             ;; Add a register to note that the player was already asked about installing,
                             ;; to prevent multiple copies from prompting multiple times.
                             :no-ability {:effect (req (swap! state assoc-in [:runner :register :crowdfunding-prompt] true))}}}
                           card nil))}]}))

(defcard "Crypt"
  {:events [{:event :successful-run
             :silent (req true)
             :optional {:prompt (msg "Place 1 virus counter on " (:title card) "?")
                        :req (req (= :archives (target-server context)))
                        :autoresolve (get-autoresolve :auto-place-counter)
                        :yes-ability {:msg "place 1 virus counter on itself"
                                      :effect (effect (add-counter card :virus 1))}}}]
   :abilities [{:async true
                :label "Install a virus program from the stack"
                :prompt "Choose a virus"
                :msg (msg "install " (:title target) " from the stack")
                :choices (req (cancellable (filter #(and (program? %)
                                                         (has-subtype? % "Virus"))
                                                   (:deck runner)) :sorted))
                :cost [(->c :click 1) (->c :virus 3) (->c :trash-can)]
                :effect (effect (trigger-event :searched-stack)
                                (shuffle! :deck)
                                (runner-install (assoc eid :source card :source-type :runner-install) target nil))}
               (set-autoresolve :auto-place-counter "Crypt placing virus counters on itself")]})

(defcard "Cybertrooper Talut"
  {:static-abilities [(link+ 1)]
   :events [{:event :runner-install
             :silent (req true)
             :req (req (and (has-subtype? (:card context) "Icebreaker")
                            (not (has-subtype? (:card context) "AI"))))
             :effect (effect (pump (:card context) 2 :end-of-turn))}]})

(defcard "Dadiana Chacon"
  (let [trash-effect {:async true
                      :req (req (zero? (get-in @state [:runner :credit])))
                      :msg "suffer 3 meat damage"
                      :effect (req (wait-for (trash state :runner card {:unpreventable true :cause-card card})
                                             (damage state :runner eid :meat 3 {:unboostable true :card card})))}]
    {:on-install {:async true
                  :effect (req (if (zero? (get-in @state [:runner :credit]))
                                 (continue-ability state side trash-effect card nil)
                                 (effect-completed state side eid)))}
     :flags {:drip-economy true}
     :events [(assoc trash-effect :event :runner-credit-loss)
              (assoc trash-effect :event :runner-spent-credits)
              {:event :runner-turn-begins
               :once :per-turn
               :interactive (req true)
               :effect
               (effect (continue-ability
                         {:msg "gain 1 [Credits]"
                          :req (req (< (get-in @state [:runner :credit]) 6))
                          :async true
                          :effect (effect (gain-credits :runner eid 1))}
                         card nil))}]}))

(defcard "Daily Casts"
  (let [ability {:once :per-turn
                 :label "Take 2 [Credits] (start of turn)"
                 :req (req (:runner-phase-12 @state))
                 :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                 :async true
                 :effect (req (let [credits (min 2 (get-counters card :credit))]
                                (add-counter state side card :credit (- credits))
                                (gain-credits state :runner eid credits)))}]
    {:data {:counter {:credit 8}}
     :flags {:drip-economy true}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)
              (trash-on-empty :credit)]}))

(defcard "Daeg, First Net-Cat"
  (let [ability {:async true
                 :interactive (req true)
                 :msg "charge"
                 :effect (effect (continue-ability (charge-ability state side eid card) card nil))}]
    {:events [(assoc ability :event :agenda-scored)
              (assoc ability :event :agenda-stolen)]}))

(defcard "Data Dealer"
  {:abilities [{:cost [(->c :click 1) (->c :forfeit)]
                :async true
                :effect (effect (gain-credits eid 9))
                :msg "gain 9 [Credits]"}]})

(defcard "Data Folding"
  (let [ability {:label "Gain 1 [Credits] (start of turn)"
                 :msg "gain 1 [Credits]"
                 :once :per-turn
                 :req (req (and (<= 2 (available-mu state))
                                (:runner-phase-12 @state)))
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:flags {:drip-economy true}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)]}))

(defcard "Data Leak Reversal"
  {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
   :abilities [{:async true
                :req (req tagged)
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :effect (req (mill state :corp eid :corp 1))
                :msg "force the Corp to trash the top card of R&D"}]})

(defcard "DDoS"
  {:abilities [{:msg "prevent the corp from rezzing the outermost piece of ice during a run on any server this turn"
                :cost [(->c :trash-can)]
                :effect (effect
                          (register-turn-flag!
                            card :can-rez
                            (fn [state _ card]
                              (let [idx (card-index state card)]
                                (if (and (ice? card)
                                         idx
                                         (= (count (get-in @state (concat [:corp :servers] (:server (:run @state)) [:ices])))
                                            (inc idx)))
                                  ((constantly false) (toast state :corp "Cannot rez any outermost ice due to DDoS." "warning"))
                                  true)))))}]})

(defcard "Dean Lister"
  {:abilities [{:req (req run)
                :label "pump icebreaker"
                :msg (msg "give +1 strength for each card in their Grip to " (:title target) " until the end of the run")
                :choices {:card #(and (installed? %)
                                      (has-subtype? % "Icebreaker"))}
                :cost [(->c :trash-can)]
                :effect (effect (register-lingering-effect
                                  card
                                  (let [breaker target]
                                    {:type :breaker-strength
                                     :duration :end-of-run
                                     :req (req (same-card? breaker target))
                                     :value (req (count (:hand runner)))}))
                                (update-breaker-strength target))}]})

(defcard "Debbie \"Downtown\" Moreira"
  {:on-install {:req (req (threat-level 4 state))
                :msg "place 2 [Credits] on itself"
                :effect (req (add-counter state side card :credit 2))}
   :events [{:event :play-event
             :req (req (has-subtype? (:card context) "Run"))
             :async true
             :msg "place 1 [Credits] on itself"
             :effect (req (add-counter state side card :credit 1)
                          (effect-completed state side eid))}]
   :abilities [{:msg "take 1 [Credits]"
                :async true
                :req (req (pos? (get-counters (get-card state card) :credit)))
                :effect (req (add-counter state side card :credit -1)
                             (wait-for (gain-credits state side (make-eid state eid) 1)
                                       (trigger-event-sync state side eid :spent-credits-from-card card)))}
               {:label "Run a server"
                :cost [(->c :click 1)]
                :async true
                :makes-run true
                :prompt "Choose a server"
                :choices (req runnable-servers)
                :effect (req (make-run state :runner eid target card))}]
   :interactions {:pay-credits {:req (req (and (get-in card [:special :run-id])
                                               (= (get-in card [:special :run-id]) (:run-id run))))
                                :type :credit}}})

(defcard "Decoy"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :abilities [{:async true
                :cost [(->c :trash-can)]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "District 99"
  (letfn [(eligible-cards [runner]
            (filter #(same-card? :faction (:identity runner) %)
                    (:discard runner)))]
    {:implementation "Place counters manually for programs or pieces of hardware trashed manually (e.g. by being over MU)"
     :abilities [{:label "Add a card from the heap to the grip"
                  :req (req (and (seq (eligible-cards runner))
                                 (not (zone-locked? state :runner :discard))))
                  :cost [(->c :click 1) (->c :power 3)]
                  :prompt "Choose a card to add to grip"
                  :choices (req (eligible-cards runner))
                  :effect (effect (move target :hand))
                  :msg (msg "add " (:title target) " from the heap to the grip")}
                 {:label "Place 1 power counter"
                  :once :per-turn
                  :effect (effect (add-counter card :power 1))
                  :msg "manually place 1 power counter on itself"}]
     :events (let [prog-or-hw (fn [targets]
                                (some #(or (program? (:card %))
                                           (hardware? (:card %)))
                                      targets))
                   trash-event (fn [side-trash]
                                 {:event side-trash
                                  :once :per-turn
                                  :once-per-instance true
                                  :req (req (and (prog-or-hw targets)
                                                 (first-event? state side side-trash prog-or-hw)))
                                  :msg "place 1 power counter on itself"
                                  :effect (effect (add-counter card :power 1))})]
               [(trash-event :corp-trash)
                (trash-event :runner-trash)])}))

(defcard "DJ Fenris"
  (let [is-draft-id? #(str/starts-with? (:code %) "00")
        sorted-id-list (fn [runner format] (->> (server-cards)
                                                (filter #(and (identity? %)
                                                              (has-subtype? % "G-mod")
                                                              (not= (-> runner :identity :faction)
                                                                    (:faction %))
                                                              (not (is-draft-id? %))
                                                              (or (= :casual format)
                                                                  (legal? format :legal %))))
                                                (sort-by :title)))
        fenris-effect {:async true
                       :waiting-prompt true
                       :prompt "Choose a g-mod identity to host"
                       :choices (req (sorted-id-list runner (:format @state)))
                       :msg (msg "host " (:title target))
                       :effect (req (let [card (assoc-host-zones card)
                                          ;; Work around for get-card and update!
                                          c (assoc target :type "Fake-Identity")
                                          c (make-card c)
                                          c (assoc c
                                                   :host (dissoc card :hosted)
                                                   :zone [:onhost]
                                                   ;; semi hack to get deactivate to work
                                                   :installed true)]
                                      ;; Manually host id on card
                                      (update! state side (assoc card :hosted [c]))
                                      (card-init state :runner c)
                                      ;; Clean-up
                                      (effect-completed state side eid)))}]
    {:on-install {:async true
                  :effect (effect (continue-ability fenris-effect card nil))}
     ;; TODO - make this work
     ;; Handle Dr. Lovegood / Malia
     :disable {:effect (req (doseq [hosted (:hosted card)]
                              (disable-card state side hosted)))}
     :reactivate {:effect (req (doseq [hosted (:hosted card)]
                                 (enable-card state side hosted)))}}))

(defcard "Donut Taganes"
  {:static-abilities [{:type :play-cost
                       :value 1}]})

(defcard "Dr. Lovegood"
  {:events [{:event :runner-turn-begins
             :label "blank a card"
             :prompt "Choose an installed card to make its text box blank for the remainder of the turn"
             :once :per-turn
             :interactive (req true)
             :choices {:card installed?}
             :msg (msg "make the text box of " (:title target) " blank for the remainder of the turn")
             :effect (req
                       (let [c target]
                         (add-icon state side card target "DL" (faction-label card))
                         (register-events
                           state side card
                           [{:event :post-runner-turn-ends
                             :unregister-once-resolved true
                             :effect (req (let [disabled-card (get-card state c)]
                                            (remove-icon state side card (get-card state disabled-card))
                                            (fake-checkpoint state)))}])
                         (register-lingering-effect
                           state side card
                           {:type :disable-card
                            :duration :runner-turn-ends
                            :req (req (same-card? c target))
                            :value (req true)})
                         (fake-checkpoint state)))}]})

(defcard "Dr. Nuka Vrolyck"
  {:data {:counter {:power 2}}
   :abilities [{:msg "draw 3 cards"
                :keep-menu-open :while-clicks-left
                :cost [(->c :click 1) (->c :power 1)]
                :async true
                :effect (req (wait-for (draw state :runner 3)
                                       (if (pos? (get-counters (get-card state card) :power))
                                         (effect-completed state side eid)
                                         (trash state :runner eid card {:unpreventable true :cause-card card}))))}]})

(defcard "DreamNet"
  {:events [{:event :successful-run
             :async true
             :req (req (first-event? state :runner :successful-run))
             :msg (msg "draw 1 card"
                       (when (or (<= 2 (get-link state))
                                 (has-subtype? (:identity (:runner @state)) "Digital"))
                         " and gain 1 [Credit]"))
             :effect (req (wait-for (draw state :runner 1)
                                    (if (or (<= 2 (get-link state))
                                            (has-subtype? (:identity (:runner @state)) "Digital"))
                                      (gain-credits state :runner eid 1)
                                      (effect-completed state side eid))))}]})

(defcard "Drug Dealer"
  {:flags {:runner-phase-12 (req (some #(card-flag? % :drip-economy true) (all-active-installed state :runner)))}
   :abilities [{:label "Lose 1 [Credits] (start of turn)"
                :msg (msg (if (zero? (get-in @state [:runner :credit]))
                            "lose 0 [Credits] (Runner has no credits to lose)"
                            "lose 1 [Credits]"))
                :req (req (:runner-phase-12 @state))
                :once :per-turn
                :async true
                :effect (effect (lose-credits eid 1))}]
   :events [{:event :corp-turn-begins
             :msg (msg "draw " (if (zero? (count (get-in @state [:runner :deck])))
                                 "no cards (the stack is empty)"
                                 "1 card"))
             :async true
             :effect (effect (draw :runner eid 1))}
            {:event :runner-turn-begins
             :msg (msg "lose " (if (zero? (get-in @state [:runner :credit]))
                                 "0 [Credits] (Runner has no credits to lose)"
                                 "1 [Credits]"))
             :once :per-turn
             :async true
             :effect (effect (lose-credits eid 1))}]})

(defcard "Duggar's"
  {:abilities [{:cost [(->c :click 4)]
                :keep-menu-open :while-4-clicks-left
                :async true
                :effect (effect (draw eid 10))
                :msg "draw 10 cards"}]})

(defcard "Dummy Box"
  (letfn [(better-name [card-type] (if (= "hardware" card-type) "piece of hardware" card-type))
          (dummy-prevent [card-type]
            {:msg (str "prevent a " (better-name card-type) " from being trashed")
             :async true
             :cost [(->c (keyword (str "trash-" card-type "-from-hand")) 1)]
             :effect (effect (trash-prevent (keyword card-type) 1))})]
    {:interactions {:prevent [{:type #{:trash-hardware :trash-resource :trash-program}
                               :req (req (and (installed? (:prevent-target target))
                                              (not= :runner-ability (:cause target))
                                              (not= :purge (:cause target))))}]}
     :abilities [(dummy-prevent "hardware")
                 (dummy-prevent "program")
                 (dummy-prevent "resource")]}))

(defcard "Earthrise Hotel"
  (let [ability {:msg "draw 2 cards"
                 :once :per-turn
                 :cost [(->c :power 1)]
                 :req (req (:runner-phase-12 @state))
                 :async true
                 :interactive (req true)
                 :effect (req (wait-for (draw state :runner 2)
                                        (if (not (pos? (get-counters (get-card state card) :power)))
                                          (trash state :runner eid card {:unpreventable true :cause-card card})
                                          (effect-completed state side eid))))}]
    {:flags {:runner-turn-draw true
             :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                       (cons (get-in @state [:runner :identity])
                                                             (all-active-installed state :runner))))))}
     :data {:counter {:power  3}}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Eden Shard"
  (shard-constructor "Eden Shard" :rd "force the Corp to draw 2 cards" (effect (draw :corp eid 2))))

(defcard "Emptied Mind"
  (let [ability {:req (req (zero? (count (:hand runner))))
                 :msg "gain [Click]"
                 :label "Gain [Click] (start of turn)"
                 :once :per-turn
                 :effect (effect (gain-clicks 1))}]
    {:events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Enhanced Vision"
  {:events [{:event :successful-run
             :silent (req true)
             :async true
             :effect (req (let [target (first (shuffle (:hand corp)))]
                            (system-msg state :runner (str "uses " (:title card) " to force the Corp to reveal " (:title target) " from HQ"))
                            (reveal state :corp eid target)))
             :req (req (genetics-trigger? state side :successful-run))}]})

(defcard "Environmental Testing"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (and (or (hardware? (:card context))
                                (program? (:card context)))
                            (not (:facedown? context))))
             :async true
             :msg "place 1 power counter on itself"
             :effect (req (add-counter state :runner eid card :power 1 nil))}
            {:event :counter-added
             :async true
             :req (req (<= 4 (get-counters (get-card state card) :power)))
             :msg "gain 9 [Credit]"
             :effect (req (wait-for (trash state side card {:unpreventable :true
                                                            :cause-card card})
                                    (gain-credits state side eid 9)))}]})

(defcard "Eru Ayase-Pessoa"
  (let [constant-ability
        {:event :breach-server
         :req (req (and (threat-level 3 state)
                        (= :rd target)
                        (= :archives (first (:server run)))))
         :msg "access 1 additional card"
         :effect (effect (access-bonus :rd 1))}
        ability
        (successful-run-replace-breach
          {:target-server :archives
           :this-card-run true
           :mandatory true
           :ability {:msg "breach R&D"
                     :async true
                     :effect (req (breach-server state :runner eid [:rd] nil))}})]
    {:events [constant-ability]
     :abilities [{:cost [(->c :click 1)]
                  :msg "make a run on Archives"
                  :label "Take 1 tag and run Archives"
                  :makes-run true
                  :once :per-turn
                  :async true
                  :effect
                  (req (wait-for (gain-tags state :runner 1 {:unpreventable true})
                                 (register-events state side card [ability])
                                 (make-run state side eid :archives (get-card state card))))}]}))

(defcard "Fall Guy"
  {:interactions {:prevent [{:type #{:trash-resource}
                             :req (req true)}]}
   :abilities [{:label "Prevent another installed resource from being trashed"
                :cost [(->c :trash-can)]
                :effect (effect (trash-prevent :resource 1))}
               {:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (gain-credits eid 2))}]})

(defcard "Fan Site"
  {:events [{:event :agenda-scored
             :msg "add itself to their score area as an agenda worth 0 agenda points"
             :req (req (installed? card))
             :effect (req (as-agenda state :runner card 0))}]})

(defcard "Fencer Fueno"
  (companion-builder
    ;; companion-builder: pay-credits-req
    (req (:successful run))
    ;; companion-builder: turn-ends-ability
    {:prompt "Choose one"
     :waiting-prompt true
     :choices (req [(when (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil (->c :credit 1))
                      "Pay 1 [Credits]")
                    "Trash Fencer Fueno"])
     :player :runner
     :msg (msg (if (= target "Trash Fencer Fueno")
                 "trash itself"
                 (decapitalize target)))
     :async true
     :effect (req (if (= target "Trash Fencer Fueno")
                    (trash state :runner eid card {:cause-card card})
                    (pay state :runner eid card (->c :credit 1))))}
    ;; companion-builder: ability
    {:req (req (and (pos? (get-counters (get-card state card) :credit))
                    (:successful run)))
     :msg "take 1 [Credits]"
     :async true
     :effect (effect (add-counter card :credit -1)
                     (gain-run-credits eid 1))}))

(defcard "Fester"
  {:events [{:event :purge
             :msg "force the Corp to lose 2 [Credits]"
             :async true
             :effect (req (if (<= 2 (:credit corp))
                            (lose-credits state :corp eid 2)
                            (effect-completed state side eid)))}]})

(defcard "Film Critic"
  (letfn [(get-agenda [card] (first (filter agenda? (:hosted card))))
          (host-agenda? [agenda]
            {:optional {:prompt (str "Host " (:title agenda) " on Film Critic?")
                        :yes-ability {:effect (req (host state side card agenda)
                                                   (swap! state dissoc :access))
                                      :msg (msg "host " (:title agenda) " instead of accessing it")}}})]
    {:events [{:event :access
               :req (req (and (empty? (filter agenda? (:hosted card)))
                              (agenda? target)))
               :interactive (req true)
               :async true
               :effect (effect (continue-ability (host-agenda? target) card nil))}]
     :abilities [{:cost [(->c :click 2)]
                  :label "Add hosted agenda to your score area"
                  :req (req (get-agenda card))
                  :async true
                  :msg (msg (let [c (get-agenda card)]
                              (str "add " (:title c) " to their score area and gain "
                                   (quantify (get-agenda-points c) "agenda point"))))
                  :effect (req (let [c (move state :runner (get-agenda card) :scored)]
                                 (when (card-flag? c :has-events-when-stolen true)
                                   (card-init state :corp c {:resolve-effect false
                                                             :init-data true}))
                                 (update-all-advancement-requirements state)
                                 (update-all-agenda-points state)
                                 (check-win-by-agenda state side)
                                 (effect-completed state side eid)))}]}))

(defcard "Find the Truth"
  {:implementation "Corporation can click Find the Truth for explicit card reveals"
   :events [{:event :post-runner-draw
             :msg (msg "reveal that they drew "
                       (enumerate-str (map :title runner-currently-drawing)))
             :async true
             :effect (req (let [current-draws runner-currently-drawing]
                            (reveal state side eid current-draws)
                            ;; If the corp wants explicit reveals from FTT, then show a prompt with
                            ;; the card names in it
                            (when  (= (get-in (get-card state card) [:special :explicit-reveal])
                                      :yes)
                              (continue-ability
                               state :corp
                               {:prompt (msg "The runner reveals that they drew "
                                             (enumerate-str (map :title current-draws)))
                                :choices ["OK"]}
                               card nil))))}
            {:event :successful-run
             :interactive (get-autoresolve :auto-peek (complement never?))
             :silent (get-autoresolve :auto-peek never?)
             :optional {:req (req (and (first-event? state side :successful-run)
                                       (-> @state :corp :deck count pos?)))
                        :autoresolve (get-autoresolve :auto-peek)
                        :prompt "Look at the top card of R&D?"
                        :yes-ability {:prompt (req (->> corp :deck first :title (str "The top card of R&D is ")))
                                      :msg "look at the top card of R&D"
                                      :choices ["OK"]}}}]
   :abilities [(set-autoresolve :auto-peek "Find the Truth looking at the top card of R&D")]
   :corp-abilities [{:label "Explicitly reveal drawn cards"
                     :prompt "Explicitly reveal cards the Runner draws?"
                     :choices ["Yes" "No"]
                     :effect (effect (update! (assoc-in card [:special :explicit-reveal](keyword (str/lower-case target))))
                                     (toast (str "From now on, " (:title card) " will "
                                                 (when (= target "No") "Not")
                                                 "explicitly reveal cards the Runner draws")
                                            "info"))}]})

(defcard "First Responders"
  {:abilities [{:cost [(->c :credit 2)]
                :req (req (some corp? (map #(:card (first %)) (turn-events state :runner :damage))))
                :msg "draw 1 card"
                :async true
                :effect (effect (draw eid 1))}]})

(defcard "Friend of a Friend"
  {:abilities [{:label "Gain 5 [Credits] and remove 1 tag"
                :msg "gain 5 [Credits] and remove 1 tag"
                :cost [(->c :click 1) (->c :trash-can)]
                :async true
                :effect (req (wait-for (gain-credits state side (make-eid state eid) 5)
                                       (lose-tags state :runner eid 1)))}
               {:label "Gain 9 [Credits] and take 1 tag"
                :msg "gain 9 [Credits] and take 1 tag"
                :cost [(->c :click 1) (->c :trash-can)]
                :req (req (not tagged))
                :async true
                :effect (req (wait-for (gain-credits state side (make-eid state eid) 9)
                                       (gain-tags state :runner eid 1)))}]})

(defcard "Gang Sign"
  {:events [{:event :agenda-scored
             :async true
             :interactive (req true)
             :msg "breach HQ"
             :effect (req (breach-server state :runner eid [:hq] {:no-root true}))}]})

(defcard "Gbahali"
  (bitey-boi 'last))

(defcard "Gene Conditioning Shoppe"
  {:on-install {:msg "make Genetics trigger a second time each turn"
                :effect (effect (register-persistent-flag! card :genetics-trigger-twice (constantly true)))}
   :leave-play (effect (clear-persistent-flag! card :genetics-trigger-twice))})

(defcard "Ghost Runner"
  {:data {:counter {:credit 3}}
   :abilities [{:msg "gain 1 [Credits]"
                :req (req (and run
                               (pos? (get-counters card :credit))))
                :async true
                :effect (req (add-counter state side card :credit -1)
                             (wait-for (gain-credits state side 1)
                                       (trigger-event-sync state side eid :spent-credits-from-card card)))}]
   :events [(trash-on-empty :credit)]
   :interactions {:pay-credits {:req (req run)
                                :type :credit}}})

(defcard "Globalsec Security Clearance"
  (let [ability {:once :per-turn
                 :label "Lose [Click] and look at the top card of R&D (start of turn)"
                 :req (req (:runner-phase-12 @state))
                 :optional
                 {:prompt "Lose [Click] to look at the top card of R&D?"
                  :waiting-prompt true
                  :autoresolve (get-autoresolve :auto-fire)
                  :yes-ability
                  {:msg "lose [Click] and look at the top card of R&D"
                   :prompt (req (->> corp :deck first :title (str "The top card of R&D is ")))
                   :choices ["OK"]
                   :effect (effect (lose-clicks 1))}}}]
  {:req (req (< 1 (get-link state)))
   :flags {:runner-phase-12 (req true)}
   :abilities [ability (set-autoresolve :auto-fire "Globalsec Security Clearance")]
   :events [(assoc ability
                   :event :runner-turn-begins
                   :interactive (req true))]}))

(defcard "Grifter"
  {:events [{:event :runner-turn-ends
             :async true
             :effect (req (let [ab (if (:successful-run runner-reg)
                                     {:msg "gain 1 [Credits]"
                                      :async true
                                      :effect (effect (gain-credits eid 1))}
                                     {:msg "trash Grifter"
                                      :async true
                                      :effect (effect (trash eid card {:cause :runner-ability :cause-card card}))})]
                            (continue-ability state side ab card targets)))}]})

(defcard "Guru Davinder"
  {:flags {:cannot-pay-net true}
   :events [{:event :pre-damage
             :req (req (and (#{:meat :net} (:type context))
                            (pos? (:amount context))))
             :msg (msg "prevent all " (name (:type context)) " damage")
             :effect (req (damage-prevent state side :meat Integer/MAX_VALUE)
                          (damage-prevent state side :net Integer/MAX_VALUE)
                          (register-events
                            state side card
                            [{:event :pre-resolve-damage
                              :unregister-once-resolved true
                              :async true
                              :msg (msg (if (= target "Trash Guru Davinder")
                                          "trash itself"
                                          (decapitalize target)))
                              :prompt "Choose one"
                              :waiting-prompt true
                              :choices (req [(when (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil (->c :credit 4))
                                               "Pay 4 [Credits]")
                                             "Trash Guru Davinder"])
                              :effect (req (if (= target "Trash Guru Davinder")
                                              (trash state :runner eid card {:cause :runner-ability :cause-card card})
                                              (pay state :runner eid card (->c :credit 4))))}]))}]})


(defcard "Hades Shard"
  (shard-constructor "Hades Shard" :archives "breach Archives"
                     (effect (breach-server eid [:archives] {:no-root true}))))

(defcard "Hannah \"Wheels\" Pilintra"
  {:abilities [{:cost [(->c :click 1)]
                :once :per-turn
                :label "Run a remote server"
                :async true
                :prompt "Choose a remote server"
                :req (req (->> runnable-servers
                               (map unknown->kw)
                               (filter is-remote?)
                               not-empty))
                :choices (req (cancellable
                                (->> runnable-servers
                                     (map unknown->kw)
                                     (filter is-remote?)
                                     (map remote->name))))
                :msg (msg "gain [Click] and make a run on " target)
                :makes-run true
                :effect (req (gain-clicks state side 1)
                             (register-events
                               state side card
                               [{:event :run-ends
                                 :duration :end-of-run
                                 :unregister-once-resolved true
                                 :req (req (and (:unsuccessful context)
                                                (same-card? card (:source-card context))))
                                 :async true
                                 :msg "take 1 tag"
                                 :effect (effect (gain-tags :runner eid 1))}])
                             (make-run state side eid target card))}
               {:cost [(->c :click 1) (->c :trash-can)]
                :async true
                :label "Gain [Click][Click]. Remove 1 tag"
                :effect (effect (gain-clicks 2)
                                (lose-tags eid 1))
                :msg "gain [Click][Click] and remove 1 tag"}]})

(defcard "Hard at Work"
  (let [ability {:msg "gain 2 [Credits] and lose [Click]"
                 :once :per-turn
                 :async true
                 :effect (effect (lose-clicks 1)
                                 (gain-credits eid 2))}]
    {:flags {:drip-economy true}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Hernando Cortez"
  {:static-abilities [{:type :rez-additional-cost
                       :req (req (and (<= 10 (:credit corp))
                                      (ice? target)))
                       :value (req [(->c :credit (count (:subroutines target)))])}]})

(defcard "Human First"
  {:events [{:event :agenda-scored
             :msg (msg "gain " (get-agenda-points (:card context)) " [Credits]")
             :async true
             :effect (effect (gain-credits :runner eid (get-agenda-points (:card context))))}
            {:event :agenda-stolen
             :msg (msg "gain " (get-agenda-points (:card context)) " [Credits]")
             :async true
             :effect (effect (gain-credits :runner eid (get-agenda-points (:card context))))}]})

(defcard "Hunting Grounds"
  (let [ability
        {:label "Prevent a \"When encountered\" ability"
         :once :per-turn
         :msg (msg "prevent the encounter effect on " (card-str state current-ice))
         :effect (req (let [[suppress]
                            (register-suppress
                              state side card
                              (let [ice current-ice]
                                [{:event :encounter-ice
                                  ;; TODO: when suppression is fixed, this should be (:ice context)
                                  :req (req (same-card? ice target))}]))]
                        (register-once state side {:once :per-turn} card) ;; needed for firing in the event handler
                        (register-events
                          state side card
                          [{:event :end-of-encounter
                            :duration :end-of-encounter
                            :unregister-once-resolved true
                            :effect (effect (unregister-suppress-by-uuid (:uuid suppress)))}])))}]
    {:events [{:event :encounter-ice
               :req (req (and (not-used-once? state {:once :per-turn} card)
                              (contains? (card-def current-ice) :on-encounter)))
               :async true
               :effect
               (effect (continue-ability
                         {:eid (assoc eid :source-type :ability)
                          :optional
                          {:prompt (str "Prevent \"when encountered\" effect of " (:title (:ice context)) "?")
                           :yes-ability ability}}
                         card nil))}]
     :abilities [(assoc ability
                        :req (req (and (= :approach-ice (:phase run))
                                       (rezzed? current-ice)
                                       (or (->> (:events @state)
                                                (filter #(and (= :encounter-ice (:event %))
                                                              (same-card? current-ice (:card %))))
                                                seq)
                                           (contains? (card-def current-ice) :on-encounter)))))
                 (letfn [(ri [cards]
                           (when (seq cards)
                             {:async true
                              :effect (req (wait-for (runner-install state side (first cards) {:facedown true})
                                                     (continue-ability state side (ri (rest cards)) card nil)))}))]
                   {:async true
                    :label "Install the top 3 cards of the stack facedown"
                    :msg "install the top 3 cards of the stack facedown"
                    :cost [(->c :trash-can)]
                    :effect (effect (continue-ability (ri (take 3 (:deck runner))) card nil))})]}))

(defcard "Ice Analyzer"
  {:implementation "Credit use restriction is not enforced"
   :events [{:event :rez
             :req (req (ice? (:card context)))
             :msg "place 1 [Credits] on itself"
             :effect (effect (add-counter :runner card :credit 1))}]
   :abilities [{:async true
                :effect (effect (add-counter card :credit -1)
                                (gain-credits eid 1))
                :msg "take 1 hosted [Credits] to install programs"}]
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (program? target)))
                                :type :credit}}})

(defcard "Ice Carver"
  {:static-abilities [{:type :ice-strength
                       :req (req (and (get-current-encounter state)
                                      (same-card? current-ice target)))
                       :value -1}]})

(defcard "Info Bounty"
  {:events [mark-changed-event
            (assoc identify-mark-ability :event :runner-turn-begins)
            {:event :run-ends
              :async true
              :interactive (req true)
              :once :per-turn
              :req (req (first-event? state side :run-ends #(is-mark? state (target-server (first %)))))
              :effect
              (req (if (first-event? state side :end-breach-server #(is-mark? state (:from-server (first %))))
                     (do (system-msg state :runner (str "uses " (:title card) " to gain 2 [Credits]"))
                         (gain-credits state :runner eid 2))
                     (effect-completed state side eid)))}]})

(defcard "Inside Man"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (hardware? target)))
                                :type :recurring}}})

(defcard "Investigative Journalism"
  {:req (req (has-bad-pub? state))
   :abilities [{:cost [(->c :click 4) (->c :trash-can)]
                :msg "give the Corp 1 bad publicity"
                :effect (effect (gain-bad-publicity :corp 1))}]})

(defcard "Investigator Inez Delgado"
  {:abilities [{:msg "add itself to their score area as an agenda worth 0 agenda points"
                :label "Add to score area and reveal cards in server"
                :async true
                :prompt "Choose a server"
                :choices (req remotes)
                :req (req (:stole-agenda runner-reg))
                :effect (req (as-agenda state :runner card 0)
                             (let [zone (server->zone state target)
                                   path (conj (into [(->c :corp)] zone) :content)
                                   cards (enumerate-str (map :title (get-in @state path)))]
                               (wait-for
                                 (reveal state :corp (make-eid state eid) targets)
                                 (system-msg state side
                                             (str "uses " (:title card) " to reveal " cards " from " target))
                                 (effect-completed state side eid))))}]})

(defcard "Jackpot!"
  {:implementation "Credit gain must be manually triggered"
   :events [{:event :runner-turn-begins
             :effect (effect (add-counter :runner card :credit 1))}
            ;; TODO (NoahTheDuke, Oct 2020):
            ;; This is an async ability and it's getting called in `move`, which is sync.
            ;; This won't work long-term, but it's the best solution at the moment.
            {:event :card-moved
             :interactive (req true)
             :optional
             {:req (req (and (in-scored? (:moved-card context))
                             (= :runner (:scored-side (:moved-card context)))))
              :waiting-prompt true
              :prompt (msg "Trash " (:title card) "?")
              :yes-ability
              {:prompt "How many hosted credits do you want to take?"
               :choices {:number (req (get-counters card :credit))}
               :async true
               :effect (req (wait-for (gain-credits state :runner target)
                                      (system-msg state :runner (str "trashes " (:title card) " to gain " target " [Credits]"))
                                      (trash state :runner eid card {:cause-card card})))}}}]})

(defcard "Jak Sinclair"
  (let [ability {:label "Make a run (start of turn)"
                 :prompt "Choose a server"
                 :once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :choices (req runnable-servers)
                 :msg (msg "make a run on " target " during which no programs can be used")
                 :makes-run true
                 :async true
                 :effect (req (wait-for (make-run state :runner (make-eid state eid) target card)
                                        (effect-completed state side eid)))}]
    {:implementation "Doesn't prevent program use"
     :flags {:runner-phase-12 (req true)}
     :install-cost-bonus (req (- (get-link state)))
     :events [{:event :runner-turn-begins
               :interactive (req true)
               :optional
               {:once :per-turn
                :prompt "Make a run?"
                :yes-ability ability}}]
     :abilities [ability]}))

(defcard "Jarogniew Mercs"
  {:on-install {:async true
                :effect (req (wait-for (gain-tags state :runner 1)
                                       (add-counter state :runner card :power (+ 3 (count-tags state)))
                                       (effect-completed state :runner eid)))}
   :events [(trash-on-empty :power)]
   :flags {:untrashable-while-resources true}
   :interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent 1 meat damage"
                :cost [(->c :power 1)]
                :effect (req (damage-prevent state side :meat 1))}]})

(defcard "John Masanori"
  {:events [{:event :successful-run
             :req (req (first-event? state side :successful-run))
             :interactive (req true)
             :msg "draw 1 card"
             :async true
             :effect (effect (draw eid 1))}
            {:event :unsuccessful-run
             :req (req (first-event? state side :unsuccessful-run))
             :async true
             :msg "take 1 tag"
             :effect (effect (gain-tags :runner eid 1))}]})

(defcard "Joshua B."
  (let [ability {:msg "gain [Click]"
                 :once :per-turn
                 :label "Gain [Click] (start of turn)"
                 :effect (effect (gain-clicks 1)
                                 (update! (assoc-in card [:special :joshua-b] true)))}]
    {:flags {:runner-phase-12 (req true)}
     :events [{:event :runner-turn-begins
               :optional {:prompt "Gain [Click]?"
                          :once :per-turn
                          :yes-ability ability
                          :no-ability
                          {:effect (effect (system-msg (str "declines to use " (:title card) " to gain [Click]"))
                                           (update! (assoc-in card [:special :joshua-b] false)))}}}
              {:event :runner-turn-ends
               :interactive (req true)
               :req (req (get-in card [:special :joshua-b]))
               :async true
               :effect (effect (gain-tags eid 1))
               :msg "gain 1 tag"}]
     :abilities [ability]}))

(defcard "Juli Moreira Lee"
  {:data {:counter {:power 4}}
   :events [(trash-on-empty :power)
            {:event :runner-spent-click
             :once :per-turn
             :req (req (let [all-cards (get-all-cards state)]
                         ;; TODO - assert this works when (e.g.) using the last click on lib-acc
                         ;; and having it trashed (all-cards wont find it)
                         ;; also asset that the first-event? fn actually works right...
                         ;; -nbk, mar '24
                         (and (resource? (find-cid (:action target) all-cards))
                              (first-event? state side :runner-spent-click
                                            #(resource?
                                               (find-cid (:action (first %)) all-cards))))))
             :cost [(->c :power 1)]
             :msg "gain [Click]"
             :effect (effect (gain-clicks 1))}]})

(defcard "Kasi String"
  {:events [{:event :run-ends
             :optional
             {:req (req (and (first-event? state :runner :run-ends #(is-remote? (:server (first %))))
                             (not (:did-steal target))
                             (:did-access target)
                             (is-remote? (:server target))))
              :autoresolve (get-autoresolve :auto-place-counter)
              :waiting-prompt true
              :prompt (msg "Place 1 power counter on " (:title card) "?")
              :yes-ability {:msg "place 1 power counter on itself"
                            :async true
                            :effect (req (add-counter state side eid card :power 1 {:placed true}))}
              :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}
            {:event :counter-added
             :req (req (<= 4 (get-counters (get-card state card) :power)))
             :msg "add itself to their score area as an agenda worth 1 agenda point"
             :effect (req (as-agenda state :runner card 1))}]
   :abilities [(set-autoresolve :auto-place-counter "Kasi String placing power counters on itself")]})

(defcard "Kati Jones"
  {:abilities [{:cost [(->c :click 1)]
                :msg "store 3 [Credits]"
                :once :per-turn
                :effect (effect (add-counter card :credit 3))}
               {:cost [(->c :click 1)]
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :once :per-turn
                :label "Take all credits"
                :async true
                :effect (effect (add-counter card :credit (- (get-counters card :credit)))
                                (gain-credits eid (get-counters card :credit)))}]})

(defcard "Keros Mcintyre"
  {:events [{:event :derez
             :req (req (and (first-event? state side :derez)
                            (= (:side context) :runner)))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits eid 2))}]})

(defcard "Kongamato"
  (bitey-boi 'first))

(defcard "Lago Paranoá Shelter"
  {:events [{:event :corp-install
             :optional {:prompt "Trash the top card of the stack?"
                        :waiting-prompt true
                        :req (req (and (not (ice? (:card target)))
                                       (not (condition-counter? (:card target)))
                                       (first-event? state side :corp-install #(and (not (ice? (:card (first %))))
                                                                                    (not (condition-counter? (:card (first %))))))))
                        :yes-ability {:msg (msg (if (seq (:deck runner))
                                                  (str "trash "
                                                       (:title (first (:deck runner)))
                                                       " from the stack and draw 1 card")
                                                  "trash no cards from the stack (it is empty)"))
                                      :async true
                                      :effect (req (wait-for (mill state :runner :runner 1)
                                                             (draw state :runner eid 1)))}
                        :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]})

(defcard "Laguna Velasco District"
  {:events [{:event :runner-click-draw
             :msg "draw 1 additional card"
             :effect (effect (click-draw-bonus 1))}]})

(defcard "Levy Advanced Research Lab"
  (letfn [(lab-keep [cards]
            {:waiting-prompt true
             :prompt "Choose a Program to add to the grip"
             :choices (concat (filter program? cards) ["None"])
             :async true
             :effect (req (if (= "None" target)
                            (system-msg state side (str "declines to use " (get-title card) " to add a program from the top of the stack to the grip"))
                            (do (system-msg state side (str "uses " (get-title card) " to add " (-> target :title) " from the top of the stack to the grip"))
                                (move state side target :hand)))
                          (continue-ability
                            state side
                            (when-let [to-bottom (not-empty (remove #(= % target) cards))]
                              (reorder-choice :runner :corp to-bottom '() (count to-bottom) to-bottom "bottom"))
                            card nil))})]
    {:abilities [{:cost [(->c :click 1)]
                  :keep-menu-open :while-clicks-left
                  :label "Reveal the top 4 cards of the stack"
                  :msg (msg "reveal " (enumerate-str (map :title (take 4 (:deck runner)))) " from the top of the stack")
                  :async true
                  :effect (req (let [from (take 4 (:deck runner))]
                                 (wait-for (reveal state side from)
                                           (continue-ability state side (lab-keep from) card nil))))}]}))

(defcard "Lewi Guilherme"
  (let [ability {:label "lose 1 [Credits] or trash"
                 :interactive (req true)
                 :async true
                 :prompt "Choose one"
                 :waiting-prompt true
                 :choices (req [(when (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil (->c :credit 1))
                                  "Pay 1 [Credits]")
                                "Trash Lewi Guilherme"])
                 :player :runner
                 :msg (msg (if (= target "Trash Lewi Guilherme")
                             "trash itself"
                             (decapitalize target)))
                 :effect (req (if (= target "Trash Lewi Guilherme")
                                (trash state :runner eid card {:cause-card card})
                                (pay state :runner eid card (->c :credit 1))))}]
    {:flags {:drip-economy true}
     :static-abilities [(corp-hand-size+ -1)]
     :events [(assoc ability :event :runner-turn-begins)]}))

(defcard "Liberated Account"
  {:data {:counter {:credit 16}}
   :abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :label "gain 4 [Credits]"
                :msg (msg "gain " (min 4 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (effect (add-counter card :credit (- (min 4 (get-counters card :credit))))
                                (gain-credits eid (min 4 (get-counters card :credit))))}]
   :events [(trash-on-empty :credit)]})

(defcard "Liberated Chela"
  {:abilities [{:cost [(->c :click 5) (->c :forfeit)]
                :msg "add itself to their score area"
                :async true
                :effect
                (effect (continue-ability
                          (if (seq (:scored corp))
                            {:optional
                             {:waiting-prompt true
                              :prompt (msg "Forfeit an agenda to prevent " (:title card) " from being added to Runner's score area?")
                              :player :corp
                              :async true
                              :yes-ability
                              {:player :corp
                               :prompt "Choose an agenda to forfeit"
                               :choices {:card #(in-corp-scored? state side %)}
                               :async true
                               :effect (req (wait-for (forfeit state side (make-eid state eid) target)
                                                      (move state :runner card :rfg)
                                                      (effect-completed state side eid)))}
                              :no-ability
                              {:msg "add itself to their score area as an agenda worth 2 points"
                               :effect (effect (as-agenda :runner card 2))}}}
                            {:msg "add itself to their score area as an agenda worth 2 points"
                             :effect (effect (as-agenda :runner card 2))})
                          card nil))}]})

(defcard "Light the Fire!"
  (let [successful-run-event
        {:event :successful-run
         :duration :end-of-run
         :async true
         :req (req (and run
                        (is-remote? (:server run))))
         :effect (effect (trash-cards eid (:content run-server)))
         :msg "trash all cards in the server for no cost"}
        disable-card-effect
        {:type :disable-card
         :duration :end-of-run
         :req (req (and run
                        (some #{:content} (:zone target))
                        (some #{run-server} (:zone target))))}]
    {:abilities [{:label "Run a remote server"
                  :cost [(->c :click 1) (->c :trash-can) (->c :brain 1)]
                  :prompt "Choose a remote server"
                  :choices (req (cancellable (filter #(can-run-server? state %) remotes)))
                  :msg (msg "make a run on " target " during which cards in the root of the attacked server lose all abilities")
                  :makes-run true
                  :async true
                  :effect (effect
                            (register-events card [successful-run-event])
                            (register-lingering-effect card [disable-card-effect])
                            (make-run eid target card))}]}))

(defcard "Logic Bomb"
  {:abilities [{:label "Bypass the encountered ice"
                :req (req (and (get-current-encounter state)
                               (rezzed? current-ice)))
                :msg (msg "bypass "
                          (:title current-ice)
                          (when (pos? (:click runner))
                            (str " and loses "
                                 (apply str (repeat (:click runner) "[Click]")))))
                :cost [(->c :trash-can)]
                :effect (req (bypass-ice state)
                             (lose-clicks state :runner (:click runner)))}]})

(defcard "London Library"
  {:abilities [{:async true
                :label "Install and host a non-virus program"
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :prompt "Choose a non-virus program in the grip"
                :choices {:card #(and (program? %)
                                      (not (has-subtype? % "Virus"))
                                      (in-hand? %))}
                :msg (msg "install and host " (:title target))
                :effect (effect (runner-install eid target {:host-card card :ignore-install-cost true}))}
               {:label "Add a hosted program to the grip"
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :choices {:req (req (same-card? card (:host target)))}
                :msg (msg "add " (:title target) " to their Grip")
                :effect (effect (move target :hand))}]
   :events [{:event :runner-turn-ends
             :interactive (req true)
             :async true
             :effect (effect (trash-cards eid (filter program? (:hosted card)) {:cause-card card}))}]})

(defcard "Manuel Lattes de Moura"
  {:static-abilities [{:type :basic-ability-additional-trash-cost
                       :req (req (and (same-card? card target)
                                      (= :corp side)
                                      (threat-level 3 state)))
                       :value [(->c :trash-from-hand 1)]}]
   :events [{:event :breach-server
             :req (req (and tagged run
                            (or (= target :rd) (= target :hq))))
             :msg (msg "access 1 additional card from " (zone->name target))
             :effect (effect (access-bonus target 1))}]})

(defcard "\"Pretty\" Mary da Silva"
  {:implementation "only works after other abilities increasing the number of accesses have resolved"
   :events [{:event :breach-server
             :async true
             :interactive (req true)
             :req (req (= :rd target))
             :effect (req
                       (let [num-access (:random-access-limit (num-cards-to-access state side :rd nil))]
                         (continue-ability
                           state side
                           (when (>= num-access 2)
                             {:optional
                              {:prompt "Access 1 additional card?"
                               :yes-ability
                               {:msg "access 1 additional card"
                                :effect (effect (access-bonus :rd 1))}}})
                           card nil)))}]})

(defcard "Maxwell James"
  {:static-abilities [(link+ 1)]
   :abilities [{:req (req (some #{:hq} (:successful-run runner-reg)))
                :prompt "Choose a piece of ice protecting a remote server"
                :choices {:card #(and (ice? %)
                                      (rezzed? %)
                                      (is-remote? (second (get-zone %))))}
                :msg "derez a piece of ice protecting a remote server"
                :cost [(->c :trash-can)]
                :effect (effect (derez target))}]})

(defcard "Miss Bones"
  {:data {:counter {:credit 12}}
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (installed? target)))
                                :type :credit}}
   :abilities [{:prompt "How many hosted credits do you want to take?"
                :label "Take hosted credits"
                :choices {:number (req (get-counters card :credit))}
                :msg (msg "gain " target " [Credits] for trashing installed cards")
                :async true
                :effect (effect (add-counter card :credit (* -1 target))
                                (gain-credits eid target))}]
   :events [(trash-on-empty :credit)]})

(defcard "Motivation"
  (let [ability {:label "Look at the top card of the stack (start of turn)"
                 :req (req (:runner-phase-12 @state))
                 :once :per-turn
                 :optional
                 {:waiting-prompt true
                  :prompt "Look at the top card of the stack?"
                  :autoresolve (get-autoresolve :auto-fire)
                  :yes-ability
                  {:prompt (req (->> runner :deck first :title (str "The top card of the stack is ")))
                   :msg "look at the top card of the stack"
                   :choices ["OK"]}}}]
    {:flags {:runner-turn-draw true
             :runner-phase-12 (req (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner)))}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability (set-autoresolve :auto-fire "Motivation")]}))

(defcard "Mr. Li"
  {:abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :msg "draw 2 cards"
                :async true
                ;; there's no event for when a draw gets prevented, so this will leave a floating
                ;; eid for later draws - we need to unregister the events after the draw to get
                ;; around this (see Genetics Pavilion) - nbkelly, 2022
                :effect (req (register-events
                               state side
                               card
                               [{:event :runner-draw
                                 :unregister-once-resolved true
                                 :duration :end-of-turn
                                 :waiting-prompt true
                                 :prompt "Choose 1 card to add to the bottom of the Stack"
                                 :choices {:req (req (some #(same-card? target %) runner-currently-drawing))}
                                 :msg "add 1 card to the bottom of the Stack"
                                 :effect (effect (move target :deck))}])
                                (wait-for (draw state side 2)
                                          (unregister-events state side card)
                                          (effect-completed state side eid)))}]})

(defcard "Muertos Gang Member"
  {:on-install {:player :corp
                :waiting-prompt true
                :prompt "Choose a card to derez"
                :choices {:card #(and (corp? %)
                                      (not (agenda? %))
                                      (rezzed? %))}
                :effect (effect (derez target))}
   :uninstall
   (effect
     (continue-ability
       {:player :corp
        :waiting-prompt true
        :prompt "Choose a card to rez, ignoring the rez cost"
        :choices {:card (complement rezzed?)}
        :async true
        :effect (effect (system-msg :corp (str "uses " (:title card) " to rez " (:title target) " at no cost"))
                        (rez eid target {:ignore-cost :rez-cost :no-msg true}))}
       card nil))
   :abilities [{:cost [(->c :trash-can)]
                :msg "draw 1 card"
                :async true
                :effect (effect (draw eid 1))}]})

(defcard "Mystic Maemi"
  (companion-builder
    ;; companion-builder: pay-credits-req
   (req (and
         (event? target)
         (or (= 0 (count (:cost-paid eid)))
             (:x-cost eid))
         (= :play (:source-type eid))))
    ;; companion-builder: turn-ends-ability
    {:prompt "Choose one"
     :waiting-prompt true
     :choices (req [(when (can-pay? state :runner
                                 (assoc eid :source card :source-type :ability)
                                 card nil :randomly-trash-from-hand 1)
                       "Trash a random card from the grip")
                     "Trash Mystic Maemi"])
     :player :runner
     :msg (msg (if (= target "Trash Mystic Maemi")
                 "trash itself"
                 (decapitalize target)))
     :async true
     :effect (req (if (= target "Trash Mystic Maemi")
                    (trash state :runner eid card {:cause-card card})
                    (pay state :runner (assoc eid :source card :source-type :ability) card [(->c :randomly-trash-from-hand 1)])))}
    ;; companion-builder: ability
    {:req (req (and (pos? (get-counters (get-card state card) :credit))
                    (:successful run)))
     :msg "take 1 [Credits]"
     :async true
     :effect (effect (add-counter card :credit -1)
                     (gain-run-credits eid 1))}))

(defcard "Net Mercur"
  {:abilities [{:msg "gain 1 [Credits]"
                :async true
                :req (req (pos? (get-counters (get-card state card) :credit)))
                :effect (req (add-counter state side card :credit -1)
                             (wait-for (gain-credits state side (make-eid state eid) 1)
                                       (trigger-event-sync state side eid :spent-credits-from-card card)))}]
   :events [{:event :spent-credits-from-card
             :req (req (and run (has-subtype? target "Stealth")))
             :once :per-run
             :waiting-prompt true
             :prompt "Choose one"
             :player :runner
             :choices ["Place 1 [Credits] on Net Mercur" "Draw 1 card"]
             :async true
             :msg (msg (if (= target "Draw 1 card")
                         (decapitalize target)
                         "place 1 [Credits] on itself"))
             :effect (req (if (= target "Draw 1 card")
                            (draw state side eid 1)
                            (do (add-counter state :runner card :credit 1)
                                (effect-completed state side eid))))}]
   ;; Normally this should be (req true), but having pay-credits prompts on
   ;; literally every interaction would get tiresome. Therefore Net Mercur will
   ;; only ask for payments during a run, traces, psi games, and prevention abilities
   ;; with the release of twinning, net mercur should always ask when spending credits on
   ;; the opponents turn
   :interactions {:pay-credits {:req (req (or
                                            run
                                            (= :corp (:active-player @state))
                                            (#{:psi :trace} (:source-type eid))
                                            (#{:net :meat :brain :tag} (get-in @state [:prevent :current]))))
                                :type :credit}}})

(defcard "Network Exchange"
  {:on-install {:msg "increase the install cost of non-innermost ice by 1"}
   :static-abilities [{:type :install-cost
                       :req (req (ice? target))
                       :value (req (when (pos? (count (:dest-zone (second targets)))) 1))}]})

(defcard "Neutralize All Threats"
  {:events [(breach-access-bonus :hq 1)
            {:event :breach-server
             :req (req (and (= target :archives)
                            (seq (filter :trash (:discard corp)))))
             :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}
            {:event :pre-trash
             :req (req (let [cards (map first (rest (turn-events state side :pre-trash)))]
                         (and (empty? (filter :trash cards))
                              (number? (:trash target)))))
             :once :per-turn
             :msg (msg "reveal " (card-str state target {:visible true}))
             :async true
             :effect (req (swap! state assoc-in [:runner :register :must-trash-with-credits] true)
                          (reveal state side eid target))}
            {:event :post-access-card
             :req (req (get-in @state [:runner :register :must-trash-with-credits]))
             :effect (req (swap! state assoc-in [:runner :register :must-trash-with-credits] false))}]})

(defcard "New Angeles City Hall"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :events [{:event :agenda-stolen
             :async true
             :msg "trash itself"
             :effect (effect (trash eid card {:cause :runner-ability :cause-card card}))}]
   :abilities [{:async true
                :cost [(->c :credit 2)]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "No Free Lunch"
  {:abilities [{:label "Gain 3 [Credits]"
                :msg "gain 3 [Credits]"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (gain-credits eid 3))}
               {:label "Remove 1 tag"
                :msg "remove 1 tag"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (lose-tags :runner eid 1))}]})

(defcard "No One Home"
  (letfn [(first-chance? [state side]
            (< (+ (event-count state side :pre-tag)
                  (event-count state side :pre-damage #(= :net (:type (first %)))))
               2))
          (start-trace [type]
            (let [message (str "avoid any " (if (= type :net)
                                              "amount of net damage"
                                              "number of tags"))]
              {:player :corp
               :label (str "Trace 0 - if unsuccessful, " message)
               :trace {:base 0
                       :unsuccessful {:async true
                                      :msg message
                                      :effect (req (if (= type :net)
                                                     (do (damage-prevent state :runner :net Integer/MAX_VALUE)
                                                         (effect-completed state side eid))
                                                     (tag-prevent state :runner eid Integer/MAX_VALUE)))}}}))]
    {:interactions {:prevent [{:type #{:net :tag}
                               :req (req (first-chance? state side))}]}
     :abilities [{:msg "force the Corp to trace"
                  :async true
                  :effect (req (let [prevent-type (get-in @state [:prevent :current])]
                                 (wait-for (trash state side card {:unpreventable true :cause-card card})
                                           (continue-ability state side (start-trace prevent-type)
                                                             card nil))))}]}))

(defcard "Off-Campus Apartment"
  {:flags {:runner-install-draw true}
   :abilities [{:async true
                :label "Install and host a connection"
                :cost [(->c :click 1)]
                :prompt "Choose a connection in the grip"
                :choices {:req (req (and (has-subtype? target "Connection")
                                         (can-pay? state side (assoc eid :source card :source-type :runner-install)
                                                   target nil [(->c :credit (install-cost state side target))])
                                         (in-hand? target)))}
                :msg (msg "install and host " (:title target) " and draw 1 card")
                :effect (effect (runner-install eid target {:host-card card}))}
               {:label "Host an installed connection (manual)"
                :prompt "Choose an installed connection"
                :choices {:card #(and (has-subtype? % "Connection")
                                      (installed? %))}
                :msg (msg "host " (:title target) " and draw 1 card")
                :async true
                :effect (effect (host card target)
                                (draw eid 1))}]
   :events [{:event :runner-install
             :req (req (same-card? card (:host (:card context))))
             :async true
             :effect (effect (draw eid 1))}]})

(defcard "Officer Frank"
  {:abilities [{:cost [(->c :credit 1) (->c :trash-can)]
                :req (req (find-first #(= :meat (:damage-type (first %))) (turn-events state :runner :damage)))
                :msg "force the Corp to trash 2 random cards from HQ"
                :async true
                :effect (effect (trash-cards :corp eid (take 2 (shuffle (:hand corp))) {:cause-card card}))}]})

(defcard "Oracle May"
  {:abilities [{:cost [(->c :click 1)]
                :label "Name a card type"
                :once :per-turn
                :prompt "Choose one"
                :choices ["Event" "Hardware" "Program" "Resource"]
                :async true
                :effect (req (let [c (first (get-in @state [:runner :deck]))]
                               (system-msg state side (str "uses " (:title card) " to name " target
                                                           " and reveal " (:title c)
                                                           " from the top of the stack"))
                               (wait-for
                                 (reveal state side c)
                                 (if (is-type? c target)
                                   (do (system-msg state side (str "gains 2 [Credits] and draws " (:title c)))
                                       (wait-for (gain-credits state side 2)
                                                 (draw state side eid 1)))
                                   (do (system-msg state side (str "trashes " (:title c)))
                                       (mill state side eid :runner 1))))))}]})

(defcard "Order of Sol"
  (let [ability {:msg "gain 1 [Credits]"
                 :req (req (zero? (:credit runner)))
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:on-install {:async true
                  :effect (effect (continue-ability ability card nil))}
     :events [(assoc ability :event :runner-credit-loss)
              (assoc ability :event :runner-spent-credits)]}))

(defcard "PAD Tap"
  {:events [{:event :corp-credit-gain
             :req (req (and (not= (:action context) :corp-click-credit)
                            (= 1 (->> (turn-events state :corp :corp-credit-gain)
                                      (remove (fn [[context]]
                                                (= (:action context) :corp-click-credit)))
                                      count))))
             :once :per-turn
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}]
   :corp-abilities [{:label "Trash PAD Tap"
                     :async true
                     :cost [(->c :click 1) (->c :credit 3)]
                     :req (req (= :corp side))
                     :effect (effect (system-msg :corp "spends [Click] and 3 [Credits] to trash PAD Tap")
                                     (trash :corp eid card {:cause-card card}))}]})

(defcard "Paige Piper"
  (letfn [(pphelper [title cards]
            {:optional
             {:prompt (str "Search the stack for additional copies of " title "?")
              :yes-ability
              {:prompt (str "How many copies of " title " would you like to get?")
               :choices (take (inc (count cards)) ["0" "1" "2" "3" "4" "5"])
               :msg "shuffle the stack"
               :async true
               :effect (req (let [target (str->int target)]
                              (trigger-event state side :searched-stack)
                              (shuffle! state :runner :deck)
                              (when (pos? target)
                                (system-msg state side (str "adds "
                                                            (quantify target "cop" "y" "ies")
                                                            " of " title " to the heap")))
                              (trash-cards state side eid (take target cards) {:unpreventable true :cause-card card})))}}})]
    {:events [{:event :runner-install
               :interactive (req true)
               :req (req (first-event? state side :runner-install))
               :async true
               :effect (effect (continue-ability
                                 (pphelper (:title (:card context))
                                           (filterv #(= (:title %) (:title (:card context))) (:deck runner)))
                                 card nil))}]}))

(defcard "Paladin Poemu"
  (companion-builder
    ;; companion-builder: pay-credits-req
    (req (and (= :runner-install (:source-type eid))
              (not (has-subtype? target "Connection"))))
    ;; companion-builder: turn-ends-ability
    {:prompt "Choose an installed card to trash"
     :waiting-prompt true
     :choices {:all true
               :card #(and (installed? %)
                           (runner? %))}
     :player :runner
     :msg (msg "trash " (:title target))
     :async true
     :effect (effect (trash eid target {:cause :runner-ability :cause-card card}))}
    ;; companion-builder: ability
    {:req (req (pos? (get-counters (get-card state card) :credit)))
     :msg "take 1 [Credits]"
     :async true
     :effect (effect (add-counter card :credit -1)
                     (gain-credits eid 1))}))

(defcard "Paparazzi"
  {:static-abilities [{:type :is-tagged
                       :val true}]
   :events [{:event :pre-damage
             :req (req (= (:type context) :meat))
             :msg "prevent all meat damage"
             :effect (effect (damage-prevent :meat Integer/MAX_VALUE))}]})

(defcard "Patron"
  (let [ability {:prompt "Choose a server"
                 :label "Choose a server (start of turn)"
                 :choices (req (concat servers ["No server"]))
                 :once :per-turn
                 :req (req (and (:runner-phase-12 @state)
                                (not (used-this-turn? (:cid card) state))))
                 :msg (msg "target " target)
                 :effect (req (when (not= target "No server")
                                (update! state side (assoc card :card-target target))))}]
    {:abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)
              (assoc
                (successful-run-replace-breach
                  {:mandatory true
                   :ability
                   {:msg "draw 2 cards"
                    :async true
                    :effect (effect (draw eid 2))}})
                :req (req (when-let [card (get-card state card)]
                            (and (= (zone->name (:server context)) (:card-target card))
                                 (first-event? state side :successful-run
                                               (fn [targets]
                                                 (let [context (first targets)]
                                                   (= (zone->name (:server context))
                                                      (:card-target card)))))))))
              {:event :runner-turn-ends
               :effect (effect (update! (dissoc (get-card state card) :card-target)))}]}))

(defcard "Paule's Café"
  {:abilities [{:label "Host a program or piece of hardware"
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :choices {:card #(and (#{"Program" "Hardware"} (:type %))
                                      (in-hand? %)
                                      (runner? %))}
                :effect (req (host state side card target))
                :msg (msg "host " (:title target) "")}
               (letfn [(discount [state card]
                         (if (and (= :runner (:active-player @state))
                                  (not (get-in @state [:per-turn (:cid card)])))
                           (->> (all-active-installed state :runner)
                                (filter #(and (resource? %)
                                              (has-subtype? % "Connection")
                                              (:uniqueness %)))
                                count
                                -)
                           0))]
                 {:async true
                  :label "Install hosted card"
                  :msg "install hosted card"
                  :cost [(->c :credit 1)]
                  :req (req (and (seq (:hosted card))
                                 (some #(can-pay? state :runner (assoc eid :source card :source-type :runner-install)
                                                  % nil
                                                  [(->c :credit (install-cost state side % {:cost-bonus (discount state card)}))])
                                       (:hosted card))))
                  :choices {:req (req (and (same-card? card (:host target))
                                           (can-pay? state :runner (assoc eid :source card :source-type :runner-install)
                                                     target nil
                                                     [(->c :credit (install-cost state side target {:cost-bonus (discount state card)}))])))}
                  :effect (req (runner-install
                                 state side
                                 (assoc eid :source card :source-type :runner-install)
                                 target
                                 {:cost-bonus (discount state card)
                                  :custom-message #(str (build-spend-msg % "install") (:title target) " using " (:title card))})
                            (swap! state assoc-in [:per-turn (:cid card)] true))})]})

(defcard "Penumbral Toolkit"
  {:data {:counter {:credit 4}}
   :install-cost-bonus (req (if (some #{:hq} (:successful-run runner-reg)) -2 0))
   :abilities [{:msg "gain 1 [Credits]"
                :req (req (and run (pos? (get-counters card :credit))))
                :async true
                :effect (req (add-counter state side card :credit -1)
                             (wait-for (gain-credits state side 1)
                                       (trigger-event-sync state side eid :spent-credits-from-card card)))}]
   :events [(trash-on-empty :credit)]
   :interactions {:pay-credits {:req (req run)
                                :type :credit}}})

(defcard "Personal Workshop"
  (let [remove-counter
        {:async true
         :req (req (seq (:hosted card)))
         :msg (msg "remove 1 power counter from " (:title target))
         :choices {:card #(:host %)}
         :effect (req (do (add-counter state side target :power -1)
                          (if (pos? (get-counters (get-card state target) :power))
                            (effect-completed state side eid)
                            (runner-install state side eid (dissoc target :counter) {:ignore-all-cost true}))))}]
    {:flags {:drip-economy true}
     :abilities [{:async true
                  :label "Host a program or piece of hardware"
                  :cost [(->c :click 1)]
                  :keep-menu-open :while-clicks-left
                  :prompt "Choose a program or piece of hardware in the grip"
                  :choices {:card #(and (or (program? %)
                                            (hardware? %))
                                        (in-hand? %)
                                        (runner? %))}
                  :effect (req (if (not (pos? (:cost target)))
                                 (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)
                                 (do (host state side card
                                           (assoc target :counter {:power (:cost target)}))
                                     (effect-completed state side eid))))
                  :msg (msg "install and host " (:title target))}
                 (assoc remove-counter
                        :label "Remove 1 power counter from a hosted card"
                        :cost [(->c :credit 1)])
                 {:async true
                  :label "Remove power counters from a hosted card"
                  :choices {:card #(:host %)}
                  :req (req (seq (:hosted card)))
                  :effect (effect
                            (continue-ability
                              (let [paydowntarget target
                                    num-counters (get-counters (get-card state paydowntarget) :power)]
                                {:async true
                                 :prompt "How many power counters do you want to remove?"
                                 :choices {:number (req (min num-counters
                                                             (total-available-credits state :runner eid card)))}
                                 :effect (req (wait-for
                                                (pay state :runner (make-eid state eid) card [(->c :credit target)])
                                                (if-let [payment-str (:msg async-result)]
                                                  (do (system-msg state side
                                                                  (str (build-spend-msg payment-str "use") (:title card)
                                                                       " to remove " (quantify target "power counter")
                                                                       " from " (:title paydowntarget)))
                                                      (if (= num-counters target)
                                                        (runner-install state side (assoc eid :source card :source-type :runner-install) (dissoc paydowntarget :counter) {:ignore-all-cost true})
                                                        (do (add-counter state side paydowntarget :power (- target))
                                                            (effect-completed state side eid))))
                                                  (effect-completed state side eid))))})
                              card nil))}]
     :events [(assoc remove-counter :event :runner-turn-begins)]}))

(defcard "Political Operative"
  {:req (req (some #{:hq} (:successful-run runner-reg)))
   :abilities [{:async true
                :trash-icon true
                :label "Trash a rezzed card"
                :effect
                (effect
                  (continue-ability
                    ;; TODO: Convert this to a cost
                    {:prompt "Choose a rezzed card with a trash cost"
                     :choices {:card #(and (:trash %)
                                           (rezzed? %)
                                           (can-pay? state side (assoc eid :source card :source-type :ability) card nil [(->c :credit (trash-cost state :runner %))]))}
                     :effect (effect
                               (continue-ability
                                 {:async true
                                  :msg (msg "trash " (card-str state target))
                                  :cost [(->c :credit (trash-cost state :runner target)) (->c :trash-can)]
                                  :effect (effect (trash eid target {:cause-card card}))}
                                 card targets))}
                    card nil))}]})

(defcard "Power Tap"
  {:events [{:event :initialize-trace
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}]})

(defcard "Professional Contacts"
  {:abilities [{:cost [(->c :click 1)]
                :msg "gain 1 [Credits] and draw 1 card"
                :async true
                :effect (req (wait-for (gain-credits state side 1)
                                       (draw state side eid 1)))}]})

(defcard "Psych Mike"
  {:events [{:event :run-ends
             :optional {:req (req (and (= :rd (target-server context))
                                       (first-successful-run-on-server? state :rd)
                                       (pos? (total-cards-accessed target :deck))))
                        :prompt "Gain 1 [Credits] for each card you accessed from R&D?"
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability
                        {:msg (msg "gain " (total-cards-accessed target :deck) " [Credits]")
                         :async true
                         :effect (effect (gain-credits :runner eid (total-cards-accessed target :deck)))}}}]
   :abilities [(set-autoresolve :auto-fire "Psych Mike")]})

(defcard "Public Sympathy"
  {:static-abilities [(runner-hand-size+ 2)]})

(defcard "Rachel Beckman"
  {:trash-when-tagged true
   :in-play [:click-per-turn 1]})

(defcard "Raymond Flint"
  {:events [{:event :corp-gain-bad-publicity
             :async true
             :msg "breach HQ"
             :effect (req (breach-server state :runner eid [:hq] {:no-root true}))}]
   :abilities [{:msg "expose 1 card"
                :label "Expose 1 installed card"
                :choices {:card installed?}
                :async true
                :cost [(->c :trash-can)]
                :effect (effect (expose eid target))}]})

(defcard "Reclaim"
  {:abilities
   [{:async true
     :label "Install a program, piece of hardware, or Virtual resource from the heap"
     :req (req (some #(and (or (program? %)
                               (hardware? %)
                               (and (resource? %)
                                    (has-subtype? % "Virtual")))
                           (runner-can-pay-and-install?
                             state side
                             (assoc eid :source card :source-type :runner-install) % nil))
                     (:discard runner)))
     :cost [(->c :click 1) (->c :trash-can) (->c :trash-from-hand 1)]
     :msg "install a program, piece of hardware, or Virtual resource from the heap"
     :effect
     (effect
       (continue-ability
         {:async true
          :prompt "Choose a card to install"
          :choices (req (vec (sort-by
                              :title
                              (filter #(and (or (program? %)
                                                (hardware? %)
                                                (and (resource? %)
                                                     (has-subtype? % "Virtual")))
                                            (can-pay? state :runner (assoc eid :source card :source-type :runner-install) % nil
                                                      [(->c :credit (install-cost state side %))]))
                                      (:discard runner)))))
          :msg (msg "install " (:title target) " from the heap")
          :effect (req (runner-install state :runner (assoc eid :source card :source-type :runner-install) target nil))}
         card nil))}]})

(defcard "Red Team"
  {:data {:counter {:credit 12}}
   :events [(trash-on-empty :credit)
            {:event :successful-run
             :req (req this-card-run)
             :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
             :interactive (req true)
             :async true
             :effect (req (let [credits (min 3 (get-counters card :credit))]
                            (add-counter state side card :credit (- credits))
                            (gain-credits state side eid credits)))}]
   :abilities [{:cost [(->c :click 1)]
                :prompt "Choose a server"
                :req (req (->> runnable-servers
                               (map unknown->kw)
                               (filter is-central?)
                               (remove (into #{} (:made-run runner-reg)))
                               not-empty))
                :choices (req (cancellable
                                (->> runnable-servers
                                     (map unknown->kw)
                                     (filter is-central?)
                                     (remove (into #{} (:made-run runner-reg)))
                                     (map central->name))))
                :msg "make a run on central server"
                :makes-run true
                :async true
                :effect (effect (make-run eid target card))}]})

(defcard "Rogue Trading"
  {:data {:counter {:credit 18}}
   :events [(trash-on-empty :credit)]
   :abilities [{:cost [(->c :click 2)]
                :keep-menu-open :while-2-clicks-left
                :msg "gain 6 [Credits] and take 1 tag"
                :async true
                :effect (req (let [credits (min 6 (get-counters card :credit))]
                               (add-counter state side card :credit (- credits))
                               (wait-for (gain-credits state :runner credits)
                                         (gain-tags state :runner eid 1))))}]})

(defcard "Rolodex"
  {:on-install {:async true
                :msg "look at the top 5 cards of the stack"
                :waiting-prompt true
                :effect (effect (continue-ability
                                  (let [from (take 5 (:deck runner))]
                                    (when (pos? (count from))
                                      (reorder-choice :runner :corp from '() (count from) from)))
                                  card nil))}
   :on-trash {:async true
              :effect (req (system-msg state :runner
                                       (str "trashes "
                                            (enumerate-str (map :title (take 3 (:deck runner))))
                                            " from the stack due to " (:title card) " being trashed"))
                           (mill state :runner eid :runner 3))}})

(defcard "Rosetta 2.0"
  (let [find-rfg (fn [state card]
                   (last (filter #(= (:cid card) (get-in % [:persistent :from-cid]))
                                 (get-in @state [:runner :rfg]))))]
    {:abilities [{:req (req (not (install-locked? state side)))
                  :label "Install a program from the stack"
                  :async true
                  :cost [(->c :click 1) (->c :rfg-program 1)]
                  :effect
                  (effect
                    (continue-ability
                      {:async true
                       :prompt "Choose a non-virus program to install"
                       :choices (req (concat
                                       (->> (:deck runner)
                                            (filter
                                              #(and (program? %)
                                                    (not (has-subtype? % "Virus"))
                                                    (can-pay? state :runner (assoc eid :source card :source-type :runner-install) % nil
                                                              [(->c :credit (install-cost
                                                                              state side %
                                                                              {:cost-bonus (- (:cost (find-rfg state card)))}))])))
                                            (sort-by :title)
                                            (seq))
                                       ["Done"]))
                       :msg (msg (if (= target "Done")
                                   "shuffle the stack"
                                   (str "install " (:title target)
                                        " from the stack, lowering its cost by "
                                        (:cost (find-rfg state card)))))
                       :effect (req (trigger-event state side :searched-stack)
                                    (shuffle! state side :deck)
                                    (if (= target "Done")
                                      (effect-completed state side eid)
                                      (runner-install state side (assoc eid :source card :source-type :runner-install)
                                                      target {:cost-bonus (- (:cost (find-rfg state card)))})))}
                      card nil))}]}))

(defcard "Sacrificial Clone"
  {:interactions {:prevent [{:type #{:net :brain :meat}
                             :req (req true)}]}
   :abilities [{:cost [(->c :trash-can)]
                :label "prevent damage"
                :async true
                :msg (msg (let [cards (concat (get-in runner [:rig :hardware])
                                              (filter #(not (has-subtype? % "Virtual"))
                                                      (get-in runner [:rig :resource]))
                                              (:hand runner))]
                            (str "prevent all damage, trash "
                                 (quantify (count cards) "card")
                                 " (" (enumerate-str (map :title cards)) "),"
                                 " lose " (quantify (:credit (:runner @state)) "credit")
                                 ", and lose " (quantify (count-real-tags state) "tag"))))
                :effect (req (damage-prevent state side :net Integer/MAX_VALUE)
                             (damage-prevent state side :meat Integer/MAX_VALUE)
                             (damage-prevent state side :brain Integer/MAX_VALUE)
                             (wait-for
                               (trash-cards
                                 state side
                                 (concat (get-in runner [:rig :hardware])
                                         (filter #(not (has-subtype? % "Virtual"))
                                                 (get-in runner [:rig :resource]))
                                         (:hand runner))
                                 {:cause-card card})
                               (wait-for (lose-credits state side (make-eid state eid) :all)
                                         (lose-tags state side eid :all))))}]})

(defcard "Sacrificial Construct"
  {:interactions {:prevent [{:type #{:trash-program :trash-hardware}
                             :req (req true)}]}
   :abilities [{:cost [(->c :trash-can)]
                :label "prevent a program trash"
                :effect (effect (trash-prevent :program 1)
                                (trash-prevent :hardware 1))}]})

(defcard "Safety First"
  {:static-abilities [(runner-hand-size+ -2)]
   :events [{:event :runner-turn-ends
             :async true
             :effect (req (if (< (count (:hand runner)) (hand-size state :runner))
                            (do (system-msg state :runner (str "uses " (:title card) " to draw 1 card"))
                                (draw state :runner eid 1))
                            (effect-completed state :runner eid)))}]})

(defcard "Salsette Slums"
  {:interactions
   {:access-ability
    {:label "Remove card from game"
     :req (req (and (not (get-in @state [:per-turn (:cid card)]))
                    (not (in-discard? target))
                    (:trash target)
                    (can-pay? state :runner (assoc eid :source card :source-type :ability)
                              card (:title target) [(->c :credit (trash-cost state side target))])))
     :once :per-turn
     :async true
     :trash? false
     :effect (req (let [trash-cost (trash-cost state side target)]
                    (wait-for (pay state side (make-eid state eid) card [(->c :credit trash-cost)])
                              (let [payment-str (:msg async-result)
                                    card (move state :corp target :rfg)]
                                (system-msg state side
                                            (str payment-str
                                                 " and remove " (:title target)
                                                 " from the game"))
                                (complete-with-result state side eid card)))))}}})

(defcard "Salvaged Vanadis Armory"
  {:events [{:event :damage
             :trash-icon true
             :optional
             {:waiting-prompt true
              :prompt "Trash Salvaged Vanadis Armory to force the Corp to trash the top cards of R&D?"
              :yes-ability {:async true
                            :cost [(->c :trash-can)]
                            :msg (msg "force the Corp to trash the top "
                                      (quantify (get-turn-damage state :runner) "card")
                                      " of R&D")
                            :effect (effect (mill :corp eid :corp (get-turn-damage state :runner)))}}}]})

(defcard "Same Old Thing"
  {:abilities [{:async true
                :label "play an event in the heap"
                :cost [(->c :click 2) (->c :trash-can)]
                :req (req (and (not (zone-locked? state :runner :discard))
                               (pos? (count (filter event? (:discard runner))))))
                :prompt "Choose an event in the heap"
                :msg (msg "play " (:title target))
                :show-discard true
                :choices {:card #(and (event? %)
                                      (in-discard? %))}
                :effect (effect (play-instant eid target))}]})

(defcard "Scrubber"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (corp? target)))
                                :type :recurring}}})

(defcard "Security Testing"
  (let [ability {:prompt "Choose a server"
                 :label "Choose a server (start of turn)"
                 :choices (req (concat servers ["No server"]))
                 :interactive (req true)
                 :msg (msg "target " target)
                 :req (req (and (:runner-phase-12 @state)
                                (not (used-this-turn? (:cid card) state))))
                 :effect (req (when (not= target "No server")
                                (update! state side (assoc card :card-target target))))}]
    {:events [(assoc ability :event :runner-turn-begins)
              (assoc
                (successful-run-replace-breach
                  {:mandatory true
                   :ability
                   {:msg "gain 2 [Credits]"
                    :async true
                    :effect (effect (gain-credits eid 2))}})
                :req (req (when-let [card (get-card state card)]
                            (and (= (zone->name (:server context)) (:card-target card))
                                 (first-event? state side :successful-run
                                               (fn [targets]
                                                 (let [context (first targets)]
                                                   (= (zone->name (:server context))
                                                      (:card-target card)))))))))
              {:event :runner-turn-ends
               :silent (req true)
               :effect (effect (update! (dissoc card :card-target)))}]
     :abilities [ability]}))

(defcard "Smartware Distributor"
  (let [start-of-turn-ability {:once :per-turn
                               :label "Take 1 [Credits] (start of turn)"
                               :req (req (and (:runner-phase-12 @state)
                                              (pos? (get-counters card :credit))))
                               :msg "take 1 [Credits]"
                               :async true
                               :effect (req (add-counter state side card :credit -1)
                                         (gain-credits state side eid 1))}]
    {:flags {:drip-economy (req (pos? (get-counters card :credit)))}
     :abilities [{:cost [(->c :click 1)]
                  :msg "place 3 [Credits]"
                  :req (req (not (:runner-phase-12 @state)))
                  :effect (req (add-counter state side card :credit 3))}
                 start-of-turn-ability]
     :events [(assoc start-of-turn-ability :event :runner-turn-begins)]}))

(defcard "Slipstream"
  {:events [{:event :pass-ice
             :optional
             {:req (req (and (rezzed? (get-card state (:ice context)))
                             (some #(and (ice? %)
                                         (not (protecting-same-server? (:ice context) %))
                                         (= run-position (card-index state %))
                                         (is-central? (second (get-zone %))))
                                   (all-installed state :corp))))
              :prompt (msg "Trash " (:title card) " to approach a piece of ice protecting a central server?")
              :yes-ability
              {:async true
               :effect
               (effect
                 (continue-ability
                   (let [passed-ice (:ice context)]
                     {:async true
                      :prompt "Choose a piece of ice protecting a central server at the same position"
                      :choices {:req (req (and (ice? target)
                                               (not (protecting-same-server? passed-ice target))
                                               (= run-position (card-index state target))
                                               (not (same-card? target passed-ice))
                                               (is-central? (second (get-zone target)))))}
                      :msg (msg "approach " (card-str state target))
                      :effect (req (wait-for (trash state side card {:unpreventable true :cause-card card})
                                             (let [dest (second (get-zone target))]
                                               (swap! state
                                                      update :run
                                                      assoc :position (inc run-position) :server [dest])
                                               (set-next-phase state :approach-ice)
                                               (update-all-ice state side)
                                               (update-all-icebreakers state side)
                                               (continue-ability state side (offer-jack-out) card nil))))})
                   card nil))}}}]})

(defcard "Spoilers"
  {:events [{:event :agenda-scored
             :async true
             :interactive (req true)
             :msg "trash the top card of R&D"
             :effect (effect (mill :corp eid :corp 1))}]})

(defcard "Starlight Crusade Funding"
  {:on-install {:msg "ignore additional costs on Double events"
                :effect (req (swap! state assoc-in [:runner :register :double-ignore-additional] true))}
   :events [{:event :runner-turn-begins
             :msg "lose [Click] and ignore additional costs on Double events"
             :effect (req (lose-clicks state :runner 1)
                          (swap! state assoc-in [:runner :register :double-ignore-additional] true))}]
   :leave-play (req (swap! state update-in [:runner :register] dissoc :double-ignore-additional))})

(defcard "Stim Dealer"
  {:events [{:event :runner-turn-begins
             :effect (req (if (>= (get-counters card :power) 2)
                            (do (add-counter state side card :power (- (get-counters card :power)))
                                (damage state side eid :brain 1 {:unpreventable true :card card})
                                (system-msg state side "takes 1 core damage from Stim Dealer"))
                            (do (add-counter state side card :power 1)
                                (gain-clicks state side 1)
                                (system-msg state side (str "uses " (:title card) " to gain [Click]")))))}]})

(defcard "Stoneship Chart Room"
  {:abilities [{:label "Draw 2 cards"
                :msg "draw 2 cards"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (draw :runner eid 2))}
               {:label "Charge a card"
                :req (req (can-charge state side))
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (continue-ability (charge-ability state side eid card) card nil))}]})

(defcard "Street Magic"
  (letfn [(runner-break [unbroken-subs]
            {:prompt "Choose a subroutine to resolve"
             :choices unbroken-subs
             :effect (req (let [sub (first (filter #(and (not (:broken %))
                                                         (= target (make-label (:sub-effect %))))
                                                   (:subroutines current-ice)))]
                            (wait-for (resolve-ability state :corp (make-eid state {:source current-ice :source-type :subroutine})
                                                       (:sub-effect sub) current-ice nil)
                                      (if (and (:run @state)
                                               (not (:ended (:end-run @state)))
                                               (rest unbroken-subs))
                                        (continue-ability
                                          state side
                                          (runner-break (remove-once #(= target %) unbroken-subs))
                                          card nil)
                                        (effect-completed state side eid)))))})]
    {:abilities [{:implementation "Effect is manually triggered"
                  :label "Choose subroutine order"
                  :req (req (pos? (count (remove :broken (:subroutines current-ice)))))
                  :async true
                  :msg (msg "choose the order the unbroken subroutines on "
                         (:title current-ice) " resolve")
                  :effect (effect (continue-ability (runner-break (unbroken-subroutines-choice current-ice)) card nil))}]}))

(defcard "Street Peddler"
  {:on-install {:interactive (req (some #(card-flag? % :runner-install-draw true) (all-active state :runner)))
                :effect (req (doseq [c (take 3 (:deck runner))]
                               (host state side (get-card state card) c {:facedown true})))}
   ;; req to use ability - can pay to install at least one of the cards
   :abilities [{:async true
                :label "install a hosted card"
                :trash-icon true
                :req (req (some #(and (not (event? (get-card state %)))
                                           (runner-can-pay-and-install?
                                             state side
                                             (assoc eid :source card :source-type :runner-install)
                                             (get-card state %)
                                             {:cost-bonus -1}))
                                (seq (:hosted card))))
                :effect (req (set-aside state side eid (:hosted card))
                             (let [set-aside-cards (get-set-aside state side eid)]
                               (wait-for (trash state side card {:cause :ability-cost :cause-card card})
                                         (system-msg state side "trashed")
                                         (continue-ability
                                           state side
                                           {:prompt "Choose a set-aside card to install"
                                            :waiting-prompt true
                                            :not-distinct true
                                            :async true
                                            :choices (req (filter #(and (not (event? %))
                                                                        (runner-can-install? state side % nil)
                                                                        (can-pay? state side (assoc eid :source card :source-type :runner-install)
                                                                                  % nil [(->c :credit (install-cost state side % {:cost-bonus -1}))])) set-aside-cards))
                                            :msg (msg "install " (:title target) ", lowering its install cost by 1 [Credits]. "
                                                      (enumerate-str (map :title (remove-once #(same-card? % target) set-aside-cards)))
                                                      "are trashed as a result")
                                            :effect (req (wait-for (runner-install state side (make-eid  state (assoc eid :source card :source-type :runner-install)) target {:cost-bonus -1})
                                                                   (trash-cards state side (assoc eid :source card) (filter #(not (same-card? % target)) set-aside-cards) {:unpreventable true :cause-card card})))}
                                           card nil))))}]})

(defcard "Symmetrical Visage"
  {:events [{:event :runner-click-draw
             :req (req (genetics-trigger? state side :runner-click-draw))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Synthetic Blood"
  {:events [{:event :damage
             :req (req (genetics-trigger? state side :damage))
             :msg "draw 1 card"
             :async true
             :effect (effect (draw :runner eid 1))}]})

(defcard "Tallie Perrault"
  {:abilities [{:label "Draw 1 card for each bad publicity the Corp has"
                :async true
                :cost [(->c :trash-can)]
                :effect (effect (draw eid (count-bad-pub state)))
                :msg (msg "draw " (quantify (count-bad-pub state) "card"))}]
   :events [{:event :play-operation
             :optional
             {:req (req (or (has-subtype? (:card context) "Black Ops")
                            (has-subtype? (:card context) "Gray Ops")))
              :waiting-prompt true
              :prompt "Give the Corp 1 bad publicity and take 1 tag?"
              :player :runner
              :yes-ability {:msg "give the Corp 1 bad publicity and take 1 tag"
                            :async true
                            :effect (effect (gain-bad-publicity :corp 1)
                                            (gain-tags :runner eid 1))}}}]})

(defcard "Tech Trader"
  {:events [{:event :runner-trash
             :req (req (and (= side :runner)
                            (= :ability-cost (:cause target))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Technical Writer"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (and (or (hardware? (:card context))
                                (program? (:card context)))
                            (not (:facedown? context))))
             :msg "place 1 [Credits] on itself"
             :effect (effect (add-counter :runner card :credit 1))}]
   :abilities [{:cost [(->c :click 1) (->c :trash-can)]
                :label "Take all hosted credits"
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (get-counters card :credit)))}]})

(defcard "Telework Contract"
  {:data {:counter {:credit 9}}
   :events [(trash-on-empty :credit)]
   :abilities [{:label "Take 3 [Credits] from this resource"
                :cost [(->c :click 1)]
                :once :per-turn
                :msg "gain 3 [Credits]"
                :async true
                :effect (req (let [credits (min 3 (get-counters card :credit))]
                               (add-counter state side card :credit (- credits))
                               (gain-credits state :runner eid credits)))}]})

(defcard "Temple of the Liberated Mind"
  {:abilities [{:cost [(->c :click 1)]
                :label "Place 1 power counter"
                :msg "place 1 power counter on itself"
                :effect (effect (add-counter card :power 1))}
               {:label "Gain [Click]"
                :cost [(->c :power 1)]
                :req (req (= (:active-player @state) :runner))
                :msg "gain [Click]" :once :per-turn
                :effect (effect (gain-clicks 1))}]})

(defcard "Temüjin Contract"
  {:data {:counter {:credit 20}}
   :on-install {:prompt "Choose a server"
                :choices (req servers)
                :msg (msg "target " target)
                :effect (effect (update! (assoc card :card-target target)))}
   :events [(trash-on-empty :credit)
            {:event :successful-run
             :req (req (= (zone->name (:server context)) (:card-target (get-card state card))))
             :msg (msg "gain " (min 4 (get-counters card :credit)) " [Credits]")
             :async true
             :effect (req (let [credits (min 4 (get-counters card :credit))]
                            (add-counter state side card :credit (- credits))
                            (gain-credits state side eid credits)))}]})

(defcard "The Archivist"
  {:static-abilities [(link+ 1)]
   :events [{:event :agenda-scored
             :interactive (req true)
             :trace {:base 1
                     :req (req (or (has-subtype? (:card context) "Initiative")
                                   (has-subtype? (:card context) "Security")))
                     :unsuccessful
                     {:effect (effect (gain-bad-publicity :corp 1)
                                      (system-msg :corp (str "takes 1 bad publicity")))}}}]})

(defcard "The Artist"
  {:abilities [{:cost [(->c :click 1)]
                :label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :once :per-turn
                :once-key :artist-credits
                :async true
                :effect (effect (gain-credits eid 2))}
               {:cost [(->c :click 1)]
                :label "Install a program or piece of hardware"
                :req (req (some #(and (or (hardware? %)
                                          (program? %))
                                      (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                [(->c :credit (install-cost state side % {:cost-bonus -1}))]))
                                (:hand runner)))
                :prompt "Choose a program or piece of hardware to install"
                :choices
                {:req (req (and (or (hardware? target)
                                    (program? target))
                                (in-hand? target)
                                (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                          [(->c :credit (install-cost state side target {:cost-bonus -1}))])))}
                :once :per-turn
                :once-key :artist-install
                :async true
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                target {:no-msg true
                                                        :cost-bonus -1}))
                :msg (msg "install " (:title target) " from the grip, lowering its cost by 1 [Credits]")}]})

(defcard "The Back"
  {:implementation "Placing power counters is manual"
   ; :events [{:event :spent-credits-from-card
               ; :req (req (and (:run @state)
                                ; (hardware? target)
                                ; (not (used-this-turn? (:cid card) state))))
               ; :once :per-turn
               ; :async true
               ; :effect (effect (system-msg (str "places 1 power counter on " (:title card)))
                                 ; (add-counter card :power 1))}]
   :abilities [{:label "Manually place 1 power counter"
                :effect (effect (system-msg (str "manually places 1 power counter on " (:title card)))
                                (add-counter card :power 1))}
               {:label "Shuffle back cards with [Trash] abilities"
                :req (req (and (pos? (get-counters card :power))
                               (some has-trash-ability? (:discard runner))
                               (not (zone-locked? state :runner :discard))))
                :cost [(->c :click 1) (->c :remove-from-game)]
                :show-discard true
                :choices {:max (req (* 2 (get-counters card :power)))
                          :req (req (and (runner? target)
                                         (in-discard? target)
                                         (has-trash-ability? target)))}
                :msg (msg "shuffle " (enumerate-str (map :title targets))
                          " into the stack")
                :effect (req (doseq [c targets] (move state side c :deck))
                             (shuffle! state side :deck)
                             (effect-completed state side eid))}]})

(defcard "The Black File"
  {:on-install {:msg "prevent the Corp from winning the game unless they are flatlined"}
   :static-abilities [{:type :cannot-win-on-points
                       :req (req (and (= :corp side)
                                      (< (get-counters card :power) 3)))
                       :value true}]
   :events [{:event :runner-turn-begins
             :effect (req (if (<= 2 (get-counters card :power))
                            (do (move state side card :rfg)
                                (system-msg state side "removes The Black File from the game")
                                (check-win-by-agenda state side))
                            (add-counter state side card :power 1)))}]
   :on-trash {:effect (effect (check-win-by-agenda))}
   :leave-play (effect (check-win-by-agenda))})

(defcard "The Class Act"
  (let [draw-ability {:req (req (= :this-turn (installed? card)))
                      :async true
                      :msg "draw 4 cards"
                      :effect (effect (draw :runner eid 4))}]
    {:events [(assoc draw-ability :event :corp-turn-ends)
              (assoc draw-ability :event :runner-turn-ends)
              {:event :pre-runner-draw
               :req (req (and (first-event? state :runner :pre-runner-draw)
                              (< 1 (count (:deck runner)))
                              (pos? target)))
               :once :per-turn
               :once-key :the-class-act-put-bottom
               :async true
               :effect
               (req (set-aside-for-me state :runner eid (take (inc target) (:deck runner)))
                    (let [cards (get-set-aside state :runner eid)]
                      (continue-ability
                        state side
                        {:waiting-prompt true
                         :prompt "Choose 1 card to add to the bottom of the stack"
                         :choices {:card #(some (fn [c] (same-card? c %)) cards)
                                   :all true}
                         :effect
                         (req (system-msg state side
                                          (str "uses " (:title card) " to add the "
                                               (pprint/cl-format nil "~:R"
                                                                 (inc (first (keep-indexed #(when (same-card? target %2) %1) cards))))
                                               " card on the top of the stack to the bottom"))
                                          (doseq [c (reverse cards)]
                                            (move state :runner
                                                  c :deck
                                                  (if (same-card? c target) nil {:front true}))))}
                        card nil)))}]}))

(defcard "The Helpful AI"
  {:static-abilities [(link+ 1)]
   :abilities [{:msg (msg "give +2 strength to " (:title target))
                :label "pump icebreaker"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (installed? %))}
                :cost [(->c :trash-can)]
                :effect (effect (pump target 2 :end-of-turn))}]})

(defcard "The Masque A"
  {:abilities [{:cost [(->c :click 1) (->c :trash-can)]
                :label "Make a run and gain [click]. If successful, draw 1 card"
                :prompt "Choose a server"
                :choices (req runnable-servers)
                :msg (msg "make a run on " target " and gain [click]")
                :async true
                :effect (effect
                          (gain-clicks :runner 1)
                          (register-events
                            card
                            [{:event :successful-run
                              :unregister-once-resolved true
                              :duration :end-of-run
                              :async true
                              :msg "draw 1 card"
                              :effect (effect (draw :runner eid 1))}])
                          (make-run eid target card))}]})

(defcard "The Masque B"
  {:implementation "Successful run condition not implemented"
   :abilities [{:cost [(->c :click 1) (->c :trash-can)]
                :label "Make a run and gain [click]. If successful, make another run on another server"
                :prompt "Choose a server"
                :choices (req runnable-servers)
                :msg (msg "make a run on " target " and gain [click]")
                :async true
                :effect (effect (gain-clicks 1)
                                (make-run eid target card))}]})

(defcard "The Nihilist"
  (let [corp-choice {:player :corp
                     :waiting-prompt true
                     :prompt "Choose one"
                     :choices (req [(when (seq (:deck corp))
                                      "Trash the top card of R&D")
                                    "The Runner draws 2 cards"])
                     :async true
                     :msg (msg (if (= target "The Runner draws 2 cards")
                                "draw 2 cards"
                                (str "force the Corp to " (decapitalize target))))
                     :effect (req (if (= target "The Runner draws 2 cards")
                                    (draw state :runner eid 2)
                                    (mill state :corp eid :corp 1)))}
        maybe-spend-2 {:event :runner-turn-begins
                       :interactive (req true)
                       :optional
                       {:prompt "Spend 2 virus counters?"
                        :yes-ability
                        {:req (req (<= 2 (number-of-runner-virus-counters state)))
                         :async true
                         :effect (req (wait-for (resolve-ability state side (pick-virus-counters-to-spend 2) card nil)
                                                (if (:msg async-result)
                                                  (do (system-msg state side (str "spends " (:msg async-result)))
                                                      (continue-ability state side corp-choice card nil))
                                                  (effect-completed state side eid))))}}}]
    {:events [maybe-spend-2
              {:event :runner-install
               :once :per-turn
               :msg "place 2 virus tokens on itself"
               :req (req (has-subtype? (:card context) "Virus"))
               :effect (effect (add-counter card :virus 2))}]}))

(defcard "The Shadow Net"
  (letfn [(events [runner] (filter #(and (event? %) (not (has-subtype? % "Priority"))) (:discard runner)))]
    {:abilities [{:async true
                  :cost [(->c :click 1) (->c :forfeit)]
                  :req (req (and (pos? (count (events runner)))
                                 (not (zone-locked? state :runner :discard))))
                  :label "Play an event from the heap, ignoring all costs"
                  :prompt "Choose an event to play"
                  :msg (msg "play " (:title target) " from the heap, ignoring all costs")
                  :choices (req (cancellable (events runner) :sorted))
                  :effect (effect (play-instant eid target {:ignore-cost true}))}]}))

(defcard "The Source"
  {:static-abilities [{:type :advancement-requirement
                       :value 1}
                      {:type :steal-additional-cost
                       :value (req (->c :credit 3))}]
   :events [{:event :agenda-scored
             :async true
             :effect (effect (trash eid card {:cause :runner-ability :cause-card card}))}
            {:event :agenda-stolen
             :async true
             :effect (effect (trash eid card {:cause :runner-ability :cause-card card}))}]})

(defcard "The Supplier"
  (let [ability {:label "Install a hosted card (start of turn)"
                 :prompt "Choose a hosted card to install"
                 :req (req (some #(can-pay? state side (assoc eid :source card :source-type :runner-install)
                                            % nil [(->c :credit (install-cost state side % {:cost-bonus -2}))])
                                 (:hosted card)))
                 :choices
                 {:req (req (and (= "The Supplier" (:title (:host target)))
                                 (runner? target)
                                 (can-pay? state side (assoc eid :source card :source-type :runner-install)
                                           target nil [(->c :credit (install-cost state side target {:cost-bonus -2}))])))}
                 :once :per-turn
                 :msg (msg "install " (:title target)
                           ", lowering its install cost by 2 [Credits]")
                 :async true
                 :effect
                 (req (if-not (runner-can-install? state side target nil)
                        (effect-completed state side eid)
                        (do (update! state side
                                     (-> card
                                         (assoc :supplier-installed (:cid target))
                                         (update-in [:hosted]
                                                    (fn [coll]
                                                      (remove-once #(same-card? % target) coll)))))
                            (runner-install state side (assoc eid :source card :source-type :runner-install)
                                            target {:cost-bonus -2}))))}]
    {:flags {:drip-economy true}  ; not technically drip economy, but has an interaction with Drug Dealer
     :abilities [{:label "Host a resource or piece of hardware"
                  :cost [(->c :click 1)]
                  :keep-menu-open :while-clicks-left
                  :prompt "Choose a card in the grip"
                  :choices {:card #(and (or (hardware? %)
                                            (resource? %))
                                        (in-hand? %))}
                  :effect (effect (host card target))
                  :msg (msg "install and host " (:title target))}
                 ability]
     ; A card installed by The Supplier is ineligible to receive the turn-begins event for this turn.
     :suppress [{:event :runner-turn-begins
                 :req (req (= (:cid target) (:supplier-installed (get-card state card))))}]
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-turn-ends
               :req (req (:supplier-installed card))
               :effect (effect (update! (dissoc card :supplier-installed)))}]}))

(defcard "The Turning Wheel"
  (letfn [(ttw-ab [name server]
            {:label (str "Access an additional card in " name)
             :cost [(->c :power 2)]
             :req (req run)
             :keep-menu-open :while-2-power-tokens-left
             :msg (msg "access 1 additional card from " name " for the remainder of the run")
             :effect (effect (register-events
                              card [(breach-access-bonus server 1 {:duration :end-of-run})]))})
          (ttw-bounce [name server]
            {:label (str "Shortcut: Bounce " name)
             :cost [(->c :click 1)]
             :keep-menu-open :while-clicks-left
             :msg (msg "bounce off of " name " for a token (shortcut)")
             :effect (req (add-counter state :runner card :power 1)
                          (swap! state assoc-in [:runner :register :made-click-run] true)
                          (swap! state update-in [:runner :register :unsuccessful-run] conj server)
                          (swap! state update-in [:runner :register :made-run] conj server))})]
    {:events [{:event :agenda-stolen
               :effect (effect (update! (assoc card :agenda-stolen true)))
               :silent (req true)}
              {:event :run-ends
               :effect (req (when (and (not (:agenda-stolen card))
                                       (#{:hq :rd} (target-server target)))
                              (add-counter state side card :power 1)
                              (system-msg state :runner (str "uses " (:title card) " to place 1 power counter on itself")))
                         (update! state side (dissoc (get-card state card) :agenda-stolen)))
               :silent (req true)}]
     :abilities [(ttw-ab "R&D" :rd)
                 (ttw-ab "HQ" :hq)
                 (ttw-bounce "R&D" :rd)
                 (ttw-bounce "HQ" :hq)]}))

(defcard "The Twinning"
  {:events [{:event :spent-credits-from-card
             :req (req (and
                         (installed? target)
                         (runner? target)
                         (first-event? state side :spent-credits-from-card #(installed? (first %)))))
             :async true
             :msg "place a power counter on itself"
             :effect (req (add-counter state :runner card :power 1 {:placed true})
                          (effect-completed state side eid))}
            {:event :breach-server
             :async true
             :req (req (and (#{:rd :hq} target)
                            (< 0 (get-counters card :power))))
             :effect (req
                       (let [target-server target]
                         (continue-ability
                           state side
                           {:req (req (< 0 (get-counters card :power)))
                            :prompt (msg "How many additional " (zone->name target-server) " accesses do you want to make?")
                            :choices {:number (req (min 2 (get-counters card :power)))
                                      :default (req (min 2 (get-counters card :power)))}
                            :msg (msg "access " (quantify target "additional card") " from "
                                      (zone->name target-server))
                            :async true
                            :effect (effect (access-bonus target-server (max 0 target))
                                            (add-counter :runner card :power (- target) {:placed true})
                                            (effect-completed eid))}
                           card nil)))}]})

(defcard "Theophilius Bagbiter"
  {:static-abilities [(runner-hand-size+ (req (:credit runner)))]
   :on-install {:async true
                :effect (req (swap! state assoc-in [:runner :hand-size :base] 0)
                             (lose-credits state :runner eid :all))}
   :leave-play (req (swap! state assoc-in [:runner :hand-size :base] 5))})

(defcard "Thunder Art Gallery"
  (let [first-event-check (fn [state fn1 fn2]
                            (and (fn1 state :runner :runner-lose-tag (fn [[context]] (= :runner (:side context))))
                                 (fn2 state :runner :runner-prevent (fn [[context]] (= :tag (:type context))))))
        ability {:async true
                 :prompt "Choose a card in the grip"
                 :waiting-prompt true
                 :choices
                 {:req (req (and (runner? target)
                                 (in-hand? target)
                                 (not (event? target))
                                 (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                           [(->c :credit (install-cost state side target {:cost-bonus -1}))])))}
                 :msg (msg "install " (:title target))
                 :effect (effect (runner-install (assoc eid
                                                        :source card
                                                        :source-type :runner-install)
                                                 target {:cost-bonus -1}))
                 :cancel-effect (effect (effect-completed eid))}]
    {:events [(assoc ability
                     :event :runner-lose-tag
                     :req (req (and (first-event-check state first-event? no-event?)
                                    (= side :runner))))
              (assoc ability
                     :event :runner-prevent
                     :req (req (and (first-event-check state no-event? first-event?)
                                    (= :tag (:type context)))))]}))

(defcard "Tri-maf Contact"
  {:abilities [{:cost [(->c :click 1)]
                :msg "gain 2 [Credits]"
                :once :per-turn
                :async true
                :effect (effect (gain-credits eid 2))}]
   :on-trash {:async true
              :effect (effect (damage eid :meat 3 {:unboostable true :card card}))}})

(defcard "Trickster Taka"
  (companion-builder
    ;; companion-builder: pay-credits req
    (req (and (= :ability (:source-type eid))
              (program? target)
              run))
    ;; companion-builder: turn-ends-ability
    {:prompt "Choose one"
     :waiting-prompt true
     :choices ["Take 1 tag" "Trash Trickster Taka"]
     :player :runner
     :msg (msg (if (= target "Trash Trickster Taka")
                 "trash itself"
                 (decapitalize target)))
     :async true
     :effect (req (if (= target "Trash Trickster Taka")
                    (trash state :runner eid card {:cause-card card})
                    (gain-tags state :runner eid 1)))}
    ;; companion-builder: ability
    {:req (req (and (pos? (get-counters (get-card state card) :credit))
                    run
                    (not (:successful run))
                    (not (:unsuccessful run))))
     :msg "take 1 [Credits]"
     :async true
     :effect (req (add-counter state side card :credit -1)
                  (wait-for (gain-credits state side 1)
                            (trigger-event-sync state side eid :spent-credits-from-card card)))}))

(defcard "Tsakhia \"Bankhar\" Gantulga"
  (let [subroutine {:variable true
                    :sub-effect (do-net-damage 1)}
        matches-server (fn [target card state side]
                         (= (:card-target card)
                            (zone->name (second (get-zone target)))))
        ability {:prompt "Choose a server"
                 :label "Choose a server (start of turn)"
                 :choices (req (concat servers ["No server"]))
                 :interactive (req true)
                 :waiting-prompt true
                 :msg (msg "target " target)
                 :req (req (and (:runner-phase-12 @state)
                                (not (used-this-turn? (:cid card) state))))
                 :effect (req (when (not= target "No server")
                                (update! state side (assoc card :card-target target))))}]
    {:events [(assoc ability :event :runner-turn-begins)
              {:event :encounter-ice
               :req (req (and
                           (matches-server (:ice target) card state side)
                           (first-event? state side :encounter-ice #(matches-server (:ice (first %)) card state side))))
               :effect (effect
                         (register-events
                           card
                           [{:event :pre-resolve-subroutine
                             :duration :end-of-encounter
                             :async true
                             :msg "force the Corp to resolve \"[Subroutine] Do 1 net damage\""
                             :effect (req (update-current-encounter state :replace-subroutine subroutine)
                                          (effect-completed state side eid))}]))}
              {:event :runner-turn-ends
               :silent (req true)
               :effect (effect (update! (dissoc card :card-target)))}]
     :abilities [ability]}))

(defcard "Tyson Observatory"
  {:abilities [{:prompt "Choose a piece of Hardware"
                :msg (msg "add " (:title target) " to their Grip")
                :label "Search stack for a piece of hardware"
                :choices (req (cancellable (filter hardware? (:deck runner)) :sorted))
                :cost [(->c :click 2)]
                :keep-menu-open :while-2-clicks-left
                :effect (effect (trigger-event :searched-stack)
                                (shuffle! :deck)
                                (move target :hand))}]})

(defcard "Underworld Contact"
  (let [ability {:label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (req (if (and (<= 2 (get-link state))
                                         (:runner-phase-12 @state))
                                (do (system-msg state :runner (str "uses " (:title card) " to gain 1 [Credits]"))
                                    (gain-credits state :runner eid 1))
                                (effect-completed state side eid)))}]
    {:flags {:drip-economy true}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)]}))

(defcard "Urban Art Vernissage"
  (let [is-eligible?
          (fn [cr] (and (program? cr)
                        (has-subtype? cr "Trojan")
                        (not (has-subtype? cr "Virus"))))
        ability {:async true
                 :label "Return a non-virus trojan program to the grip"
                 :once :per-turn
                 :req (req (some is-eligible? (all-installed-runner state)))
                 :choices {:req (req (and (runner? target)
                                          (installed? target)
                                          (is-eligible? target)))}
                 :msg (msg "add " (:title target) " to the grip and place 2 [Credits] on itself")
                 :cancel-effect (req (system-msg state :runner (str "declines to use " (:title card)))
                                     (effect-completed state side eid))
                 :effect (req (move state side target :hand)
                              (add-counter state side card :credit 2)
                              (effect-completed state side eid))}]
    {:interactions {:pay-credits {:req (req (= :runner-install (:source-type eid)))
                                  :type :credit}}
     :flags {:runner-phase-12 (req (some is-eligible? (all-installed-runner state)))}
     :events [(assoc ability
                     :event :runner-turn-begins
                     :interactive (req true))]
     :abilities [ability]}))

(defcard "Utopia Shard"
  (shard-constructor "Utopia Shard" :hq "force the Corp to discard 2 cards from HQ at random"
                     (effect (trash-cards :corp eid (take 2 (shuffle (:hand corp))) {:cause-card card}))))

(defcard "Valentina Ferreira Carvalho"
  {:on-install {:prompt "Choose one"
                :choices (req [(when tagged "Remove 1 tag")
                               "Gain 2 [Credits]"
                               "Done"])
                :req (req (and (threat-level 3 state)
                               (= (:active-player @state) :runner)))
                :effect (req (cond
                               (= "Remove 1 tag" target) (do (lose-tags state :runner eid 1)
                                                             (system-msg state :runner (str "uses " (:title card)
                                                                                            " to " (decapitalize target))))
                               (= "Gain 2 [Credits]" target) (do (gain-credits state :runner eid 2)
                                                                 (system-msg state :runner (str "uses " (:title card)
                                                                                                " to " (decapitalize target))))
                               :else (effect-completed state side eid)))}
   :events [{:event :runner-lose-tag
             :req (req (= :runner (:side context)))
             :player :runner
             :msg "gain 1 [Credits]"
             :async true
             :interactive (req true)
             :effect (req (gain-credits state :runner eid 1))}]})

(defcard "Verbal Plasticity"
  {:events [{:event :runner-click-draw
             :req (req (genetics-trigger? state side :runner-click-draw))
             :msg "draw 1 additional card"
             :effect (effect (click-draw-bonus 1))}]})

(defcard "Virus Breeding Ground"
  {:events [{:event :runner-turn-begins
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:cost [(->c :click 1)]
                :label "move hosted virus counter"
                :req (req (pos? (get-counters card :virus)))
                :async true
                :effect (req (continue-ability
                               state side
                               {:msg (msg "move 1 virus counter to " (:title target))
                                :choices {:not-self true
                                          :card #(pos? (get-virus-counters state %))}
                                :effect (req (add-counter state side card :virus -1)
                                             (add-counter state side target :virus 1))}
                               card nil))}]})

(defcard "Wasteland"
  {:events [{:event :runner-trash
             :once-per-instance true
             :req (req (and (first-installed-trash-own? state :runner)
                            (some #(and (installed? (:card %))
                                        (runner? (:card %)))
                                  targets)))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Whistleblower"
  {:events [{:event :successful-run
             :optional
             {:autoresolve (get-autoresolve :auto-fire)
              :prompt "Name an agenda?"
              :yes-ability
              {:async true
               :prompt "Name an agenda"
               :choices {:card-title (req (and (corp? target)
                                               (agenda? target)))}
               :effect (effect (system-msg (str "trashes " (:title card)
                                                " to use " (:title card)
                                                " to name " (:title target)))
                               (register-events
                                 card
                                 (let [named-agenda target]
                                   [{:event :access
                                     :duration :end-of-run
                                     :unregister-once-resolved true
                                     :async true
                                     :req (req (= (:title target) named-agenda))
                                     :effect (effect (steal eid target))}]))
                               (trash eid card {:unpreventable true :cause-card card}))}}}]
   :abilities [(set-autoresolve :auto-fire "Whistleblower")]})

(defcard "Wireless Net Pavilion"
  {:implementation "[Erratum] Should be unique"
   :static-abilities [{:type :card-ability-additional-cost
                       :req (req (and (same-card? (:card context) (:basic-action-card corp))
                                      (= "Trash 1 resource if the Runner is tagged"
                                         (:label (:ability context)))))
                       :value (->c :credit 2)}]})

(defcard "Woman in the Red Dress"
  (let [ability {:msg (msg "reveal " (:title (first (:deck corp))) " from the top of R&D")
                 :label "Reveal the top card of R&D (start of turn)"
                 :once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :async true
                 :effect (req (wait-for
                                (reveal state side (first (:deck corp)))
                                (continue-ability
                                  state side
                                  {:optional
                                   {:player :corp
                                    :waiting-prompt true
                                    :prompt (msg "Draw " (:title (first (:deck corp))) "?")
                                    :yes-ability
                                    {:async true
                                     :effect (effect (system-msg (str "draws " (:title (first (:deck corp)))))
                                                     (draw eid 1))}
                                    :no-ability
                                    {:effect (effect (system-msg (str "declines to use " (:title card))))}}}
                                  card nil)))}]
    {:events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Wyldside"
  {:flags {:runner-turn-draw true
           :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                     (cons (get-in @state [:runner :identity])
                                                           (all-active-installed state :runner))))))}
   :events [{:event :runner-turn-begins
             :async true
             :effect (req (lose-clicks state side 1)
                          (if (get-in @state [:per-turn (:cid card)])
                            (effect-completed state side eid)
                            (do (system-msg state side (str "uses " (:title card) " to draw 2 cards and lose [Click]"))
                                (draw state side eid 2))))}]
   :abilities [{:msg "draw 2 cards and lose [Click]"
                :once :per-turn
                :async true
                :effect (effect (draw eid 2))}]})

(defcard "Xanadu"
  {:static-abilities [{:type :rez-cost
                       :req (req (ice? target))
                       :value 1}]})

(defcard "Zona Sul Shipping"
  {:events [{:event :runner-turn-begins
             :effect (effect (add-counter card :credit 1))}]
   :trash-when-tagged true
   :abilities [{:cost [(->c :click 1)]
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :label "Take all credits"
                :async true
                :effect (effect (add-counter card :credit
                                             (- (get-counters card :credit)))
                                (gain-credits eid (get-counters card :credit)))}]})
