(ns game.cards.programs
  (:require
   [game.core.access :refer [access-bonus max-access]]
   [game.core.board :refer [all-active all-active-installed all-installed all-installed-runner-type
                            card->server server->zone]]
   [game.core.card :refer [active? agenda? asset? card-index corp? facedown? faceup?
                           get-advancement-requirement get-card get-counters
                           get-nested-host get-title get-zone
                           hardware? has-subtype? in-hand? in-discard? ice? installed?
                           is-type? program? resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.charge :refer [charge-ability]]
   [game.core.cost-fns :refer [install-cost rez-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out trash-on-empty get-x-fn rfg-on-empty]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [any-effects register-lingering-effect unregister-effects-for-card update-disabled-cards]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [ability-as-handler dissoc-req not-used-once? pay
                             print-msg register-events register-once
                             trigger-event trigger-event-simult unregister-events]]
   [game.core.events :refer [run-events first-event? first-installed-trash? run-events
                             first-successful-run-on-server? no-event? turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [can-host? can-trash? card-flag? lock-zone release-zone zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-credits]]
   [game.core.hosting :refer [host]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.ice :refer [add-sub all-subs-broken-by-card? all-subs-broken?
                          any-subs-broken-by-card? auto-icebreaker break-sub
                          break-subroutine! break-subroutines-msg breaker-strength-bonus dont-resolve-subroutine!
                          get-strength ice-strength pump pump-ice set-current-ice strength-pump
                          unbroken-subroutines-choice update-all-icebreakers update-breaker-strength]]
   [game.core.initializing :refer [ability-init card-init subroutines-init]]
   [game.core.installing :refer [install-locked? runner-can-install? runner-can-pay-and-install?
                                 runner-install]]
   [game.core.link :refer [get-link]]
   [game.core.mark :refer [identify-mark-ability mark-changed-event]]
   [game.core.memory :refer [available-mu update-mu]]
   [game.core.moving :refer [flip-facedown mill move swap-cards swap-ice trash trash-cards
                             trash-prevent]]
   [game.core.optional :refer [get-autoresolve set-autoresolve never?]]
   [game.core.payment :refer [build-cost-label can-pay? cost-target cost-value ->c value]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [active-encounter? bypass-ice continue end-run end-run-prevent
                           get-current-encounter make-run successful-run-replace-breach
                           update-current-encounter]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote? protecting-same-server?
                              remote->name target-server unknown->kw zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.trace :refer [force-base]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defn- power-counter-break
  "Only break ability uses power counters
  (Cerberus suite: Cerberus \"Lady\" H1, Cerberus \"Rex\" H2, Cerberus \"Cuj.0\" H3)"
  [ice-type]
  (auto-icebreaker {:data {:counter {:power 4}}
                    :abilities [(break-sub [(->c :power 1)] 2 ice-type)
                                (strength-pump 1 1)]}))

(defn- swap-with-in-hand
  "Swap with a deva program from the grip
  (Deva suite: Aghora, Sadyojata, Vamadeva)"
  [card-name break-req]
  (auto-icebreaker
    {:abilities [(break-sub 1 1 "All" break-req)
                 (strength-pump 1 1)
                 {:req (req (seq (filter #(has-subtype? % "Deva") (:hand runner))))
                  :label "Swap with a deva program from the grip"
                  :cost [(->c :credit 2)]
                  :prompt (str "Choose a deva program to swap with " card-name)
                  :choices {:card #(and (in-hand? %)
                                        (has-subtype? % "Deva"))}
                  :msg (msg "swap in " (:title target) " from the grip")
                  :effect (req (if-let [hostcard (:host card)]
                                 (let [hosted (host state side (get-card state hostcard) target)]
                                   (card-init state side hosted {:resolve-effect false
                                                                 :init-data true})
                                   (move state side card :hand))
                                 (let [[_ moved-target] (swap-cards state side card target)]
                                   (card-init state side moved-target {:resolve-effect false
                                                                       :init-data true}))))}]}))

(defn- install-from-heap
  "Install-from-heap ability for conspiracy breakers
  (Conspiracy suite: Black Orchestra, MKUltra, Paperclip)"
  [title ice-type abilities]
  {:abilities abilities
   :events [{:event :encounter-ice
             :async true
             :location :discard
             :req (req (and (in-discard? card)
                            (has-subtype? (:ice context) ice-type)
                            (runner-can-pay-and-install?
                              state :runner
                              (assoc eid :source card :source-type :runner-install)
                              card nil)))
             :effect (effect
                       (continue-ability
                         {:req (req (and (not-any? #(and (= title (:title %))
                                                         (get-in % [:special :heap-breaker-dont-install]))
                                                   (all-active-installed state :runner))
                                         (not (get-in @state [:run :register (keyword (str "conspiracy-" title)) (:cid current-ice)]))))
                          :prompt (str "Install " title " from the heap?")
                          :choices (req ["Yes"
                                         "No"
                                         (when (pos? (count (filter #(= title (:title %)) (all-active-installed state :runner))))
                                           (str "Don't ask again while " title " is installed"))])
                          :async true
                          :effect (req
                                    (continue-ability
                                      state side
                                      (cond
                                        (= target "Yes") {:async true
                                                          :effect (effect (runner-install :runner (assoc eid :source card :source-type :runner-install) card nil))}
                                        ;; Add a register to note that the player was already asked about installing,
                                        ;; to prevent multiple copies from prompting multiple times.
                                        (= target "No") {:effect (req (swap! state assoc-in [:run :register (keyword (str "conspiracy-" title)) (:cid current-ice)] true))}
                                        ;; mark that we never want to install this heap breaker while another copy is installed
                                        :else {:effect (req
                                                         (let [heap-breakers (filter #(= title (:title %)) (all-active-installed state :runner))]
                                                           (vec (map #(update! state side (assoc-in % [:special :heap-breaker-dont-install] :true)) heap-breakers))))})
                                      card targets))}
                         card targets))}]})

(defn- pump-and-break
  "Paid ability for conspiracy breakers
  (Conspiracy suite: Black Orchestra, MKUltra, Paperclip)"
  [cost strength subtype]
  (merge
    (dissoc-req (break-sub cost strength subtype))
    {:label (str "add " strength " strength and "
                 " break up to "
                 (quantify strength (str subtype " subroutine")))
     :heap-breaker-pump strength ; strength gained
     :heap-breaker-break strength ; number of subs broken
     :cost cost
     :msg (msg "increase its strength from " (get-strength card)
               " to " (+ strength (get-strength card)))
     :effect (effect (pump card strength)
                     (continue-ability (break-sub nil strength subtype {:repeatable false}) (get-card state card) nil))
     :pump strength}))

(def heap-breaker-auto-pump-and-break
  "Implements auto-pump-and-break for heap breakers. Updates an icebreaker's
  abilities with a pseudo-ability to trigger the auto-pump routine in core,
  IF we are encountering an ice with a subtype we can break."
  {:effect
   (req (let [abs (remove #(or (= (:dynamic %) :auto-pump)
                               (= (:dynamic %) :auto-pump-and-break))
                          (:abilities card))
              current-ice (when-not (get-in @state [:end-run :ended])
                            (get-card state current-ice))
              ;; match strength
              can-pump (fn [ability]
                         (when (:heap-breaker-pump ability)
                           ((:req ability (req true)) state side eid card nil)))
              pump-ability (some #(when (can-pump %) %) (:abilities (card-def card)))
              can-break (fn [ability]
                          (when (:break-req ability)
                            ((:break-req ability) state side eid card nil)))
              break-ability (some #(when (can-break %) %) (:abilities (card-def card)))
              pump-strength-at-once (when pump-ability
                                      (:heap-breaker-pump pump-ability))
              subs-broken-at-once (when pump-ability
                                    (:heap-breaker-break pump-ability))
              strength-diff (when (and current-ice
                                       (get-strength current-ice)
                                       (get-strength card))
                              (max 0 (- (get-strength current-ice)
                                        (get-strength card))))
              unbroken-subs (count (remove :broken (:subroutines current-ice)))
              no-unbreakable-subs (empty? (filter #(if (fn? (:breakable %)) ; filter for possibly unbreakable subs
                                                     (not= :unrestricted ((:breakable %) state side eid current-ice [card]))
                                                     (not (:breakable % true))) ; breakable is a bool
                                                  (:subroutines current-ice)))
              x-number (when (and strength-diff unbroken-subs)
                         (max strength-diff unbroken-subs))
              x-breaker (= :x pump-strength-at-once)
              pumps-needed (when (and strength-diff pump-strength-at-once)
                             (if x-breaker
                               1
                               (int (Math/ceil (/ strength-diff pump-strength-at-once)))))
              breaks-needed (when (and unbroken-subs subs-broken-at-once)
                              (if x-breaker
                                1
                                (int (Math/ceil (/ unbroken-subs subs-broken-at-once)))))
              ability-uses-needed (when (and pumps-needed breaks-needed)
                                    (if x-breaker
                                      1
                                      (+ pumps-needed
                                         breaks-needed
                                         (if (pos? pumps-needed) -1 0)))) ;already broken once with last pump
              total-cost (when (and pump-ability
                                    ability-uses-needed)
                           (if x-breaker
                             [(->c :credit x-number)]
                             (repeat ability-uses-needed (:cost pump-ability))))]
          (update! state side
                   (assoc card :abilities
                          (if (and (seq total-cost)
                                   (active-encounter? state)
                                   pump-ability
                                   break-ability)
                            (vec (concat abs
                                         (when (and pump-ability
                                                    break-ability
                                                    no-unbreakable-subs
                                                    (pos? unbroken-subs)
                                                    (can-pay? state side eid card total-cost))
                                           [{:dynamic :auto-pump-and-break
                                             :cost total-cost
                                             :cost-label (build-cost-label total-cost)
                                             :label (str "Match strength and fully break "
                                                         (:title current-ice))}])))
                            abs)))))})

(defn- mu-based-strength
  "Strength depends on available memory units
  (Greek/Philosopher suite: Adept, Sage, Savant)"
  [abilities]
  {:abilities abilities
   :static-abilities [(breaker-strength-bonus (req (available-mu state)))]})

(defn- break-multiple-types
  "Single ability to break multiple types of ice
  (Greek/Philosopher suite: Adept, Sage, Savant)"
  [first-qty first-type second-qty second-type]
  (break-sub 2
             (req
               (cond (has-subtype? current-ice first-type) first-qty
                     (has-subtype? current-ice second-type) second-qty
                     :else (throw (ex-info "What are we encountering?" current-ice))))
             (hash-set first-type second-type)
             {:label (str "break "
                          (quantify first-qty (str first-type " subroutine")) " or "
                          (quantify second-qty (str second-type " subroutine")))}))

(defn- give-ice-subtype
  "Make currently encountered ice gain chosen type until end of encounter
  (Wrestling suite: Laamb, Engolo)"
  [cost ice-type abilities]
  (auto-icebreaker
    {:abilities abilities
     :events [{:event :encounter-ice
               :req (req (and (not-used-once? state {:once :per-turn} card)
                              (not (has-subtype? (:ice context) ice-type))
                              (can-pay? state :runner eid card nil [(->c :credit 2)])))
               :async true
               :effect
               (effect
                 (continue-ability
                   {:optional
                    {:prompt (str "Pay " cost
                                  " [Credits] to make " (:title (:ice context))
                                  " gain " ice-type "?")
                     :yes-ability
                     {:cost [(->c :credit cost)]
                      :msg (msg "make " (:title current-ice) " gain " ice-type)
                      :effect (effect (register-once {:once :per-turn} card)
                                      (register-lingering-effect
                                        card
                                        (let [ice current-ice]
                                          {:type :gain-subtype
                                           :duration :end-of-encounter
                                           :req (req (same-card? ice target))
                                           :value ice-type})))}}}
                   card nil))}]}))

(defn- virus-breaker
  "Spends virus counters from any card to pump/break, gains virus counters for successful runs
  (Khumalo suite: Musaazi, Yusuf)"
  [ice-type]
  (auto-icebreaker
    {:events [{:event :successful-run
               :silent (req true)
               :effect (effect (system-msg (str "places 1 virus counter on " (:title card)))
                               (add-counter card :virus 1))}]
     :abilities [(break-sub [(->c :any-virus-counter 1)] 1 ice-type)
                 (strength-pump [(->c :any-virus-counter 1)] 1)]}))

(defn- central-only
  "Break ability cannot be used on remote servers
  (Central suite: Alias, Breach, Passport)"
  [break pump]
  (auto-icebreaker
    {:abilities [(break-sub (:break-cost break) (:break break) (:breaks break)
                            {:req (req (and (#{:hq :rd :archives} (target-server run))
                                            (<= (get-strength current-ice) (get-strength card))
                                            (has-subtype? current-ice (first (:breaks break)))))})
                 pump]}))

(defn- return-and-derez
  "Return to grip to derez current ice
  (Bird suite: Golden, Peregrine, Saker)"
  [break pump]
  (let [ice-type (first (:breaks break))]
    (auto-icebreaker
      {:abilities [break
                   pump
                   {:label (str "Derez " ice-type " being encountered")
                    :cost [(->c :credit 2) (->c :return-to-hand)]
                    :req (req (and (get-current-encounter state)
                                   (rezzed? current-ice)
                                   (has-subtype? current-ice ice-type)
                                   (all-subs-broken-by-card? current-ice card)))
                    :msg (msg "derez " (:title current-ice))
                    :effect (effect (derez current-ice))}]})))

(defn- trash-to-bypass
  "Trash to bypass current ice
  (Fraud suite: Abagnale, Demara, Lustig)"
  [break pump]
  (let [ice-type (first (:breaks break))]
    (auto-icebreaker
      {:abilities [break
                   pump
                   {:label (str "Bypass " ice-type " being encountered")
                    :cost [(->c :trash-can)]
                    :req (req (and (active-encounter? state)
                                   (has-subtype? current-ice ice-type)))
                    :msg (msg "bypass " (:title current-ice))
                    :effect (req (bypass-ice state)
                                 (continue state :runner nil))}]})))

(defn- cloud-icebreaker
  "Reduce MU cost to 0 with 2+ link
  (Cloud subtype: Creeper, ZU.13 Key Master, B&E, GlobalSec)"
  [cdef]
  (update cdef :static-abilities conj {:type :used-mu
                                       :req (req (<= 2 (get-link state)))
                                       :value (req (- (:memoryunits card)))}))

(defn- break-and-enter
  "No MU with 2+ link, strength based on installed Icebreakers, trash to break 3 subs
  (Breaking and Entering suite: Crowbar, Shiv, Spike)"
  [ice-type]
  (auto-icebreaker
    (cloud-icebreaker
      {:abilities [(break-sub [(->c :trash-can)] 3 ice-type)]
       :static-abilities [(breaker-strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                                      (all-active-installed state :runner)))))]})))

(defn- global-sec-breaker
  "No MU with 2+ link, break any number of subs for 2, pump 2 for 3
  (GlobalSec suite: GS Strike M1, GS Shrike M2, GS Sherman M3)"
  [ice-type]
  (cloud-icebreaker (auto-icebreaker {:abilities [(break-sub 2 0 ice-type)
                                                  (strength-pump 2 3)]})))

;; Card definitions

(defcard "Abaasy"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 2 2)]
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (first-event? state side :subroutines-broken #(all-subs-broken-by-card? (first %) card))))
                              :async true
                              :effect (effect (continue-ability
                                                {:prompt "Choose 1 card in the grip to trash"
                                                 :waiting-prompt true
                                                 :async true
                                                 :choices {:card #(and (in-hand? %)
                                                                       (runner? %))}
                                                 :msg (msg "trash " (:title target) " to draw 1 card")
                                                 :effect (req (wait-for (trash state side target {:unpreventable true
                                                                                                  :cause-card card})
                                                                        (draw state :runner eid 1)))
                                                 :cancel-effect (effect (effect-completed eid))}
                                                card nil))}]}))

(defcard "Abagnale"
  (trash-to-bypass (break-sub 1 1 "Code Gate")
                   (strength-pump 2 2)))

(defcard "Adept"
  (mu-based-strength [(break-multiple-types
                        1 "Barrier"
                        1 "Sentry")]))

(defcard "Afterimage"
  (auto-icebreaker
    {:events [{:event :encounter-ice
               :interactive (req true)
               :optional
               {:req (req (and (has-subtype? (:ice context) "Sentry")
                               (can-pay? state :runner eid card nil [(->c :credit 2)])
                               (some #(has-subtype? % "Stealth")
                                     (all-active state :runner))))
                :once :per-turn
                :prompt (msg "Pay 2 [Credits] to bypass " (:title (:ice context)) "?")
                :yes-ability
                {:cost [(->c :credit 2 {:stealth :all-stealth})]
                 :msg (msg "bypass " (:title (:ice context)))
                 :effect (req (bypass-ice state))}}}]
     :abilities [(break-sub 1 2 "Sentry")
                 (strength-pump (->c :credit 1 {:stealth 1}) 2 :end-of-encounter)]}))

(defcard "Aghora"
  (swap-with-in-hand "Aghora"
                     {:req (req (and (<= 5 (value (first (get-rez-cost state side current-ice nil))))
                                     (<= (get-strength current-ice) (get-strength card))))}))

(defcard "Algernon"
  {:events
   [{:event :runner-turn-begins
     :optional
     {:prompt (msg "Pay 2 [Credits] to gain [Click]?")
      :req (req (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil [(->c :credit 2)]))
      :player :runner
      :yes-ability {:cost [(->c :credit 2)]
                    :msg "gain [Click]"
                    :effect (req (gain-clicks state :runner 1)
                                 (update! state :runner (assoc-in (get-card state card) [:special :used-algernon] true)))}}}
    {:event :runner-turn-ends
     :async true
     :effect (req (if (get-in card [:special :used-algernon])
                    (do
                      (update! state :runner (dissoc-in card [:special :used-algernon]))
                      (if-not (:successful-run runner-reg)
                        (do
                          (system-msg state :runner "trashes Algernon because a successful run did not occur")
                          (trash state :runner eid card {:cause-card card}))
                        (effect-completed state side eid)))
                    (effect-completed state side eid)))}]})

(defcard "Alias"
  (central-only (break-sub 1 1 "Sentry")
                (strength-pump 2 3)))

(defcard "Alpha"
  (auto-icebreaker {:abilities [(break-sub
                                 1 1 "All"
                                 {:req (req (let [server-ice (:ices (card->server state current-ice))]
                                              (same-card? current-ice (last server-ice))))})
                                (strength-pump 1 1)]}))

(defcard "Amina"
  (auto-icebreaker {:abilities [(break-sub 2 3 "Code Gate")
                                (strength-pump 2 3)]
                    :events [{:event :end-of-encounter
                              :req (req (and (all-subs-broken-by-card? (:ice context) card)
                                             (first-event? state side :end-of-encounter
                                                           (fn [targets]
                                                             (let [context (first targets)]
                                                               (all-subs-broken-by-card? (:ice context) card))))))
                              :msg "make the Corp lose 1 [Credits]"
                              :async true
                              :effect (effect (lose-credits :corp eid 1))}]}))

(defcard "Analog Dreamers"
  (let [ability (successful-run-replace-breach
                 {:target-server :rd
                  :duration :end-of-run
                  :ability
                  {:prompt "Choose a card to shuffle into R&D"
                   :choices {:card #(and (not (ice? %))
                                         (not (faceup? %))
                                         (zero? (get-counters % :advancement)))}
                   :msg (msg "shuffle " (card-str state target) " into R&D")
                   :effect (effect (move :corp target :deck)
                                   (shuffle! :corp :deck))}})]
    {:abilities [{:cost [(->c :click 1)]
                  :msg "make a run on R&D"
                  :makes-run true
                  :async true
                  :effect (effect (register-events card [ability])
                                  (make-run eid :rd card))}]}))

(defcard "Ankusa"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Barrier")
                                (strength-pump 1 1)]
                    :events [{:event :subroutines-broken
                              :req (req (and (has-subtype? target "Barrier")
                                             (all-subs-broken-by-card? target card)))
                              :msg (msg "add " (:title target)
                                        " to HQ after breaking all its subroutines")
                              :effect (effect (move :corp target :hand nil)
                                              (continue :runner nil))}]}))

(defcard "Atman"
  {:on-install {:cost [(->c :x-credits)]
                :msg (msg "place " (quantify (cost-value eid :x-credits) "power counter") " on itself")
                :effect (effect (add-counter card :power (cost-value eid :x-credits)))}
   :abilities [(break-sub 1 1 "All" {:req (req (= (get-strength current-ice) (get-strength card)))})]
   :static-abilities [(breaker-strength-bonus (req (get-counters card :power)))]})

(defcard "Au Revoir"
  {:events [{:event :jack-out
             :async true
             :effect (effect (gain-credits eid 1))
             :msg "gain 1 [Credits]"}]})

(defcard "Audrey v2"
  {:abilities [(break-sub [(->c :virus 1)] 2)
               (strength-pump [(->c :trash-from-hand 1)] 3)]
   :events [{:event :runner-trash
             :once-per-instance true
             :req (req (:accessed target))
             :effect (effect (add-counter :runner card :virus 1))
             :msg "place 1 virus counter on itself"}]})

(defcard "Aumakua"
  (auto-icebreaker {:implementation "[Erratum] Whenever you finish breaching a server, if you did not steal or trash any accessed cards, place 1 virus counter on this program."
                    :abilities [(break-sub 1 1)
                                {:label "Place 1 virus counter"
                                 :msg "manually place 1 virus counter on itself"
                                 :effect (effect (add-counter card :virus 1))}]
                    :static-abilities [(breaker-strength-bonus (req (get-virus-counters state card)))]
                    :events [{:event :end-breach-server
                              :req (req (not (or (:did-steal target)
                                                 (:did-trash target))))
                              :effect (effect (add-counter card :virus 1))}
                             {:event :expose
                              :effect (effect (add-counter card :virus 1))}]}))

(defcard "Aurora"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Barrier")
                                (strength-pump 2 3)]}))

(defcard "Baba Yaga"
  (let [host-click {:cost [(->c :click 1)]
                    :label "Install and host a non-AI icebreaker on Baba Yaga"
                    :prompt "Choose a non-AI icebreaker in the grip"
                    :choices {:card #(and (has-subtype? % "Icebreaker")
                                          (not (has-subtype? % "AI"))
                                          (in-hand? %))}
                    :async true
                    :effect (effect (runner-install eid target {:host-card card}))}
        host-free {:label "Host an installed non-AI icebreaker (manual)"
                   :prompt "Choose an installed non-AI icebreaker"
                   :choices {:card #(and (has-subtype? % "Icebreaker")
                                         (not (has-subtype? % "AI"))
                                         (installed? %))}
                   :effect (effect (host card target))}
        gain-abis (req (let [new-abis (mapcat (comp ability-init card-def) (:hosted card))]
                         (update! state :runner (assoc card :abilities (concat [host-click host-free] new-abis)))))]
    {:abilities [host-click host-free]
     :hosted-gained gain-abis
     :hosted-lost gain-abis}))

(defcard "Bankroll"
  {:implementation "Bankroll gains credits automatically."
   :events [{:event :successful-run
             :req (req (not (= "Jak Sinclair" (get-in run [:source-card :title])))) ;; TODO: dirty hack
             :msg "place 1 [Credit] on itself"
             :effect (effect (add-counter card :credit 1))}]
   :abilities [{:label "Take all hosted credits"
                :async true
                ;; Cannot trash unless there are counters (so game state changes)
                :req (req (pos? (get-counters card :credit)))
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :cost [(->c :trash-can)]
                :effect (effect (gain-credits eid (get-counters card :credit)))}]})

(defcard "Banner"
  (auto-icebreaker {:abilities [{:label "Prevent barrier subroutines from ending the run this encounter"
                                 :cost [(->c :credit 2)]
                                 :req (req (and (get-current-encounter state)
                                                 (<= (get-strength current-ice) (get-strength card))
                                                 (has-subtype? current-ice "Barrier")))
                                 :msg (msg "prevent " (card-str state current-ice) " from ending the run this encounter")
                                 :effect (req
                                           (let [target-ice (:ice (get-current-encounter state))]
                                             (register-lingering-effect
                                               state side
                                               card
                                               {:type :auto-prevent-run-end
                                                :duration :end-of-encounter
                                                :req (req
                                                       (let [target (second targets)]
                                                         (and (same-card? target target-ice)
                                                              ;;special case for border control/MIC
                                                              ;; this is an ugly hack, but we have
                                                              ;; no way of knowing which *ability*
                                                              ;; actually ended the run
                                                              ;; these seem like the safe hedge.
                                                              ;; MIC is included for paint effects.
                                                              ;; TODO - fix this, add :cause :subroutine to a bunch of
                                                              ;; end the run effects
                                                              (if (#{"Border Control" "M.I.C."} (:title target-ice))
                                                                (not (some #(and
                                                                              (same-card? target (:card (first %)))
                                                                              (= (:cause (first %)) :ability-cost))
                                                                           (run-events state :corp :corp-trash)))
                                                                true))))
                                                :value (req true)})))}]}))

(defcard "Battering Ram"
  (auto-icebreaker {:abilities [(break-sub 2 2 "Barrier")
                                (strength-pump 1 1 :end-of-run)]}))

(defcard "Begemot"
  (auto-icebreaker {:on-install {:async true
                                 :effect (effect (damage eid :brain 1 {:card card}))}
                    :abilities [(break-sub 1 0 "Barrier")]
                    :static-abilities [(breaker-strength-bonus (req (:brain-damage runner)))]}))

(defcard "Berserker"
  (auto-icebreaker {:events [{:event :encounter-ice
                              :req (req (has-subtype? (:ice context) "Barrier"))
                              :msg (msg "gain " (count (:subroutines (:ice context))) " strength")
                              :effect (effect (pump card (count (:subroutines (:ice context)))))}]
                    :abilities [(break-sub 2 2 "Barrier")]}))

(defcard "Bishop"
  {:implementation "[Erratum] Program: Caïssa - Trojan"
   :abilities [{:cost [(->c :click 1)]
                :label "Host on another piece of ice"
                :effect (req (let [b (get-card state card)
                                   hosted? (ice? (:host b))
                                   remote? (is-remote? (second (get-zone (:host b))))]
                               (continue-ability
                                 state side
                                 {:prompt (msg "Choose a piece of ice protecting "
                                               (if hosted? (if remote? "a central" "a remote") "any") " server")
                                  :choices {:card #(if hosted?
                                                     (and (if remote?
                                                            (is-central? (second (get-zone %)))
                                                            (is-remote? (second (get-zone %))))
                                                          (ice? %)
                                                          (can-host? %)
                                                          (= (last (get-zone %)) :ices)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa"))
                                                                    (:hosted %)))
                                                     (and (ice? %)
                                                          (can-host? %)
                                                          (= (last (get-zone %)) :ices)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa"))
                                                                    (:hosted %))))}
                                  :msg (msg "host itself on " (card-str state target))
                                  :effect (effect (host target card))}
                                 card nil)))}]
   :static-abilities [{:type :ice-strength
                       :req (req (and (= (:cid target)
                                         (:cid (:host card)))
                                      (:rezzed target)))
                       :value -2}]})

(defcard "Black Orchestra"
   (let [events (for [event [:run :approach-ice :encounter-ice :pass-ice :run-ends
                             :ice-strength-changed :ice-subtype-changed :breaker-strength-changed
                             :subroutines-changed]]
                  (assoc heap-breaker-auto-pump-and-break :event event))
         cdef (install-from-heap "Black Orchestra" "Code Gate"
                                 [(pump-and-break [(->c :credit 3)] 2 "Code Gate")])]
     (assoc cdef :events (apply conj events (:events cdef)))))

(defcard "BlacKat"
  (auto-icebreaker {:implementation "Stealth credit restriction not enforced"
                    :abilities [(break-sub 1 1 "Barrier")
                                (break-sub (->c :credit 1 {:stealth 1}) 3 "Barrier")
                                (strength-pump 2 1)
                                (strength-pump (->c :credit 2 {:stealth 1}) 2 :end-of-encounter)]}))

(defcard "Blackstone"
  (auto-icebreaker
    {:abilities
     [(break-sub 1 1 "Barrier")
      (strength-pump (->c :credit 3 {:stealth 1}) 4 :end-of-run)]}))

(defcard "Boi-tatá"
  (letfn [(was-a-runner-card?
            [target]
            (runner? (:card (first target))))]
    {:static-abilities [{:type :card-ability-cost
                         :req (req (and (same-card? card (:card context))
                                        (not (no-event? state side :runner-trash was-a-runner-card?))))
                         :value (->c :credit -1)}]
     :abilities [(break-sub 2 2 "Sentry")
                 (strength-pump 3 3)]}))

(defcard "Botulus"
  {:implementation "[Erratum] Program: Virus - Trojan"
   :data {:counter {:virus 1}}
   :hosting {:req (req (and (ice? target)
                            (can-host? state target)))}
   :events [{:event :runner-turn-begins
             :effect (effect (add-counter card :virus 1))}]
   :abilities [(break-sub
                 [(->c :virus 1)] 1 "All"
                 {:req (req (same-card? current-ice (:host card)))})]})

(defcard "Brahman"
  (auto-icebreaker {:abilities [(break-sub 1 2 "All")
                                (strength-pump 2 1)]
                    :events [{:event :end-of-encounter
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :player :runner ; Needed for when the run is ended by the Corp
                              :prompt "Choose a non-virus program to add to the top of the stack"
                              :choices {:card #(and (installed? %)
                                                    (program? %)
                                                    (not (facedown? %))
                                                    (not (has-subtype? % "Virus")))}
                              :msg (msg "add " (:title target) " to the top of the stack")
                              :effect (effect (move (get-card state target) :deck {:front true}))}]}))

(defcard "Breach"
  (central-only (break-sub 2 3 "Barrier")
                (strength-pump 2 4)))

(defcard "Bug"
  {:req (req (some #{:hq} (:successful-run runner-reg)))
   :events [{:event :corp-draw
             :optional
             {:prompt "Pay credits to reveal drawn card?"
              :req (req (< 1 (total-available-credits state :runner eid card)))
              :yes-ability
              {:prompt "How many cards do you want to reveal for 2 [Credits] each?"
               :waiting-prompt true
               :choices {:number (req (min (:count context)
                                           (quot (total-available-credits state :runner eid card) 2)))}
               :async true
               :effect (req (let [cards (->> corp-currently-drawing
                                             (shuffle)
                                             (keep #(find-cid (:cid %) (:set-aside corp)))
                                             (take target))]
                              (wait-for
                                (pay state side (make-eid state eid) card [(->c :credit (* 2 target))])
                                (let [payment-str (:msg async-result)]
                                  (wait-for
                                    (reveal state side (make-eid state eid) cards)
                                    (system-msg state side (str payment-str
                                                                " to use " (:title card)
                                                                " to force the Corp to reveal they drew "
                                                                (enumerate-str (map :title cards))))
                                    (effect-completed state side eid))))))}}}]})

(defcard "Bukhgalter"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 1)]
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (first-event? state side :subroutines-broken #(all-subs-broken-by-card? (first %) card))))
                              :msg "gain 2 [Credits]"
                              :async true
                              :effect (effect (gain-credits :runner eid 2))}]}))

(defcard "Buzzsaw"
  (auto-icebreaker {:abilities [(break-sub 1 2 "Code Gate")
                                (strength-pump 3 1)]}))

(defcard "Cache"
  {:abilities [{:cost [(->c :virus 1)]
                :async true
                :effect (effect (gain-credits eid 1))
                :msg "gain 1 [Credits]"}]
   :data {:counter {:virus 3}}})

(defcard "Carmen"
   (auto-icebreaker {:install-cost-bonus (req (if (:successful-run runner-reg) -2 0))
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 3)]}))

(defcard "Cerberus \"Cuj.0\" H3"
  (power-counter-break "Sentry"))

(defcard "Cerberus \"Lady\" H1"
  (power-counter-break "Barrier"))

(defcard "Cerberus \"Rex\" H2"
  (power-counter-break "Code Gate"))

(defcard "Cezve"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (:run @state)
                                               (is-central? (:server run))))
                                :type :recurring}}})

(defcard "Chakana"
  {:static-abilities [{:type :advancement-requirement
                       :req (req (<= 3 (get-virus-counters state card)))
                       :value 1}]
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :rd (target-server context)))
             :effect (effect (add-counter card :virus 1))}]})

(defcard "Chameleon"
  (auto-icebreaker {:on-install {:prompt "Choose one"
                                 :choices ["Barrier" "Code Gate" "Sentry"]
                                 :msg (msg "choose " target)
                                 :effect (effect (update! (assoc card :subtype-target target)))}
                    :events [{:event :runner-turn-ends
                              :msg "add itself to Grip"
                              :interactive (req true)
                              :effect (effect (move card :hand))}]
                    :abilities [(break-sub 1 1 "All" {:req (req (if-let [subtype (:subtype-target card)]
                                                                  (has-subtype? current-ice subtype)
                                                                  true))})]}))

(defcard "Chisel"
  {:implementation "[Erratum] Program: Virus - Trojan"
   :hosting {:req (req (and (ice? target)
                            (installed? target)
                            (can-host? state target)))}
   :static-abilities [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value (req (- (get-virus-counters state card)))}]
   :events [{:event :encounter-ice
             :req (req (same-card? (:ice context) (:host card)))
             :async true
             :effect (req (if (pos? (ice-strength state side (:ice context)))
                            (do (system-msg state side (str "uses " (:title card) " to place 1 virus counter on itself"))
                                (add-counter state side card :virus 1)
                                (effect-completed state side eid))
                            (do (system-msg state side (str "uses " (:title card) " to trash " (card-str state (:ice context))))
                                (trash state side eid (:ice context) {:cause-card card}))))}]})

(defcard "Cat's Cradle"
  (auto-icebreaker
    {:static-abilities [{:type :rez-cost
                         :req (req (and (ice? target) (has-subtype? target "Code Gate")))
                         :value 1}]
     :abilities [(break-sub 1 1 "Code Gate")
                 (strength-pump 1 1)]}))

(defcard "Cleaver"
  (auto-icebreaker {:abilities [(break-sub 1 2 "Barrier")
                                (strength-pump 2 1)]}))

(defcard "Cloak"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Clot"
  {:on-install
   {:effect (req (let [agendas (->> (turn-events state :corp :corp-install)
                                    (map #(:card (first %)))
                                    (filter agenda?))]
                   (swap! state assoc-in [:corp :register :cannot-score] agendas)))}
   :events [{:event :purge
             :async true
             :msg "trash itself"
             :effect (req (swap! state update-in [:corp :register] dissoc :cannot-score)
                          (trash state :runner eid card {:cause :purge
                                                         :cause-card card}))}
            {:event :corp-install
             :req (req (agenda? (:card context)))
             :effect (req (swap! state update-in [:corp :register :cannot-score] #(cons (:card context) %)))}]
   :leave-play (req (swap! state update-in [:corp :register] dissoc :cannot-score))})

(defcard "Coalescence"
  {:abilities [{:cost [(->c :power 1)]
                :req (req (= (:active-player @state) :runner))
                :async true
                :keep-menu-open :while-power-tokens-left
                :effect (effect (gain-credits eid 2))
                :msg "gain 2 [Credits]"}]
   :data {:counter {:power 2}}})

(defcard "Collective Consciousness"
  {:events [{:event :rez
             :req (req (ice? (:card target)))
             :msg "draw 1 card"
             :async true
             :effect (effect (draw :runner eid 1))}]})

(defcard "Conduit"
  {:events [{:event :run-ends
             :optional
             {:req (req (and (:successful context)
                             (= :rd (target-server context))))
              :player :runner
              :waiting-prompt true
              :autoresolve (get-autoresolve :auto-place-counter)
              :prompt (msg "Place 1 virus counter on " (:title card) "?")
              :yes-ability {:msg "place 1 virus counter on itself"
                            :effect (effect (add-counter card :virus 1))}
              :no-ability {:effect (effect (system-msg (str "declines to use " (:title card) " to place 1 virus counter on itself")))}}}
            {:event :successful-run
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :rd (max 0 (get-virus-counters state card)) {:duration :end-of-run})]))}]
   :abilities [{:cost [(->c :click 1)]
                :msg "make a run on R&D"
                :makes-run true
                :async true
                :effect (req (make-run state side eid :rd card))}
               (set-autoresolve :auto-place-counter "Conduit placing virus counters on itself")]})

(defcard "Consume"
  {:events [{:event :runner-trash
             :once-per-instance true
             :async true
             :req (req (some #(corp? (:card %)) targets))
             :effect (req (let [amt-trashed (count (filter #(corp? (:card %)) targets))
                                sing-ab {:optional {:prompt (msg "Place 1 virus counter on " (:title card) "?")
                                                    :autoresolve (get-autoresolve :auto-place-counter)
                                                    :yes-ability {:effect (effect (add-counter :runner card :virus 1))
                                                                  :msg "place 1 virus counter on itself"}}}
                                mult-ab {:prompt (msg "Place virus counters on " (:title card) "?")
                                         :choices {:number (req amt-trashed)
                                                   :default (req amt-trashed)}
                                         :msg (msg "place " (quantify target "virus counter") " on itself")
                                         :effect (effect (add-counter :runner card :virus target))}
                                ab (if (= 1 amt-trashed) sing-ab mult-ab)]
                            (continue-ability state side ab card targets)))}]
   :abilities [{:req (req (pos? (get-virus-counters state card)))
                :cost [(->c :click 1)]
                :label "Gain 2 [Credits] for each hosted virus counter, then remove all virus counters"
                :async true
                :effect (req (wait-for (gain-credits state side (* 2 (get-virus-counters state card)))
                                       (update! state side (assoc-in card [:counter :virus] 0))
                                       (doseq [h (filter #(= "Hivemind" (:title %)) (all-active-installed state :runner))]
                                         (update! state side (assoc-in h [:counter :virus] 0)))
                                       (effect-completed state side eid)))
                :msg (msg (let [local-virus (get-counters card :virus)
                                global-virus (get-virus-counters state card)
                                hivemind-virus (- global-virus local-virus)]
                            (str "gain " (* 2 global-virus) " [Credits], removing " (quantify local-virus "virus counter") " from itself"
                                 (when (pos? hivemind-virus)
                                   (str " (and " hivemind-virus " from Hivemind)")))))}
               (set-autoresolve :auto-place-counter "Consume placing virus counters on itself")]})

(defcard "Copycat"
  {:abilities [{:async true
                :req (req (and run
                               (:rezzed current-ice)))
                :prompt (msg "Choose a rezzed copy of " (:title current-ice))
                :choices {:req (req (and (rezzed? target)
                                         (ice? target)
                                         (= (:title target) (:title current-ice))))}
                :msg "redirect the run"
                :effect (req (let [dest (second (get-zone target))
                                   tgtndx (card-index state target)]
                               (swap! state
                                      update :run
                                      assoc :position tgtndx :server [dest])
                               (set-current-ice state)
                               (trash state side eid card {:unpreventable true
                                                           :cause-card card})))}]})

(defcard "Cordyceps"
  {:data {:counter {:virus 2}}
   :events [{:event :successful-run
             :interactive (req true)
             :optional
             {:req (req (and (is-central? (target-server context))
                             (can-pay? state side eid card nil [(->c :virus 1)])
                             (not-empty run-ices)
                             (<= 2 (count (filter ice? (all-installed state :corp))))))
              :once :per-turn
              :prompt "Swap 2 pieces of ice?"
              :yes-ability
              {:prompt "Choose a piece of ice protecting this server"
               :choices {:req (req (and (installed? target)
                                        (ice? target)
                                        (= (target-server (:run @state)) (second (get-zone target)))))}
               :async true
               :effect (effect
                         (continue-ability
                           (let [first-ice target]
                             {:prompt "Choose a piece of ice to swap with"
                              :choices {:req (req (and (installed? target)
                                                       (ice? target)
                                                       (not= first-ice target)))}
                              :msg (msg "swap the positions of " (card-str state first-ice)
                                        " and " (card-str state target))
                              :async true
                              :effect (req (wait-for (pay state side (make-eid state eid) card [(->c :virus 1)])
                                                     (system-msg state side (:msg async-result))
                                                     (swap-ice state side first-ice target)
                                                     (effect-completed state side eid)))})
                           card nil))}}}]})

(defcard "Corroder"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 1 1)]}))

(defcard "Cradle"
  (auto-icebreaker {:abilities [(break-sub 2 0 "Code Gate")]
                    :static-abilities [(breaker-strength-bonus (req (- (count (:hand runner)))))]}))

(defcard "Creeper"
  (cloud-icebreaker
    (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")
                                  (strength-pump 1 1)]})))

(defcard "Crescentus"
  {:abilities [{:req (req (and (get-current-encounter state)
                               (rezzed? current-ice)
                               (all-subs-broken? current-ice)))
                :label "derez an ice"
                :cost [(->c :trash-can)]
                :msg (msg "derez " (:title current-ice))
                :effect (effect (derez current-ice))}]})

(defcard "Crowbar"
  (break-and-enter "Code Gate"))

(defcard "Crypsis"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All")
                                (strength-pump 1 1)
                                {:cost [(->c :click 1)]
                                 :keep-menu-open :while-clicks-left
                                 :msg "place 1 virus counter on itself"
                                 :effect (effect (add-counter card :virus 1))}]
                    :events [{:event :end-of-encounter
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :msg (msg (if (can-pay? state side eid card nil [(->c :virus 1)])
                                          "remove 1 hosted virus counter"
                                          "trash itself"))
                              :async true
                              :effect (req (wait-for (pay state :runner (make-eid state eid) card [(->c :virus 1)])
                                                     (if-let [payment-str (:msg async-result)]
                                                       (do (system-msg state :runner payment-str)
                                                           (effect-completed state side eid))
                                                       (trash state side eid card {:cause-card card}))))}]}))

(defcard "Cupellation"
  {:events [;; Note - we don't actually have access events for inactive cards in archives
            ;; because of this, we need to perform this evil hack - nbkelly, jan '24
            {:event :end-breach-server
             :async true
             :interactive (req true)
             :req (req (and (= (:from-server target) :archives)
                            ;; can-pay-credit           ;;todo - assert that we can pay the credit
                            (seq (filter #(and (:seen %) (not (agenda? %))) (:discard corp)))
                            (empty? (filter corp? (:hosted card)))))
             :prompt "1 [Credits]: Host a card from archives?"
             :choices (req (cancellable (filter #(and (:seen %) (not (agenda? %)))
                                                (:discard corp))
                                        :sorted))
             :cost [(->c :credit 1)]
             :msg (msg "host " (:title target) " on itself")
             :effect (req (disable-card state side (get-card state target))
                          (host state side (assoc card :seen true) target)
                          (effect-completed state side eid))}
            {:event :breach-server
             :async true
             :optional {:req (req (and (= :hq target)
                                       (seq (filter corp? (:hosted card)))))
                        :prompt "Trash this program to access 2 additional cards from HQ?"
                        :yes-ability {:async true
                                      :effect (effect (access-bonus :hq 2)
                                                      (effect-completed eid))
                                      :cost [(->c :credit 1) (->c :trash-can)]
                                      :msg "access 2 additional cards from HQ"}}}]
   :interactions {:access-ability {:label "Host card"
                                   :trash? false
                                   :req (req (and (empty? (filter corp? (:hosted card)))
                                                  (not (agenda? target))))
                                   :cost [(->c :credit 1)]
                                   :msg (msg "host " (:title target) " on itself")
                                   :async true
                                   :effect (req
                                             (disable-card state side (get-card state target))
                                             (host state side (assoc card :seen true)
                                                   (get-card state target))
                                             (swap! state dissoc :access)
                                             (effect-completed state side eid))}}})

(defcard "Curupira"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 1 1)]
                    :interactive (req true)
                    :events [{:event :encounter-ice
                              :optional {:prompt (msg "Spend 3 power counters to bypass " (card-str state current-ice) "?")
                                         :waiting-prompt true
                                         :req (req (and
                                                     (has-subtype? (:ice context) "Barrier")
                                                     (<= 3 (get-counters (get-card state card) :power))))
                                         :yes-ability {:cost [(->c :power 3)]
                                                       :msg (msg "bypass " (card-str state current-ice))
                                                       :effect (req (bypass-ice state))}}}

                             {:event :subroutines-broken
                              :req (req (all-subs-broken-by-card? target card))
                              :msg "place 1 power counter on itself"
                              :async true
                              :effect (effect (add-counter card :power 1)
                                              (effect-completed eid))}]}))

(defcard "Customized Secretary"
  (letfn [(custsec-host [cards]
            (if (empty? (filter program? cards))
              {:msg "shuffle the stack"
               :effect (effect (shuffle! :deck))}
              {:prompt "Choose a program to host"
               :choices (concat (filterv program? cards) ["Done"])
               :async true
               :effect (req (if (= target "Done")
                              (do (shuffle! state side :deck)
                                  (system-msg state side "shuffles the stack")
                                  (effect-completed state side eid))
                              (do (host state side (get-card state card) target)
                                  (system-msg state side (str "hosts " (:title target) " on Customized Secretary"))
                                  (continue-ability state side (custsec-host (remove-once #(= % target) cards))
                                                    card nil))))}))]
    {:on-install {:async true
                  :interactive (req (some #(card-flag? % :runner-install-draw true) (all-active state :runner)))
                  :msg (msg "reveal " (enumerate-str (map :title (take 5 (:deck runner)))) " from the top of the stack")
                  :waiting-prompt true
                  :effect (req (let [from (take 5 (:deck runner))]
                                 (wait-for (reveal state side from)
                                           (continue-ability state side (custsec-host from) card nil))))}
     :abilities [{:cost [(->c :click 1)]
                  :keep-menu-open :while-clicks-left
                  :label "Install a hosted program"
                  :prompt "Choose a program to install"
                  :choices (req (cancellable (filter #(can-pay? state side (assoc eid :source card :source-type :runner-install)
                                                                % nil [(->c :credit (install-cost state side %))])
                                                     (:hosted card))))
                  :msg (msg "install " (:title target))
                  :async true
                  :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]}))

(defcard "Cyber-Cypher"
  (auto-icebreaker
    {:on-install {:prompt "Choose a server"
                  :msg (msg "target " target)
                  :choices (req servers)
                  :effect (effect (update! (assoc card :card-target target)))}
     :leave-play (effect (update! (dissoc card :card-target)))
     :abilities [(break-sub 1 1 "Code Gate" {:req (req (if (:card-target card)
                                                         (#{(last (server->zone state (:card-target card)))} (target-server run))
                                                         true))})
                 (strength-pump 1 1 :end-of-encounter {:req (req (if (:card-target card)
                                                                   (#{(last (server->zone state (:card-target card)))} (target-server run))
                                                                   true))})]}))

(defcard "D4v1d"
  (let [david-req (req (<= 5 (get-strength current-ice)))]
    {:data {:counter {:power 3}}
     :abilities [(break-sub [(->c :power 1)] 1 "All" {:req david-req})]}))

(defcard "Dagger"
  (auto-icebreaker
    {:abilities
     [(break-sub 1 1 "Sentry")
      (strength-pump (->c :credit 1 {:stealth 1}) 5 :end-of-encounter)]}))

(defcard "Dai V"
  (auto-icebreaker {:abilities [(break-sub [(->c :credit 2 {:stealth :all-stealth})] 0
                                           "All" {:all true})
                                (strength-pump 1 1)]}))

(defcard "Darwin"
  (auto-icebreaker {:flags {:runner-phase-12 (req true)}
                    :x-fn (req (get-virus-counters state card))
                    :abilities [(break-sub 2 1)
                                {:label "Place 1 virus counter (start of turn)"
                                 :once :per-turn
                                 :cost [(->c :credit 1)]
                                 :msg "place 1 virus counter on itself"
                                 :req (req (:runner-phase-12 @state))
                                 :effect (effect (add-counter card :virus 1))}]
                    :static-abilities [(breaker-strength-bonus (get-x-fn))]}))

(defcard "Datasucker"
  {:events [{:event :successful-run
             :silent (req true)
             :req (req (is-central? (target-server context)))
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:cost [(->c :virus 1)]
                :label "Give -1 strength to current piece of ice"
                :req (req (and (rezzed? current-ice)
                               (get-current-encounter state)))
                :msg (msg "give -1 strength to " (:title current-ice))
                :effect (effect (pump-ice current-ice -1))}]})

(defcard "DaVinci"
  {:events [{:event :successful-run
             :silent (req true)
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:req (req (some #(and (or (hardware? %)
                                          (program? %)
                                          (resource? %))
                                      (runner-can-install? state side % nil)
                                      (<= (install-cost state side %) (get-counters card :power)))
                                (:hand runner)))
                :label "install a card from the grip"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect
                          (continue-ability
                            {:waiting-prompt true
                             :prompt "Choose a card to install"
                             :msg (msg "install " (:title target) " from the grip at no cost")
                             :choices {:req (req (and (in-hand? target)
                                                      (or (hardware? target)
                                                          (program? target)
                                                          (resource? target))
                                                      (<= (install-cost state side target)
                                                          (get-counters (cost-target eid :trash-can) :power))))}
                             :async true
                             :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                             target {:ignore-install-cost true}))}
                            card nil))}]})

(defcard "Deep Thought"
  {:events [{:event :successful-run
             :silent (req true)
             :effect (effect (add-counter card :virus 1))
             :req (req (= :rd (target-server context)))}
            {:event :runner-turn-begins
             :req (req (>= (get-virus-counters state card) 3))
             :msg "look at the top card of R&D"
             :effect (effect (continue-ability
                               {:prompt (req (->> corp :deck first :title (str "The top card of R&D is ")))
                                :choices ["OK"]}
                               card nil))}]})

(defcard "Demara"
  (trash-to-bypass (break-sub 2 2 "Barrier")
                   (strength-pump 2 3)))

(defcard "Deus X"
  {:interactions {:prevent [{:type #{:net}
                             :req (req true)}]}
   :abilities [(break-sub [(->c :trash-can)] 0 "AP")
               {:msg "prevent any amount of net damage"
                :cost [(->c :trash-can)]
                :effect (effect (damage-prevent :net Integer/MAX_VALUE))}]})

(defcard "Dhegdheer"
  {:abilities [{:req (req (and (not (get-in card [:special :dheg-prog]))
                               (some #(and (program? %)
                                           (runner-can-install? state side % false)
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [(->c :credit (install-cost state side % {:cost-bonus -1}))]))
                                     (:hand runner))))
                :cost [(->c :click 1)]
                :label "Install and host a program"
                :prompt "Choose a program in the grip"
                :choices
                {:req (req (and (program? target)
                                (runner-can-install? state side target false)
                                (in-hand? target)
                                (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                          [(->c :credit (install-cost state side target {:cost-bonus -1}))])))}
                :msg (msg (str "install and host " (:title target)
                               (when (-> target :cost pos?)
                                 ", lowering its cost by 1 [Credit]")))
                :async true
                :effect (effect (update! (assoc-in card [:special :dheg-prog] (:cid target)))
                                (runner-install (assoc eid :source (get-card state card) :source-type :runner-install)
                                                target {:host-card (get-card state card)
                                                        :no-mu true
                                                        :cost-bonus -1}))}
               {:label "Host an installed program with [Credit] discount (manual)"
                :req (req (nil? (get-in card [:special :dheg-prog])))
                :prompt "Choose an installed program"
                :choices {:card #(and (program? %)
                                      (installed? %))}
                :msg (msg (str "host " (:title target)
                               (when (-> target :cost pos?)
                                 ", lowering its cost by 1 [Credit]")))
                :async true
                :effect (req (let [c (if (-> target :cost pos?) 1 0)]
                               (wait-for (gain-credits state side c)
                                         (host state side card (get-card state target))
                                         (unregister-effects-for-card state side target #(= :used-mu (:type %)))
                                         (update-mu state)
                                         (update! state side (assoc-in (get-card state card) [:special :dheg-prog] (:cid target)))
                                         (update-breaker-strength state side target)
                                         (effect-completed state side eid))))}
               {:label "Host an installed program (manual)"
                :req (req (nil? (get-in card [:special :dheg-prog])))
                :prompt "Choose an installed program"
                :choices {:card #(and (program? %)
                                      (installed? %))}
                :msg (msg (str "host " (:title target)))
                :effect (effect (host card (get-card state target))
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu)
                                (update-breaker-strength target)
                                (update! (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}]})

(defcard "Disrupter"
  {:events
   [{:event :initialize-trace
     :trash-icon true
     :optional
     {:player :runner
      :waiting-prompt true
      :prompt "Trash Disrupter to reduce the base trace strength to 0?"
      :yes-ability
      {:cost [(->c :trash-can)]
       :effect (req (force-base state 0))}}}]})

(defcard "Diwan"
  {:on-install {:prompt "Choose a server"
                :choices (req servers)
                :effect (effect (update! (assoc card :card-target target)))}
   :static-abilities [{:type :install-cost
                       :req (req (let [serv (:server (second targets))]
                                   (= serv (:card-target card))))
                       :value 1}]
   :events [{:event :purge
             :async true
             :msg "trash itself"
             :effect (req (trash state :runner eid card {:cause :purge
                                                         :cause-card card}))}]})

(defcard "Djinn"
  {:abilities [{:label "Search the stack for a virus program and add it to the grip"
                :prompt "Choose a Virus"
                :msg (msg "add " (:title target) " from the stack to the grip")
                :choices (req (cancellable (filter #(and (program? %)
                                                         (has-subtype? % "Virus"))
                                                   (:deck runner)) :sorted))
                :cost [(->c :click 1) (->c :credit 1)]
                :keep-menu-open :while-clicks-left
                :effect (effect (trigger-event :searched-stack)
                                (shuffle! :deck)
                                (move target :hand))}
               {:label "Install and host a non-Icebreaker program"
                :cost [(->c :click 1)]
                :prompt "Choose a non-Icebreaker program"
                :choices {:req (req (and (program? target)
                                         (runner-can-install? state side target false)
                                         (not (has-subtype? target "Icebreaker"))
                                         (in-hand? target)))}
                :msg (msg "install from the grip and host " (:title target))
                :async true
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed non-Icebreaker program (manual)"
                :prompt "Choose an installed non-Icebreaker program"
                :choices {:card #(and (program? %)
                                      (not (has-subtype? % "Icebreaker"))
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]})

(defcard "Eater"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All" {:additional-ability {:msg "access not more than 0 cards for the remainder of this run"
                                                                           :effect (req (max-access state 0))}
                                                      :label "break 1 subroutine and access 0 cards"})
                                (strength-pump 1 1)]}))

(defcard "Echelon"
  (auto-icebreaker {:static-abilities [(breaker-strength-bonus (req (count (filter #(and (program? %)
                                                                                         (has-subtype? % "Icebreaker"))
                                                                                   (all-active-installed state :runner)))))]
                    :abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 3 2)]}))

(defcard "Egret"
  {:implementation "[Erratum] Program: Trojan"
   :hosting {:req (req (and (ice? target)
                            (can-host? state target)
                            (rezzed? target)))}
   :on-install {:msg (msg "make " (card-str state (:host card))
                          " gain Barrier, Code Gate and Sentry subtypes")}
   :static-abilities [{:type :gain-subtype
                       :req (req (same-card? target (:host card)))
                       :value ["Barrier" "Code Gate" "Sentry"]}]})

(defcard "Endless Hunger"
  {:implementation "ETR restriction not implemented"
   :abilities [(break-sub [(->c :trash-installed 1)] 1 "All" {:label "break 1 \"[Subroutine] End the run.\" subroutine"})]})

(defcard "Engolo"
  (give-ice-subtype 2 "Code Gate"
                    [(break-sub 1 1 "Code Gate")
                     (strength-pump 2 4)]))

(defcard "Equivocation"
  (let [force-draw (fn [title]
                     {:optional
                      {:prompt (str "Force the Corp to draw " title "?")
                       :yes-ability
                       {:async true
                        :effect (req (system-msg state :corp (str "is forced to draw " title))
                                     (draw state :corp eid 1))}}})
        rvl {:optional
             {:prompt "Reveal the top card of R&D?"
              :yes-ability
              {:async true
               :effect (req (let [topcard (-> corp :deck first :title)]
                              (wait-for
                                (reveal state side topcard)
                                (system-msg state :runner (str "reveals " topcard
                                                               " from the top of R&D"))
                                (continue-ability state side (force-draw topcard) card nil))))}}}]
    {:events [{:event :successful-run
               :req (req (= :rd (target-server context)))
               :async true
               :interactive (req true)
               :waiting-prompt true
               :effect (effect (continue-ability rvl card nil))}]}))

(defcard "Euler"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Code Gate" {:req (req (= :this-turn (installed? card)))})
                                (break-sub 2 2 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "eXer"
  {:events [(breach-access-bonus :rd 1)
            {:event :purge
             :async true
             :msg "trash itself"
             :effect (req (trash state :runner eid card {:cause :purge
                                                         :cause-card card}))}]})

(defcard "Expert Schedule Analyzer"
  (let [ability (successful-run-replace-breach
                 {:target-server :hq
                  :duration :end-of-run
                  :ability
                  {:msg (msg "reveal " (enumerate-str (map :title (:hand corp))) " from HQ")
                   :async true
                   :effect (effect (reveal eid (:hand corp)))}})]
    {:abilities [{:cost [(->c :click 1)]
                  :msg "make a run on HQ"
                  :makes-run true
                  :async true
                  :effect (effect (register-events card [ability])
                                  (make-run eid :hq card))}]}))

(defcard "Faerie"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Sentry")
                                (strength-pump 1 1)]
                    :events [{:event :end-of-encounter
                              :async true
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :msg (msg "trash " (:title card))
                              :effect (effect (trash eid card {:cause :runner-ability
                                                               :cause-card card}))}]}))

(defcard "False Echo"
  {:events [{:event :pass-ice
             :optional
             {:req (req (not (rezzed? (:ice context))))
              :prompt (msg "Trash " (:title card) " to make the Corp rez the passed piece of ice or add it to HQ?")
              :yes-ability
              {:async true
               :msg "force the Corp to either rez the passed piece of ice or add it to HQ"
               :effect
               (req (wait-for
                      (trash state side card nil)
                      (continue-ability
                        state side
                        (let [ice (:ice context)]
                          {:async true
                           :prompt "Choose one"
                           :waiting-prompt true
                           :player :corp
                           :choices (req [(when (can-pay? state :runner eid card nil [(->c :credit (rez-cost state side ice))])
                                            (str "Rez " (get-title ice)))
                                          (str "Add " (get-title ice) " to HQ")])
                           :effect (req (if (= target (str "Rez " (get-title ice)))
                                          (rez state side eid ice)
                                          (do (system-msg state :corp "adds the passed piece of ice to HQ")
                                              (move state :corp ice :hand)
                                              (effect-completed state side eid))))})
                        card target)))}}}]})

(defcard "Faust"
  {:abilities [(break-sub [(->c :trash-from-hand 1)] 1)
               (strength-pump [(->c :trash-from-hand 1)] 2)]})

(defcard "Fawkes"
  {:abilities [(break-sub 1 1 "Sentry")
               {:label "+X strength for the remainder of the run (using at least 1 stealth [Credits])"
                :cost [(->c :x-credits 0 {:stealth 1})]
                :prompt "How many credits do you want to spend?"
                :effect (effect (pump card (cost-value eid :x-credits) :end-of-run))
                :msg (msg "increase strength by " (cost-value eid :x-credits)
                          " for the remainder of the run")}]})

(defcard "Femme Fatale"
  (auto-icebreaker
    {:on-install
     {:prompt "Choose a piece of ice to target for bypassing"
      :choices {:card ice?}
      :effect (req (let [ice target]
                     (add-icon state side card ice "FF" (faction-label card))
                     (system-msg state side
                                 (str "selects " (card-str state ice)
                                      " for " (:title card) "'s bypass ability"))
                     (register-events
                       state side card
                       [{:event :encounter-ice
                         :interactive (req true)
                         :optional
                         {:req (req (and (same-card? ice (:ice context))
                                         (can-pay? state :runner eid (:ice context) nil [(->c :credit (count (:subroutines (get-card state ice))))])))
                          :prompt (msg "Pay " (count (:subroutines (get-card state ice)))
                                       " [Credits] to bypass " (:title ice) "?")
                          :yes-ability {:async true
                                        :effect (req (wait-for
                                                       (pay state side (make-eid state eid) card [(->c :credit (count (:subroutines (get-card state ice))))])
                                                       (let [payment-str (:msg async-result)
                                                             msg-ab {:msg (str "bypass " (:title (:ice context)))}]
                                                         (print-msg state side msg-ab card nil payment-str))
                                                       (bypass-ice state)
                                                       (effect-completed state side eid)))}}}])))}
     :leave-play (effect (remove-icon card))
     :abilities [(break-sub 1 1 "Sentry")
                 (strength-pump 2 1)]}))

(defcard "Fermenter"
  {:on-install {:effect (effect (add-counter card :virus 1))}
   :events [{:event :runner-turn-begins
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:req (req (pos? (get-virus-counters state card)))
                :cost [(->c :click 1) (->c :trash-can)]
                :label "Gain 2 [Credits] for each hosted virus counter"
                :msg (msg (str "gain " (* 2 (get-virus-counters state card)) " [Credits]"))
                :async true
                :effect (effect (gain-credits eid (* 2 (get-virus-counters state card))))}]})

(defcard "Flashbang"
  (auto-icebreaker {:abilities [{:label "Derez a Sentry being encountered"
                                 :cost [(->c :credit 6)]
                                 :req (req (and (get-current-encounter state)
                                                (has-subtype? current-ice "Sentry")))
                                 :msg (msg "derez " (:title current-ice))
                                 :effect (effect (derez current-ice))}
                                (strength-pump 1 1)]}))

(defcard "Flux Capacitor"
  {:hosting {:req (req (and (ice? target)
                            (can-host? state target)))}
    :events [{:event :subroutines-broken
              :once :per-encounter
              :async true
              :req (req (and this-server (same-card? current-ice (:host card))))
              :effect (effect (continue-ability (charge-ability state side eid card) card nil))}]})

(defcard "Force of Nature"
  (auto-icebreaker {:abilities [(break-sub 2 2 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "Garrote"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 1)]}))

(defcard "Gauss"
  (auto-icebreaker {:static-abilities [(breaker-strength-bonus (req (= :this-turn (installed? card))) 3)]
                    :abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 2 2)]}))

(defcard "Gingerbread"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Tracer")
                                (strength-pump 2 3)]}))

(defcard "God of War"
  (auto-icebreaker {:flags {:runner-phase-12 (req true)}
                    :abilities [(break-sub [(->c :virus 1)] 1)
                                (strength-pump 2 1)
                                {:label "Take 1 tag to place 2 virus counters (start of turn)"
                                 :once :per-turn
                                 :effect (req (wait-for (gain-tags state :runner 1)
                                                        (if (not (get-in @state [:tag :tag-prevent]))
                                                          (do (add-counter state side card :virus 2)
                                                              (system-msg state side
                                                                          (str "takes 1 tag to place 2 virus counters on God of War"))
                                                              (effect-completed state side eid))
                                                          (effect-completed state side eid))))}]}))

(defcard "Golden"
  (return-and-derez (break-sub 2 2 "Sentry")
                    (strength-pump 2 4)))

(defcard "Gordian Blade"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 1 1 :end-of-run)]}))

(defcard "Gorman Drip v1"
  {:events [{:event :corp-credit-gain
             :req (req (= :corp-click-credit (:action context)))
             :effect (effect (add-counter :runner card :virus 1))}
            {:event :corp-click-draw
             :effect (effect (add-counter :runner card :virus 1))}]
   :abilities [{:cost [(->c :click 1) (->c :trash-can)]
                :label "Gain credits"
                :async true
                :effect (effect (gain-credits eid (get-virus-counters state card)))
                :msg (msg "gain " (get-virus-counters state card) " [Credits]")}]})

(defcard "Grappling Hook"
  (let [break-subs (fn [state ice subroutines]
                     (doseq [sub subroutines]
                       (break-subroutine! state (get-card state ice) sub)))]
    {:abilities [{:label "break all but 1 subroutine"
                  :req (req (and (active-encounter? state)
                                 (< 1 (count (remove :broken (:subroutines current-ice))))))
                  :break 1 ;technically not correct, but will only be used by the engine to check for breaking abilities
                  :breaks "All"
                  :break-cost [(->c :trash-can)]
                  :cost [(->c :trash-can)]
                  :prompt "Choose the subroutine to NOT break"
                  :choices (req (unbroken-subroutines-choice current-ice))
                  :msg (msg (let [subroutines (:subroutines current-ice)
                                  target (->> subroutines
                                              (filter #(and (not (:broken %))
                                                            (= target (make-label (:sub-effect %)))))
                                              first)
                                  broken-subs (->> (:subroutines current-ice)
                                                   (remove #(= (:index %) (:index target))))]
                              (break-subroutines-msg current-ice broken-subs card)))
                  :async true
                  :effect (req (let [selected (:idx (first targets))
                                     subs-to-break (remove #(= (:index %) selected) (:subroutines current-ice))]
                                 (break-subs state current-ice subs-to-break)
                                 (let [ice (get-card state current-ice)
                                       on-break-subs (when ice (:on-break-subs (card-def ice)))
                                       event-args (when on-break-subs {:card-abilities (ability-as-handler ice on-break-subs)})]
                                   (wait-for (trigger-event-simult state side :subroutines-broken event-args ice subs-to-break)
                                             (effect-completed state side eid)))))}]}))

(defcard "Gravedigger"
  (let [e {:req (req (and (installed? (:card target))
                          (corp? (:card target))))
           :msg (msg "place 1 virus counter on " (:title card))
           :effect (effect (add-counter :runner card :virus 1))}]
    {:events [(assoc e :event :runner-trash)
              (assoc e :event :corp-trash)]
     :abilities [{:async true
                  :cost [(->c :click 1) (->c :virus 1)]
                  :keep-menu-open :while-virus-tokens-left
                  :msg "force the Corp to trash the top card of R&D"
                  :effect (effect (mill :corp eid :corp 1))}]}))

(defcard "GS Sherman M3"
  (global-sec-breaker "Barrier"))

(defcard "GS Shrike M2"
  (global-sec-breaker "Sentry"))

(defcard "GS Striker M1"
  (global-sec-breaker "Code Gate"))

(defcard "Harbinger"
  {:on-trash
   {:req (req (not-any? #{:facedown :hand} (get-zone card)))
    :effect (req (if (zone-locked? state :runner :discard)
                   (let [locks (get-in @state [:runner :locked :discard])]
                     (doseq [lock locks]
                       (release-zone state side lock :runner :discard))
                     (flip-facedown state side card)
                     (doseq [lock locks]
                       (lock-zone state side lock :runner :discard)))
                   (flip-facedown state side card)))}})

(defcard "Heliamphora"
  (let [traps-we-care-about #{"Mavirus" "Cyberdeck Virus Suite" "Breached Dome"
                              "Honeyfarm" "Increased Drop Rates" "News Team"
                              "Nightmare Archive" "Shi.Kyū" "Shock!"
                              "Space Camp"}]
    ;; TODO - we should probably just add a flag to the cdef of all archives activated ambushes
    ;; instead of manually listing them. It might come in handy later, too -nbkelly
  {:events [{:event :breach-server
             :async true
             :interactive (req true)
             :req (req (and (= target :archives)
                            (not-empty (:discard corp))))
             :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                          (update! state side (assoc-in card [:special :host-available] true))
                          (continue-ability
                            state side
                            {:optional
                             {:prompt "Host a card on this program instead of accessing it?"
                              :yes-ability {:prompt "Choose a card in Archives"
                                            :choices (req (:discard corp))
                                            :msg (msg "host " (:title target) " on itself instead of accessing it")
                                            :effect (effect
                                                      (update! (assoc-in card [:special :host-available] false))
                                                      (host card target))}}}
                            card nil))}
            {:event :pre-access-card
             :req (req (and
                         (get-in card [:special :host-available])
                         (in-discard? target)
                         (or (agenda? target)
                             (not (:seen target))
                             (contains? traps-we-care-about (:title target)))))
             :async true
             ;; note that it's possible for cards to get added to archives mid-access (even by this card)
             ;; because of this, we can't just treat this like archives interface
             ;; and we need special rules to handle these cards (and we also might as well explicitly
             ;; handle archives poison, too)
             :effect (req (let [target-card target
                                is-facedown? (not (:seen target))
                                is-agenda? (agenda? target)
                                is-trap? (contains? traps-we-care-about (:title target))]
                            (continue-ability
                              state side
                              (cond
                                is-facedown?
                                {:optional
                                 {:prompt "Host face-down card on this program instead of accessing it?"
                                  :yes-ability {:msg "host a facedown card on itself instead of accessing it"
                                                :effect (effect (update! (assoc-in card [:special :host-available] false))
                                                                (host card target-card))}}}
                                (or is-agenda? is-trap?)
                                {:optional
                                 {:prompt (msg "Host " (:title target-card) " on this program instead of accessing it?")
                                  :yes-ability {:msg (msg "host " (:title target-card) " on itself instead of accessing it")
                                                :effect (effect (update! (assoc-in card [:special :host-available] false))
                                                                (host card target-card))}}})
                              card nil)))}
            {:event :purge
             :msg "force the Corp to trash 2 cards from HQ at random, then trash itself"
             :async true
             :effect (req (wait-for
                            (trash-cards state :corp (make-eid state eid)
                                         (take 2 (shuffle (:hand corp))) {:cause-card card})
                            (trash state :runner eid card {:cause :purge :cause-card card})))}]}))

(defcard "Hemorrhage"
  {:events [{:event :successful-run
             :silent (req true)
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:cost [(->c :click 1) (->c :virus 2)]
                :keep-menu-open :while-2-virus-tokens-left
                :req (req (pos? (count (:hand corp))))
                :msg "force the Corp to trash 1 card from HQ"
                :async true
                :effect (req (continue-ability
                               state :corp
                               {:waiting-prompt true
                                :prompt "Choose a card to trash"
                                :choices (req (filter corp? (:hand corp)))
                                :async true
                                :effect (effect (trash eid target {:cause-card card
                                                                   :cause :forced-to-trash}))}
                               card nil))}]})

(defcard "Hivemind"
  {:data {:counter {:virus 1}}
   :abilities [{:req (req (pos? (get-counters card :virus)))
                :label "Move hosted virus counters"
                :prompt "Choose a Virus card to move hosted virus counters to"
                :choices {:card #(has-subtype? % "Virus")
                          :not-self true}
                :msg (msg "manually move a virus counter from itself to " (:title target))
                :effect (effect (add-counter :runner target :virus 1)
                                (add-counter :runner card :virus -1))}]})

(defcard "Houdini"
  (auto-icebreaker
    {:abilities
     [(break-sub 1 1 "Code Gate")
      (strength-pump (->c :credit 2 {:stealth 1}) 4 :end-of-run)]}))

(defcard "Hush"
  (let [reset-card-to-printed-subs
        (fn [state side card]
          (let [card (get-card state card)
                subs (subroutines-init card (card-def card))
                new-card (assoc card :subroutines subs)]
            (update! state :corp new-card)
            (trigger-event state side :subroutines-changed (get-card state new-card))))
        subroutines-should-update {:silent (req true)
                                   :effect (req (trigger-event
                                                  state :corp
                                                  :subroutines-should-update))}]
  {:implementation "Experimentally implemented. If it doesn't work correctly, please file a bug report with the exact case and cards used, and we will investigate."
   :hosting {:req (req (and (ice? target)
                            (can-host? state target)))}
   :static-abilities [{:type :disable-card
                       :req (req (same-card? target (:host card)))
                       :value true}]
   :on-install {:effect (req (reset-card-to-printed-subs state side (:host card)))}
   ;; hoping and praying that it's impossible for this to cause a feedback loop
   :events [{:event :subroutines-changed
             :req (req (and
                         (same-card? target (:host card))
                         (or (some :variable (:subroutines target))
                             (some #(not (:printed %)) (:subroutines target)))))
             :effect (req (reset-card-to-printed-subs state side target))}]
   :on-trash subroutines-should-update
   :move-zone (req (continue-ability state side subroutines-should-update card nil))
   :abilities [{:label "Host on a piece of ice"
                :prompt "Choose a piece of ice"
                :cost [(->c :click 1)]
                :choices {:req (req (and (ice? target)
                                         (installed? target)
                                         (can-host? state target)))}
                :msg (msg "host itself on " (card-str state target))
                :async true
                :effect (req (host state side target card)
                             (update-disabled-cards state)
                             (reset-card-to-printed-subs state side target)
                             (continue-ability state side subroutines-should-update card nil))}]}))

(defcard "Hyperbaric"
  (auto-icebreaker {:data {:counter {:power 1}}
                    :abilities [(break-sub 1 1 "Code Gate")
                                {:cost [(->c :credit 2)]
                                 :msg "place 1 power counter"
                                 :async true
                                 :effect (effect (add-counter eid card :power 1 nil))}]
                    :static-abilities [(breaker-strength-bonus (req (get-counters card :power)))]}))

(defcard "Hyperdriver"
  {:flags {:runner-phase-12 (req true)}
   :abilities [{:label "Remove Hyperdriver from the game to gain [Click] [Click] [Click]"
                :req (req (:runner-phase-12 @state))
                :effect (effect (move card :rfg)
                                (gain-clicks 3))
                :msg "gain [Click][Click][Click]"}]})

(defcard "Ika"
  (auto-icebreaker {:implementation "[Erratum] Program: Icebreaker - Killer - Trojan"
                    :abilities [{:label "Host on a piece of ice"
                                 :prompt "Choose a piece of ice"
                                 :cost [(->c :credit 2)]
                                 :choices {:req (req (and (ice? target)
                                                          (installed? target)
                                                          (can-host? state target)))}
                                 :msg (msg "host itself on " (card-str state target))
                                 :effect (effect (host target card))}
                                (break-sub 1 2 "Sentry")
                                (strength-pump 2 3)]}))

(defcard "Imp"
  {:data {:counter {:virus 2}}
   :interactions {:access-ability {:label "Trash card"
                                   :trash? true
                                   :req (req (and (can-trash? state :runner target)
                                                  (not (in-discard? target))
                                                  (not (get-in @state [:per-turn (:cid card)]))))
                                   :cost [(->c :virus 1)]
                                   :msg (msg "trash " (:title target) " at no cost")
                                   :once :per-turn
                                   :async true
                                   :effect (effect (trash eid (assoc target :seen true)
                                                          {:accessed true
                                                           :cause-card card}))}}})

(defcard "Incubator"
  {:events [{:event :runner-turn-begins
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:cost [(->c :click 1) (->c :trash-can)]
                :label "move hosted virus counters"
                :msg (msg "move " (get-counters card :virus) " virus counter to " (:title target))
                :choices {:card #(and (installed? %)
                                      (has-subtype? % "Virus"))}
                :effect (effect (add-counter target :virus (get-counters card :virus)))}]})

(defcard "Inti"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 2 1 :end-of-run)]}))

(defcard "Inversificator"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 1 1)]
                    :events [{:event :pass-ice
                              :interactive (req true)
                              :req (req (and (all-subs-broken-by-card? (:ice context) card)
                                             (first-event? state side :end-of-encounter
                                                           (fn [targets]
                                                             (let [context (first targets)]
                                                               (all-subs-broken-by-card? (:ice context) card))))))
                              :async true
                              :effect
                              (effect
                                (continue-ability
                                  (let [ice (:ice context)]
                                    {:optional
                                     {:prompt (str "Swap " (:title ice) " with another ice?")
                                      :yes-ability
                                      {:prompt "Choose another ice"
                                       :choices {:card #(and (installed? %)
                                                             (ice? %)
                                                             (not (same-card? % ice)))}
                                       :msg (msg "swap the positions of " (card-str state ice)
                                                 " and " (card-str state target))
                                       :effect (effect (swap-ice (get-card state ice) (get-card state target)))}}})
                                  card nil))}]}))

(defcard "Ixodidae"
  {:events [{:event :corp-credit-loss
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}
            {:event :purge
             :async true
             :msg "trash itself"
             :effect (req (trash state :runner eid card {:cause :purge :cause-card card}))}]})

(defcard "K2CP Turbine"
  {:static-abilities [{:type :breaker-strength
                       :req (req (and (has-subtype? target "Icebreaker")
                                      (not (has-subtype? target "AI"))))
                       :value 2}]
   :leave-play (effect (update-all-icebreakers))})

(defcard "Keyhole"
  (let [ability (successful-run-replace-breach
                 {:target-server :rd
                  :mandatory true
                  :duration :end-of-run
                  :ability
                  {:prompt "Choose a card to trash"
                   :not-distinct true
                   :msg (msg "trash " (:title target))
                   :choices (req (take 3 (:deck corp)))
                   :async true
                   :effect (effect (shuffle! :corp :deck)
                                   (trash eid (assoc target :seen true) {:cause-card card}))}})]
    {:abilities [{:cost [(->c :click 1)]
                  :msg "make a run on R&D"
                  :makes-run true
                  :async true
                  :effect (effect (register-events card [ability])
                                  (make-run eid :rd card))}]}))

(defcard "Knight"
  (let [knight-req (req (and (same-card? current-ice (get-nested-host card))
                             (<= (get-strength current-ice) (get-strength card))))]
    {:implementation "[Erratum] Program: Icebreaker - AI - Caïssa - Trojan"
     :abilities [{:action true
                  :label "Host on a piece of ice"
                  :async true
                  :effect (req (let [k (get-card state card)
                                     hosted (ice? (:host k))
                                     icepos (card-index state (get-card state (:host k)))]
                                 (continue-ability
                                   state side
                                   {:prompt (msg "Choose a piece of ice"
                                                 (when hosted " not before or after the current host ice"))
                                    :cost [(->c :click 1)]
                                    :choices {:req (req (if hosted
                                                          (and (or (when (= (get-zone target) (get-zone (:host k)))
                                                                     (not= 1 (abs (- (card-index state target) icepos))))
                                                                   (not= (get-zone target) (get-zone (:host k))))
                                                               (ice? target)
                                                            (can-host? state target)
                                                            (installed? target)
                                                            (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted target)))
                                                          (and (ice? target)
                                                            (installed? target)
                                                            (can-host? state target)
                                                            (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted target)))))}
                                    :msg (msg "host itself on " (card-str state target))
                                    :effect (effect (host target card))}
                                   card nil)))}
                 (break-sub 2 1 "All" {:req knight-req})]}))

(defcard "Kyuban"
  {:implementation "[Erratum] Program: Trojan"
   :hosting {:req (req (and (ice? target)
                            (installed? target)
                            (can-host? state target)))}
   :events [{:event :pass-ice
             :interactive (req true)
             :req (req (same-card? (:ice context) (:host card)))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 2))}]})

(defcard "Laamb"
  (give-ice-subtype 2 "Barrier"
                    [(break-sub 2 0 "Barrier")
                     (strength-pump 3 6)]))

(defcard "Lamprey"
  {:events [{:event :successful-run
             :req (req (= :hq (target-server context)))
             :msg "force the Corp to lose 1 [Credits]"
             :async true
             :effect (effect (lose-credits :corp eid 1))}
            {:event :purge
             :async true
             :msg "trash itself"
             :effect (req (trash state :runner eid card {:cause :purge
                                                         :cause-card card}))}]})

(defcard "Laser Pointer"
  {:events [{:event :encounter-ice
             :req (req (or (has-subtype? current-ice "AP")
                           (has-subtype? current-ice "Observer")
                           (has-subtype? current-ice "Destroyer")))
             :async true
             :effect (effect (continue-ability
                               {:optional
                                {:prompt (msg "Trash this program to bypass "
                                              (card-str state current-ice)
                                              "?")
                                 :waiting-prompt true
                                 :yes-ability
                                 {:msg (msg "bypass" (card-str state current-ice))
                                  :effect (req
                                            (wait-for (trash state :runner (make-eid state eid) card
                                                             {:unpreventable :true
                                                              :cause-card card})
                                                      (bypass-ice state)))}}}
                               card nil))}]})

(defcard "Leech"
  {:events [{:event :successful-run
             :req (req (is-central? (target-server context)))
             :msg "place 1 virus counter on itself"
             :effect (req (add-counter state side card :virus 1))}]
   :abilities [{:cost [(->c :virus 1)]
                :label "Give -1 strength to current piece of ice"
                :req (req (active-encounter? state))
                :msg (msg "give -1 strength to " (:title current-ice))
                :effect (effect (pump-ice current-ice -1))}]})

(defcard "Leprechaun"
  {:abilities [{:label "Install and host a program"
                :cost [(->c :click 1)]
                :prompt "Choose a program in the grip"
                :choices {:req (req (and (program? target)
                                         (runner-can-install? state side target false)
                                         (in-hand? target)))}
                :msg (msg "install and host " (:title target))
                :async true
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed program (manual)"
                :prompt "Choose an installed program"
                :choices {:card #(and (program? %)
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]})

(defcard "Leviathan"
  (auto-icebreaker {:abilities [(break-sub 3 3 "Code Gate")
                                (strength-pump 3 5)]}))

(defcard "Living Mural"
  (auto-icebreaker {:on-install {:req (req (threat-level 4 state))
                                 :msg "gain 3 strength for the remainder of the turn"
                                 :effect (effect (pump card 3 :end-of-turn))}
                    :hosting {:req (req (and (ice? target)
                                             (installed? target)
                                             (can-host? state target)))}
                    :abilities [(break-sub 1 1 "Sentry" {:req (req (protecting-same-server? current-ice (:host card)))})
                                (strength-pump 1 2)]}))

(defcard "LLDS Energy Regulator"
  {:interactions {:prevent [{:type #{:trash-hardware}
                             :req (req true)}]}
   :abilities [{:cost [(->c :credit 3)]
                :msg "prevent a piece of hardware from being trashed"
                :effect (effect (trash-prevent :hardware 1))}
               {:cost [(->c :trash-can)]
                :msg "prevent a piece of hardware from being trashed"
                :effect (effect (trash-prevent :hardware 1))}]})

(defcard "Lobisomem"
  (auto-icebreaker {:data {:counter {:power 1}}
                    :abilities [(break-sub 1 1 "Code Gate")
                                {:label "Break X Barrier subroutines"
                                 :cost [(->c :x-credits) (->c :power 1)]
                                 :break-cost [(->c :x-credits) (->c :power 1)]
                                 :req (req (and
                                             (active-encounter? state)
                                             (<= (get-strength current-ice) (get-strength card))))
                                 :msg (msg "break " (quantify (cost-value eid :x-credits) "subroutine")
                                             " on " (card-str state current-ice))
                                 :effect (effect
                                             (continue-ability
                                               (when (pos? (cost-value eid :x-credits))
                                                 (break-sub nil (cost-value eid :x-credits) "Barrier"))
                                               card nil))}
                                (strength-pump 1 2)]
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (has-subtype? target "Code Gate")))
                              :msg "place 1 power counter on itself"
                              :async true
                              :effect (effect (add-counter card :power 1)
                                              (effect-completed eid))}]}))

(defcard "Lustig"
  (trash-to-bypass (break-sub 1 1 "Sentry")
                   (strength-pump 3 5)))

(defcard "Magnum Opus"
  {:abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :async true
                :effect (effect (gain-credits eid 2))
                :msg "gain 2 [Credits]"}]})

(defcard "Makler"
  (auto-icebreaker {:abilities [(break-sub 2 2 "Barrier")
                                (strength-pump 2 2)]
                    :events [{:event :pass-ice
                              :req (req (and (all-subs-broken-by-card? (:ice context) card)
                                             (first-event? state side :pass-ice
                                                           (fn [targets]
                                                             (let [context (first targets)]
                                                               (all-subs-broken-by-card? (:ice context) card))))))
                              :msg "gain 1 [Credits]"
                              :async true
                              :effect (effect (gain-credits :runner eid 1))}]}))

(defcard "Malandragem"
  {:data {:counter {:power 2}}
   :events [(rfg-on-empty :power)
            {:event :encounter-ice
             :interactive (req true)
             :ability-name "Malandragem (rfg)"
             :optional {:prompt "Remove this program from the game to bypass encountered ice?"
                        :req (req (threat-level 4 state))
                        :yes-ability {:cost [(->c :remove-from-game)]
                                      :msg (msg "bypass " (card-str state current-ice))
                                      :effect (req (bypass-ice state))}}}
            {:event :encounter-ice
             :interactive (req true)
             :ability-name "Malandragem (Power counter)"
             :optional {:prompt "Remove 1 power counter to bypass encountered ice?"
                        :once :per-turn
                        :req (req (and (>= 3 (ice-strength state side current-ice))
                                       (<= 1 (get-counters (get-card state card) :power))))
                        :yes-ability {:cost [(->c :power 1)]
                                      :msg (msg "bypass " (card-str state current-ice))
                                      :effect (req (bypass-ice state))}}}]})

(defcard "Mammon"
  (auto-icebreaker {:flags {:runner-phase-12 (req (pos? (:credit runner)))}
                    :abilities [{:label "Place X power counters"
                                 :prompt "How many credits do you want to spend?"
                                 :once :per-turn
                                 :cost [(->c :x-credits)]
                                 :req (req (:runner-phase-12 @state))
                                 :async true
                                 :effect (effect (add-counter card :power (cost-value eid :x-credits)))
                                 :msg (msg "place " (quantify (cost-value eid :x-credits) "power counter") " on itself")}
                                (break-sub [(->c :power 1)] 1)
                                (strength-pump 2 2)]
                    :events [{:event :runner-turn-ends
                              :effect (effect (update! (assoc-in card [:counter :power] 0)))}]}))

(defcard "Mantle"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (or (hardware? target)
                                                   (program? target))))
                                :type :recurring}}})

(defcard "Marjanah"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Barrier"
                                           {:label "Break 1 Barrier subroutine"
                                            :break-cost-bonus (req (when (:successful-run runner-reg) [(->c :credit -1)]))})
                                (strength-pump 1 1)]}))

(defcard "Mass-Driver"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Code Gate")
                                (strength-pump 1 1)]
                    :events [{:event :subroutines-broken
                              :req (req (all-subs-broken-by-card? target card))
                              :msg "prevent the first 3 subroutines from resolving on the next encountered ice"
                              :effect
                              (effect
                                (register-events
                                  card
                                  (let [broken-ice target]
                                    [{:event :encounter-ice
                                      :duration :end-of-run
                                      :unregister-once-resolved true
                                      :effect (req (doseq [sub (take 3 (:subroutines (:ice context)))]
                                                     (dont-resolve-subroutine! state (get-card state (:ice context)) sub)))}])))}]}))

(defcard "Matryoshka"
  (let [break-abi {:label "Break X subroutines"
                   :cost [(->c :x-credits)]
                   :break-cost [(->c :x-credits)]
                   :req (req (let [hosted (:hosted (get-card state card))
                                   valid (filter #(not (facedown? %)) hosted)
                                   same-title (filter #(= (:title card) (:title %)) valid)]
                               (and (active-encounter? state)
                                    current-ice card
                                    (<= (get-strength current-ice) (get-strength card))
                                    (not-empty same-title))))
                                        ; no break-req to not enable auto-pumping
                   :msg (msg "break " (quantify (cost-value eid :x-credits) "subroutine")
                             " on " (card-str state current-ice) " and turn 1 hosted copy of "
                             (:title card) " facedown")
                   :effect (req
                             (let [hosted (:hosted (get-card state card))
                                   valid (filter #(not (facedown? %)) hosted)
                                   same-title (filter #(= (:title card) (:title %)) valid)
                                   first-copy (first same-title)]
                               (flip-facedown state side (get-card state first-copy))
                               (if (pos? (cost-value eid :x-credits))
                                 (continue-ability
                                   state side
                                   (break-sub nil (cost-value eid :x-credits) "All")
                                   card nil)
                                 (effect-completed state side eid))))}
        host-abi {:label "Host 1 copy of Matryoshka"
                  :prompt (msg "Choose 1 copy of " (:title card) " in the grip")
                  :keep-menu-open :while-clicks-left
                  :cost [(->c :click 1)]
                  :choices {:card #(and (in-hand? %)
                                        (= (:title %) "Matryoshka"))}
                  :msg (msg "host " (:title target) " on itself")
                  :effect (req (host state :runner card target))}]
    (auto-icebreaker
      {:abilities [host-abi
                   break-abi
                   (strength-pump 1 1)]
       :events [{:event :runner-turn-begins
                 :req (req (some #(facedown? %) (:hosted (get-card state card))))
                 :msg "turn all cards hosted on itself face-up"
                 :effect (req (let [targets (filter #(facedown? %) (:hosted (get-card state card)))]
                                (doseq [oc targets]
                                  ;; we can't use flip-faceup as that wires up events
                                  (let [newcard (assoc oc :facedown nil)]
                                    (update! state side newcard)))))}]})))

(defcard "Maven"
  (auto-icebreaker {:abilities [(break-sub 2 1)]
                    :static-abilities [(breaker-strength-bonus (req (count (filter program? (all-active-installed state :runner)))))]}))

(defcard "Mayfly"
  (auto-icebreaker
    {:abilities [(break-sub
                   1 1 "All"
                   {:additional-ability
                    {:msg "will trash itself when this run ends"
                     :effect (req
                               (register-events
                                 state :runner (get-card state card)
                                 [{:event :run-ends
                                   :duration :end-of-run
                                   :unregister-once-resolved true
                                   :async true
                                   :effect (effect (trash eid card {:cause :runner-ability
                                                                    :cause-card card}))}]))}})
                 (strength-pump 1 1)]}))

(defcard "Medium"
  {:events [{:event :successful-run
             :req (req (= :rd (target-server context)))
             :effect (effect (add-counter card :virus 1))}
            {:event :breach-server
             :async true
             :req (req (= target :rd))
             :effect (effect (continue-ability
                               {:req (req (< 1 (get-virus-counters state card)))
                                :prompt "How many additional cards from R&D do you want to access?"
                                :choices {:number (req (dec (get-virus-counters state card)))
                                          :default (req (dec (get-virus-counters state card)))}
                                :msg (msg "access " (quantify target "additional card") " from R&D")
                                :effect (effect (access-bonus :rd (max 0 target)))}
                               card nil))}]})

(defcard "Mimic"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")]}))

(defcard "Misdirection"
  {:abilities [{:cost [(->c :click 2) (->c :x-credits)]
                :label "remove X tags"
                :async true
                :msg (msg "remove " (quantify (cost-value eid :x-credits) "tag"))
                :effect (effect (lose-tags :runner eid (cost-value eid :x-credits)))}]})

(defcard "MKUltra"
  (let [events (for [event [:run :approach-ice :encounter-ice :pass-ice :run-ends
                            :ice-strength-changed :ice-subtype-changed :breaker-strength-changed
                            :subroutines-changed]]
                 (assoc heap-breaker-auto-pump-and-break :event event))
        cdef (install-from-heap "MKUltra" "Sentry"
                                [(pump-and-break [(->c :credit 3)] 2 "Sentry")])]
    (assoc cdef :events (apply conj events (:events cdef)))))

(defcard "Mongoose"
  (auto-icebreaker {:events [{:event :subroutines-broken
                              :silent (req true)
                              :req (req (and (any-subs-broken-by-card? target card)
                                             run))
                              :effect (req (let [broken-ice target]
                                             (register-lingering-effect
                                               state side
                                               card
                                               {:type :prevent-paid-ability
                                                :duration :end-of-run
                                                :req (req (let [[break-card break-ability] targets]
                                                            (and (not (same-card? current-ice broken-ice))
                                                                 (same-card? break-card card))))
                                                :value (req true)})))}]
                    :abilities [(break-sub 1 2 "Sentry")
                                (strength-pump 2 2)]}))

(defcard "Monkeywrench"
  {:hosting {:req (req (and (ice? target)
                            (installed? target)
                            (can-host? state target)))}
   :static-abilities [{:type :ice-strength
                       :req (req (and (ice? target)
                                      (= (get-zone (:host card)) (get-zone target))))
                       :value (req (if (same-card? target (:host card)) -2 -1))}]})

(defcard "Morning Star"
  (auto-icebreaker {:abilities [(break-sub 1 0 "Barrier")]}))

(defcard "Multithreader"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (program? target)))
                                :type :recurring}}})

(defcard "Musaazi"
  (virus-breaker "Sentry"))

(defcard "Muse"
  (letfn [(trojan-auto-hosts?
            ;; NOTE - this function will theoretically need to be maintained if more cards like this are ever printed - nbkelly, jan '24
            [card]
            (not (or (has-subtype? card "Caïssa")
                     (= (:title card) "Ika"))))
          (muse-abi [where]
            {:prompt "Choose a non-daemon program"
             :msg (msg (if (= target "Done")
                         "shuffle the stack"
                         (str "install " (:title target))))
             :choices (req (concat
                             (->> (where runner)
                                  (filter
                                    #(and (program? %)
                                          (not (has-subtype? % "Daemon"))
                                          (can-pay? state side
                                                    (assoc eid :source card :source-type :runner-install)
                                                    % nil [(->c :credit (install-cost state side %))])))
                                  (sort-by :title)
                                  (seq))
                             ["Done"]))
             :async true
             :effect (req (when (= :deck where)
                            (trigger-event state side :searched-stack)
                            (shuffle! state side :deck))
                          (if (not= target "Done")
                            ;; does the card need to be installed on muse?
                            (if-not (has-subtype? target "Trojan")
                              (runner-install state side (assoc eid :source card :source-type :runner-install) target {:host-card (get-card state card)})
                              ;;otherwise, pick a target card to host the trojan on
                              (if (trojan-auto-hosts? target)
                                ;; if the trojan does it for free, so be it
                                (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)
                                ;; do it the hard way
                                (let [target-card target]
                                  (continue-ability
                                    state side
                                    {:prompt (msg "Choose a piece of ice to host " (:title target-card))
                                     :choices {:card #(and (installed? %)
                                                           (ice? %))}
                                     :async true
                                     :effect (req (runner-install state side (assoc eid :source card :source-type :runner-install) target-card {:host-card (get-card state target)}))}
                                    card nil))))
                            ;; declined to install
                            (effect-completed state side eid)))})]
    {:on-install {:async true
                  :prompt "Choose where to install from"
                  :choices (req ["Grip" "Stack"
                                 (when-not (zone-locked? state :runner :discard) "Heap")])
                  :msg (msg "search the " target " for a non-daemon program to install")
                  :effect (effect (continue-ability
                                    (muse-abi (if (= "Stack" target) :deck
                                                  (if (= "Grip" target) :hand :discard)))
                                    card nil))}}))

(defcard "Na'Not'K"
  (auto-icebreaker {:static-abilities [(breaker-strength-bonus (req (count run-ices)))]
                    :abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 3 2)]}))

(defcard "Nanuq"
  (let [self-rfg {:msg "remove itself from the game"
                  :once :per-turn ;; prevents self triggering
                  :interactive (req true)
                  :effect (effect (move card :rfg))}]
    (auto-icebreaker {:abilities [(break-sub 2 2 "All")
                                  (strength-pump 1 1)]
                      :uninstall (effect (continue-ability self-rfg card nil))
                      :events [(assoc self-rfg :event :agenda-scored)
                               (assoc self-rfg :event :agenda-stolen)]})))

(defcard "Nerve Agent"
  {:events [{:event :successful-run
             :req (req (= :hq (target-server context)))
             :effect (effect (add-counter card :virus 1))}
            {:event :breach-server
             :async true
             :req (req (= target :hq))
             :effect (effect (continue-ability
                               {:req (req (< 1 (get-virus-counters state card)))
                                :prompt "How many additional cards from HQ do you want to access?"
                                :choices {:number (req (dec (get-virus-counters state card)))
                                          :default (req (dec (get-virus-counters state card)))}
                                :msg (msg "access " (quantify target "additional card") " from HQ")
                                :effect (effect (access-bonus :hq (max 0 target)))}
                               card nil))}]})

(defcard "Net Shield"
  {:interactions {:prevent [{:type #{:net}
                             :req (req true)}]}
   :abilities [{:cost [(->c :credit 1)]
                :once :per-turn
                :msg "prevent the first net damage this turn"
                :effect (effect (damage-prevent :net 1))}]})

(defcard "Nfr"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")]
                    :static-abilities [(breaker-strength-bonus (req (get-counters card :power)))]
                    :events [{:event :end-of-encounter
                              :req (req (all-subs-broken-by-card? (:ice context) card))
                              :msg "place 1 power counter on itself"
                              :effect (effect (add-counter card :power 1))}]}))

(defcard "Nga"
  {:data {:counter {:power 3}}
   :events [(trash-on-empty :power)
            {:event :successful-run
            :interactive (get-autoresolve :auto-fire (complement never?))
            :silent (get-autoresolve :auto-fire never?)
            :optional
            {:req (req (and (first-event? state side :successful-run)
                            (pos? (get-counters card :power))))
             :player :runner
             :autoresolve (get-autoresolve :auto-fire)
             :waiting-prompt true
             :prompt "Remove 1 hosted power counter?"
             :yes-ability
             {:msg "remove 1 hosted power counter to sabotage 1"
              :async true
              :cost [(->c :power 1)]
              :effect (effect (continue-ability
                                (sabotage-ability 1)
                                card nil))}
             :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]
   :abilities [(set-autoresolve :auto-fire "Nga")]})

(defcard "Ninja"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 3 5)]}))

(defcard "Num"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")]}))

(defcard "Nyashia"
  {:data {:counter {:power 3}}
   :events [{:event :breach-server
             :optional
             {:req (req (and (pos? (get-counters card :power))
                             (= target :rd)))
              :waiting-prompt true
              :prompt "Spend 1 hosted power counter to access 1 additional card?"
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability {:msg "access 1 additional card from R&D"
                            :effect (effect (access-bonus :rd 1)
                                            (add-counter card :power -1))}}}]
   :abilities [(set-autoresolve :auto-fire "Nyashia")]})

(defcard "Odore"
  (auto-icebreaker {:abilities [(break-sub 2 0 "Sentry")
                                (break-sub 0 1 "Sentry"
                                           {:label "Break 1 Sentry subroutine (Virtual restriction)"
                                            :req (req (<= 3 (count (filter #(has-subtype? % "Virtual")
                                                                           (all-active-installed state :runner)))))})
                                (strength-pump 3 3)]}))

(defcard "Omega"
  (auto-icebreaker {:abilities [(break-sub
                                 1 1 "All"
                                 {:req (req (let [server-ice (:ices (card->server state current-ice))]
                                                                  (same-card? current-ice (first server-ice))))})
                                (strength-pump 1 1)]}))

(defcard "Orca"
  (auto-icebreaker {:abilities [(break-sub 2 0 "Sentry")
                                (strength-pump 2 3)]
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (first-event? state side :subroutines-broken #(all-subs-broken-by-card? (first %) card))))
                              :async true
                              :effect (effect (continue-ability (charge-ability state side eid card) card nil))}]}))

(defcard "Origami"
  {:static-abilities [{:type :hand-size
                       :req (req (= :runner side))
                       :value (req (count (filter #(= (:title %) "Origami")
                                                  (all-active-installed state :runner))))}]})

(defcard "Overmind"
  (auto-icebreaker {:on-install {:effect (effect (add-counter card :power (available-mu state)))}
                    :abilities [(break-sub [(->c :power 1)] 1)
                                (strength-pump 1 1)]}))

(defcard "Paintbrush"
  {:abilities [{:cost [(->c :click 1)]
                :label "give ice a subtype"
                :choices {:card #(and (installed? %)
                                      (ice? %)
                                      (rezzed? %))}
                :async true
                :effect (effect
                          (continue-ability
                            (let [ice target]
                              {:prompt "Choose one"
                               :choices ["Sentry" "Code Gate" "Barrier"]
                               :msg (msg "spend [Click] and make " (card-str state ice)
                                         " gain " target
                                         " until the end of the next run this turn")
                               :effect (effect (register-lingering-effect
                                                 card
                                                 {:type :gain-subtype
                                                  :duration :end-of-next-run
                                                  :req (req (same-card? ice target))
                                                  :value target}))})
                            card nil))}]})

(defcard "Panchatantra"
  {:events [{:event :encounter-ice
             :optional
             {:prompt "Give encountered piece ice a subtype?"
              :req (req (not (get-in @state [:per-turn (:cid card)])))
              :yes-ability
              {:prompt "Choose an ice subtype"
               :choices (req (->> (server-cards)
                                  (reduce (fn [acc card]
                                            (if (ice? card)
                                              (apply conj acc (:subtypes card))
                                              acc))
                                          #{})
                                  (#(disj % "Barrier" "Code Gate" "Sentry"))
                                  sort))
               :msg (msg "make " (card-str state current-ice) " gain " target)
               :effect (effect (register-once {:once :per-turn} card)
                               (register-lingering-effect
                                 card
                                 (let [ice current-ice]
                                   {:type :gain-subtype
                                    :duration :end-of-run
                                    :req (req (same-card? ice target))
                                    :value target})))}}}]})

(defcard "Paperclip"
  (let [events (for [event [:run :approach-ice :encounter-ice :pass-ice :run-ends
                            :ice-strength-changed :ice-subtype-changed :breaker-strength-changed
                            :subroutines-changed]]
                 (assoc heap-breaker-auto-pump-and-break :event event))
        cdef (install-from-heap "Paperclip" "Barrier" [])
        abilities [{:label "+X strength, break X subroutines"
                    :cost [(->c :x-credits)]
                    :heap-breaker-pump :x ; strength gained
                    :heap-breaker-break :x ; number of subs broken
                    :break-req (req (and (active-encounter? state)
                                         (has-subtype? current-ice "Barrier")))
                    :effect (effect (pump card (cost-value eid :x-credits))
                                    (continue-ability
                                      (break-sub nil (cost-value eid :x-credits) "Barrier" {:repeatable false})
                                      (get-card state card) nil))
                    :msg (msg "increase its strength from " (get-strength card)
                              " to " (+ (cost-value eid :x-credits) (get-strength card)))}]]
    (assoc cdef
           :events (apply conj events (:events cdef))
           :abilities abilities)))

(defcard "Parasite"
  {:implementation "[Erratum] Program: Virus - Trojan"
   :hosting {:req (req (and (ice? target)
                            (installed? target)
                            (rezzed? target)
                            (can-host? state target)))}
   :on-install
   {:effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))}
   :static-abilities [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value (req (- (get-virus-counters state card)))}]
   :events [{:event :runner-turn-begins
             :effect (req (add-counter state side card :virus 1))}
            {:event :ice-strength-changed
             :req (req (and (same-card? (:card context) (:host card))
                            (not (card-flag? (:host card) :untrashable-while-rezzed true))
                            (<= (get-strength (:card context)) 0)))
             :async true
             :effect (req (unregister-events state side card)
                          (when (get-in card [:special :installing])
                            (update! state side (update-in card [:special] dissoc :installing))
                            (trigger-event state :runner :runner-install card))
                          (trash state :runner eid (:card context) {:unpreventable true :cause-card card}))
             :msg (msg "trash " (:title (:card context)))}]})

(defcard "Paricia"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (asset? target)))
                                :type :recurring}}})

(defcard "Passport"
  (central-only (break-sub 1 1 "Code Gate")
                (strength-pump 2 2)))

(defcard "Pawn"
  {:implementation "[Erratum] Program: Caïssa - Trojan. Developer Note: All abilities are manual"
   :abilities [{:label "Host on the outermost piece of ice of a central server"
                :cost [(->c :click 1)]
                :prompt "Choose the outermost piece of ice of a central server"
                :choices {:req (req (and (ice? target)
                                         (can-host? state target)
                                         (= (last (get-zone target)) :ices)
                                         (is-central? (second (get-zone target)))))}
                :msg (msg "host itself on " (card-str state target))
                :effect (effect (host target card))}
               {:label "Host on the next innermost piece of ice"
                :prompt "Choose the next innermost piece of ice"
                :choices {:req (req (and (ice? target)
                                         (can-host? state target)
                                         (= (last (get-zone target)) :ices)
                                         (is-central? (second (get-zone target)))))}
                :msg (msg "host itself on " (card-str state target))
                :effect (effect (host target card))}
               {:req (req (not (zone-locked? state :runner :discard)))
                :label "Trash to install a Caïssa program from the grip or heap, ignoring all costs"
                :async true
                :effect (req (let [this-pawn (:cid card)]
                               (wait-for (trash state side card nil)
                                         (continue-ability
                                           state side
                                           {:prompt "Choose a Caïssa program to install from the grip or heap"
                                            :show-discard true
                                            :choices {:card #(and (has-subtype? % "Caïssa")
                                                                  (not= (:cid %) this-pawn)
                                                                  (or (in-hand? %)
                                                                      (in-discard? %)))}
                                            :msg (msg "install " (:title target))
                                            :async true
                                            :effect (effect (runner-install eid target {:ignore-all-cost true}))}
                                           card nil))))}]})

(defcard "Peacock"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Code Gate")
                                (strength-pump 2 3)]}))

(defcard "Pelangi"
  {:data {:counter {:virus 2}}
   :abilities [{:once :per-turn
                :req (req (active-encounter? state))
                :cost [(->c :virus 1)]
                :label "Make ice gain a subtype"
                :prompt "Choose an ice subtype"
                :choices (req (->> (server-cards)
                                   (reduce (fn [acc card]
                                             (if (ice? card)
                                               (into acc (:subtypes card))
                                               acc))
                                           #{})
                                   sort))
                :msg (msg "make " (card-str state current-ice)
                          " gain " target
                          " until end of the encounter")
                :effect (effect (register-lingering-effect
                                  card
                                  (let [ice current-ice]
                                    {:type :gain-subtype
                                     :duration :end-of-encounter
                                     :req (req (same-card? target ice))
                                     :value target})))}]})

(defcard "Penrose"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier" {:req (req (= :this-turn (installed? card)))})
                                (break-sub 1 1 "Code Gate")
                                (strength-pump (->c :credit 1 {:stealth 1}) 3 :end-of-encounter)]}))

(defcard "Peregrine"
  (return-and-derez (break-sub 1 1 "Code Gate")
                    (strength-pump 3 3)))

(defcard "Persephone"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")
                                (strength-pump 1 1)]
                    :events [{:event :pass-ice
                              :req (req (and (has-subtype? (:ice context) "Sentry")
                                             (rezzed? (:ice context))
                                             (pos? (count (:deck runner)))))
                              :effect
                              (effect
                                (continue-ability
                                  (let [fired-subs (count (filter :fired (:subroutines (:ice context))))]
                                    {:optional
                                     {:prompt (str "Trash the top card of the stack to trash " (quantify fired-subs "card") " from R&D?")
                                      :yes-ability
                                      {:async true
                                       :msg (msg (str "trash " (:title (first (:deck runner)))
                                                      " from the stack and"
                                                      " trash " (quantify fired-subs "card") " from R&D"))
                                       :effect (req (wait-for (mill state :runner :runner 1)
                                                              (mill state :runner eid :corp fired-subs)))}}})
                                  card nil))}]}))

(defcard "Pheromones"
  {:x-fn (req (get-counters card :virus))
   :recurring (get-x-fn)
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :hq (target-server context)))
             :effect (effect (add-counter card :virus 1))}]
   :interactions {:pay-credits {:req (req (= :hq (get-in @state [:run :server 0])))
                                :type :recurring}}})

(defcard "Physarum Entangler"
  {:hosting {:req (req (and (ice? target)
                            (installed? target)
                            (can-host? state target)))}
   :events [{:event :purge
             :async true
             :msg "trash itself"
             :effect (req (trash state :runner eid card {:cause :purge
                                                         :cause-card card}))}
            {:event :encounter-ice
             :optional {:prompt (msg "Pay " (count (:subroutines (get-card state current-ice)))
                                     " [Credits] to bypass encountered ice?")
                        :req (req (and (not (has-subtype? current-ice "Barrier"))
                                       (same-card? current-ice (:host card))
                                       (can-pay? state :runner eid (:ice context) nil [(->c :credit (count (:subroutines (get-card state current-ice))))])))
                        :yes-ability {:async true
                                      :effect (req (wait-for
                                                     (pay state side (make-eid state eid) card [(->c :credit (count (:subroutines (get-card state current-ice))))])
                                                     (let [payment-str (:msg async-result)
                                                           msg-ab {:msg (str "bypass " (card-str state (:ice context)))}]
                                                       (print-msg state side msg-ab card nil payment-str))
                                                     (bypass-ice state)
                                                     (effect-completed state side eid)))}}}]})

(defcard "Pichação"
  ;; TODO - there's not really a way to tell if an event happened during a run?
  ;; this can be cleaned up a little later
  {:hosting {:req (req (and (ice? target)
                            (installed? target)
                            (can-host? state target)))}
   :events [{:event :pass-ice
             :optional {:interactive (req true)
                        :prompt "Gain [Click]?"
                        :waiting-prompt true
                        :req (req (same-card? (:ice context) (:host card)))

                        :yes-ability
                        {:msg "gain [Click]"
                         :async true
                         :effect (req
                                   (gain-clicks state :runner 1)
                                   (if (< 1 (count (turn-events state side :runner-click-gain)))
                                     (continue-ability
                                       state side
                                       {:optional
                                        {:prompt (str "Is " (:title card) " added to the grip?")
                                         :waiting-prompt true
                                         :yes-ability {:msg "appease the rules"
                                                       :cost [(->c :return-to-hand)]}}}
                                       card nil)
                                     (effect-completed state side eid)))}
                        :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]})

(defcard "Pipeline"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 2 1 :end-of-run)]}))

(defcard "Plague"
  {:on-install {:prompt "Choose a server"
                :choices (req servers)
                :msg (msg "target " target)
                :req (req (not (:card-target card)))
                :effect (effect (update! (assoc card :card-target target)))}
   :events [{:event :successful-run
             :req (req (= (zone->name (:server context))
                          (:card-target (get-card state card))))
             :msg "place 2 virus counters on itself"
             :effect (effect (add-counter :runner card :virus 2))}]})

(defcard "Pressure Spike"
  (letfn [(once [card]
            {:once :per-run
             :once-key (str (:cid card) "-thread-pump")})]
    {:implementation "Auto-breaking disabled for this card."
     :abilities [(break-sub 1 1 "Barrier")
                 (strength-pump 2 3)
                 (let [base (strength-pump
                              2 9 :end-of-encounter
                              {:req (req (threat-level 4 state)
                                         (not-used-once? state (once card) card))})]
                   (assoc base :effect (req (register-once state side (once card) card)
                                            ((:effect base) state side eid card targets))))]}))

(defcard "Progenitor"
  {:abilities [{:label "Install and host a virus program"
                :req (req (empty? (:hosted card)))
                :cost [(->c :click 1)]
                :prompt "Choose a virus program"
                :choices {:card #(and (program? %)
                                      (has-subtype? % "Virus")
                                      (in-hand? %))}
                :msg (msg "install and host " (:title target))
                :async true
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed virus (manual)"
                :req (req (empty? (:hosted card)))
                :prompt "Choose an installed virus program"
                :choices {:card #(and (program? %)
                                      (has-subtype? % "Virus")
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]
   :static-abilities
   [{:type :prevent-purge-virus-counters
     :req (req (pos? (get-counters (first (:hosted card)) :virus)))
     :value (req {:card (first (:hosted card))
                  :quantity 1})}]})

(defcard "Propeller"
  (auto-icebreaker {:data {:counter {:power 4}}
                    :abilities [(break-sub 1 1 "Barrier")
                                (strength-pump [(->c :power 1)] 2)]}))

(defcard "Puffer"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 2 1)
                                {:cost [(->c :click 1)]
                                 :msg "place 1 power counter"
                                 :label "Place 1 power counter"
                                 :effect (effect (add-counter card :power 1))}
                                {:cost [(->c :click 1)]
                                 :msg "remove 1 power counter"
                                 :label "Remove 1 power counter"
                                 :effect (effect (add-counter card :power -1))}]
                    :static-abilities [(breaker-strength-bonus (req (get-counters card :power)))
                                       {:type :used-mu
                                        :duration :while-active
                                        :value (req (get-counters card :power))}]}))

(defcard "Reaver"
  {:events [{:event :runner-trash
             :async true
             :interactive (req true)
             :once-per-instance true
             :req (req (and (some #(installed? (:card %)) targets)
                            (first-event? state side :runner-trash
                              (fn [targets] (some #(installed? (:card %)) targets)))))
             :msg "draw 1 card"
             :effect (effect (draw :runner eid 1))}]})

(defcard "Refractor"
  (auto-icebreaker
    {:abilities
     [(break-sub 1 1 "Code Gate")
      (strength-pump (->c :credit 1 {:stealth 1}) 3 :end-of-encounter)]}))

(defcard "Revolver"
  (auto-icebreaker
   {:data {:counter {:power 6}}
    :abilities [(break-sub [(->c :power 1)] 1 "Sentry" {:auto-break-sort 1})
                (break-sub [(->c :trash-can)] 1 "Sentry")
                (strength-pump 2 3)]}))

(defcard "Rezeki"
  {:events [{:event :runner-turn-begins
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "RNG Key"
  {:events [{:event :pre-access-card
             :req (req (get-in card [:special :rng-guess]))
             :async true
             :msg (msg "reveal " (:title target) " from " (zone->name (get-zone target)))
             :effect (req (wait-for
                            (reveal state side target)
                            (continue-ability
                              state side
                              (let [guess (get-in card [:special :rng-guess])]
                                (when (#{(:cost target) (get-advancement-requirement target)} guess)
                                  {:prompt "Choose one"
                                   :waiting-prompt true
                                   :choices ["Gain 3 [Credits]" "Draw 2 cards"]
                                   :async true
                                   :msg (msg (decapitalize target))
                                   :effect (req (if (= target "Draw 2 cards")
                                                  (draw state :runner eid 2)
                                                  (gain-credits state :runner eid 3)))}))
                              card nil)))}
            {:event :post-access-card
             :effect (effect (update! (assoc-in card [:special :rng-guess] nil)))}
            (let [highest-cost
                  (fn [state card]
                    (or (get-in card [:special :rng-highest])
                        (let [cost (->> (server-cards)
                                        (filter :cost)
                                        (map :cost)
                                        sort
                                        last)]
                          (update! state :runner (assoc-in card [:special :rng-highest] cost))
                          cost)))]
              {:event :successful-run
               :optional
               {:req (req (and (#{:hq :rd} (target-server context))
                               (first-event? state :runner :successful-run
                                             (fn [targets]
                                               (let [context (first targets)]
                                                 (#{:hq :rd} (target-server context)))))))
                :prompt "Name a number?"
                :autoresolve (get-autoresolve :auto-fire)
                :yes-ability {:prompt "Guess a number"
                              :choices {:number (req (highest-cost state card))}
                              :msg (msg "guess " target)
                              :effect (effect (update! (assoc-in card [:special :rng-guess] target)))}}})]
   :abilities [(set-autoresolve :auto-fire "RNG Key")]})

(defcard "Rook"
  {:implementation "[Erratum] Program: Caïssa - Trojan"
   :abilities [{:cost [(->c :click 1)]
                :label "Host on another ice"
                :async true
                :effect (req (let [r (get-card state card)
                                   hosted? (ice? (:host r))
                                   icepos (card-index state (get-card state (:host r)))]
                               (continue-ability
                                 state side
                                 {:prompt (if hosted?
                                            (msg "Choose a piece of ice protecting this server or at position "
                                                 icepos " of a different server")
                                            (msg "Choose a piece of ice protecting any server"))
                                  :choices {:req (req (if hosted?
                                                        (and (or (= (get-zone target) (get-zone (:host r)))
                                                                 (= (card-index state target) icepos))
                                                             (= (last (get-zone target)) :ices)
                                                             (ice? target)
                                                             (can-host? state target)
                                                             (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted target)))
                                                        (and (ice? target)
                                                             (can-host? state target)
                                                             (= (last (get-zone target)) :ices)
                                                             (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted target)))))}
                                  :msg (msg "host itself on " (card-str state target))
                                  :effect (effect (host target card))}
                                 card nil)))}]
   :static-abilities [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (= (get-zone (:host card)) (get-zone target))))
                       :value 2}]})

(defcard "Saci"
  {:hosting {:req (req (and (ice? target)
                            (installed? target)
                            (can-host? state target)))}
   :events [{:event :rez
             :req (req (same-card? (:card context) (:host card)))
             :msg "gain 3 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 3))}
            {:event :derez
             :req (req (same-card? (:card context) (:host card)))
             ;; NOTE
             ;;   current guidance from rules is that saci doesn't get
             ;;   a payout on magnet rez, but does get one when magnet is
             ;;   derezzed.
             ;; - Apr 13 '24, nbkelly
             :msg "gain 3 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 3))}]})

(defcard "Sadyojata"
  (swap-with-in-hand "Sadyojata"
                     {:req (req (and (<= 3 (count (:subtypes current-ice)))
                                     (<= (get-strength current-ice) (get-strength card))))}))

(defcard "Sage"
  (mu-based-strength [(break-multiple-types
                        1 "Barrier"
                        1 "Code Gate")]))

(defcard "Sahasrara"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (program? target)))
                                :type :recurring}}})

(defcard "Saker"
  (return-and-derez (break-sub 1 1 "Barrier")
                    (strength-pump 2 2)))

(defcard "Savant"
  (mu-based-strength [(break-multiple-types
                        2 "Code Gate"
                        1 "Sentry")]))

(defcard "Savoir-faire"
  {:abilities [{:cost [(->c :credit 2)]
                :label "Install a program"
                :once :per-turn
                :req (req (not (install-locked? state side)))
                :msg (msg "install " (:title target) " from the grip")
                :prompt "Choose a program to install"
                :choices {:card #(and (program? %)
                                      (in-hand? %))}
                :async true
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]})

(defcard "Scheherazade"
  {:abilities [{:label "Install and host a program from the grip"
                :async true
                :cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :prompt "Choose a program in the grip"
                :choices {:req (req (and (program? target)
                                      (runner-can-install? state side target false)
                                      (in-hand? target)))}
                :msg (msg "install and host " (:title target) " and gain 1 [Credits]")
                :effect (req (wait-for (gain-credits state side 1)
                                       (runner-install state side
                                                       (assoc eid :source card :source-type :runner-install)
                                                       target {:host-card card})))}
               {:label "Host an installed program (manual)"
                :prompt "Choose an installed program"
                :choices {:card #(and (program? %)
                                      (installed? %))}
                :msg (msg "host " (:title target) " and gain 1 [Credits]")
                :async true
                :effect (req (if (host state side card target)
                               (gain-credits state side eid 1)
                               (effect-completed state side eid)))}]})

(defcard "Self-modifying Code"
  {:abilities [{:req (req (not (install-locked? state side)))
                :label "Install a program from the stack"
                :cost [(->c :trash-can) (->c :credit 2)]
                :async true
                :effect (effect (continue-ability
                                  {:prompt "Choose a program to install"
                                   :msg (msg (if (= target "Done")
                                               "shuffle the stack"
                                               (str "install " (:title target) " from the stack")))
                                   :choices (req (concat
                                                   (->> (:deck runner)
                                                        (filter
                                                          #(and (program? %)
                                                                (can-pay? state side
                                                                          (assoc eid :source card :source-type :runner-install)
                                                                          % nil [(->c :credit (install-cost state side %))])))
                                                        (sort-by :title)
                                                        (seq))
                                                  ["Done"]))
                                   :async true
                                   :effect (req (trigger-event state side :searched-stack)
                                                (shuffle! state side :deck)
                                                (if (= target "Done")
                                                  (effect-completed state side eid)
                                                  (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)))}
                                  card nil))}]})

(defcard "Sharpshooter"
  (auto-icebreaker {:abilities [(break-sub [(->c :trash-can)] 0 "Destroyer")
                                (strength-pump 1 2)]}))

(defcard "Shibboleth"
  (auto-icebreaker {:x-fn (req (if (threat-level 4 state) -2 0))
                    :abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 2 2)]
                    :static-abilities [(breaker-strength-bonus (get-x-fn))]}))

(defcard "Shiv"
  (break-and-enter "Sentry"))

(defcard "Slap Vandal"
  {:hosting {:req (req (and (ice? target)
                            (installed? target)
                            (can-host? state target)))}
   :abilities [(break-sub 1 1 "All" {:req (req (same-card? current-ice (:host card)))
                                     :repeatable false})]})

(defcard "Sneakdoor Beta"
  {:abilities [{:cost [(->c :click 1)]
                :msg "make a run on Archives"
                :makes-run true
                :async true
                :effect (effect (register-events
                                  card
                                  [{:event :pre-successful-run
                                    :duration :end-of-run
                                    :unregister-once-resolved true
                                    :interactive (req true)
                                    :msg "change the attacked server to HQ"
                                    :req (req (= :archives (-> run :server first)))
                                    :effect (req (swap! state assoc-in [:run :server] [:hq]))}])
                                (make-run eid :archives (get-card state card)))}]})

(defcard "Sneakdoor Prime A"
  {:abilities [{:cost [(->c :click 2)]
                :prompt "Choose a server"
                :choices (req (cancellable
                                (->> runnable-servers
                                     (map unknown->kw)
                                     (filter is-remote?)
                                     (map remote->name))))
                :msg "make a run on a remote server"
                :makes-run true
                :async true
                :effect (req (let [initial-server target]
                                  (register-events state side card
                                    [{:event :pre-successful-run
                                      :duration :end-of-run
                                      :unregister-once-resolved true
                                      :req (req (= (unknown->kw initial-server) (-> run :server first)))
                                      :prompt "Choose a server"
                                      :choices (req ["Archives" "R&D" "HQ"])
                                      :msg (msg "change the attacked server to " target)
                                      :effect (req (swap! state assoc-in [:run :server] [(unknown->kw target)]))}])
                                  (make-run state side eid initial-server card)))}]})

(defcard "Sneakdoor Prime B"
  {:abilities [{:cost [(->c :click 2)]
                :prompt "Choose a server"
                :choices (req (cancellable
                                (->> runnable-servers
                                     (map unknown->kw)
                                     (filter is-central?)
                                     (map central->name))))
                :msg "make a run on central server"
                :makes-run true
                :async true
                :effect (req (let [initial-server target]
                               (register-events state side card
                                 [{:event :pre-successful-run
                                   :duration :end-of-run
                                   :unregister-once-resolved true
                                   :req (req (= (unknown->kw initial-server) (-> run :server first)))
                                   :prompt "Choose a server"
                                   :choices (req (cancellable remotes))
                                   :msg (msg "change the attacked server to " target)
                                   :effect (req (swap! state assoc-in [:run :server] [(unknown->kw target)]))}])
                               (make-run state side eid initial-server card)))}]})

(defcard "Snitch"
  {:events [{:event :approach-ice
             :optional
             {:req (req (not (rezzed? (:ice context))))
              :prompt "Expose approached piece of ice?"
              :yes-ability
              {:async true
               :msg "expose the approached piece of ice"
               :effect (req (wait-for
                              (expose state side (:ice context))
                              (continue-ability state side (offer-jack-out) card nil)))}}}]})

(defcard "Snowball"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier"
                                           {:additional-ability {:msg "gain +1 strength for the remainder of the run"
                                                                 :effect (effect (pump card 1 :end-of-run))}})
                                (strength-pump 1 1)]}))

(defcard "Spike"
  (break-and-enter "Barrier"))

(defcard "Stargate"
  (let [ability (successful-run-replace-breach
                 {:target-server :rd
                  :mandatory true
                  :duration :end-of-run
                  :ability
                  {:async true
                   :msg (msg "reveal " (->> (:deck corp)
                                            (take 3)
                                            (map :title)
                                            (enumerate-str))
                             " from the top of R&D")
                   :effect (req (wait-for
                                 (reveal state side (take 3 (:deck corp)))
                                 (continue-ability
                                  state side
                                  {:async true
                                   :prompt "Choose a card to trash"
                                   :not-distinct true
                                   :choices (req (take 3 (:deck corp)))
                                   :msg (msg (let [card-titles (map :title (take 3 (:deck corp)))
                                                   target-position (first (positions #{target} (take 3 (:deck corp))))
                                                   position (case target-position
                                                              0 "top "
                                                              1 "middle "
                                                              2 "bottom "
                                                              "this-should-not-happen ")]
                                               (if (= 1 (count (filter #{(:title target)} card-titles)))
                                                 (str "trash " (:title target))
                                                 (str "trash " position (:title target)))))
                                   :effect (effect (trash :runner eid (assoc target :seen true) {:cause-card card}))}
                                  card nil)))}})]
    {:abilities [{:cost [(->c :click 1)]
                  :once :per-turn
                  :msg "make a run on R&D"
                  :makes-run true
                  :async true
                  :effect (effect (register-events card [ability])
                                  (make-run eid :rd card))}]}))

(defcard "Study Guide"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                {:cost [(->c :credit 2)]
                                 :msg "place 1 power counter"
                                 :effect (effect (add-counter card :power 1))}]
                    :static-abilities [(breaker-strength-bonus (req (get-counters card :power)))]}))

(defcard "Sūnya"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")]
                    :static-abilities [(breaker-strength-bonus (req (get-counters card :power)))]
                    :events [{:event :end-of-encounter
                              :req (req (all-subs-broken-by-card? (:ice context) card))
                              :msg "place 1 power counter on itself"
                              :effect (effect (add-counter card :power 1))}]}))

(defcard "Surfer"
  (letfn [(surf [state cice]
            {:prompt (msg "Choose a piece of ice before or after " (:title cice))
             :choices {:card #(and (ice? %)
                                   (= (get-zone %) (get-zone cice))
                                   (= 1 (abs (- (card-index state %)
                                                (card-index state cice)))))}
             :msg (msg "swap " (card-str state cice)
                       " and " (card-str state target))
             :effect (req (let [tgtndx (card-index state target)]
                            (when run (swap! state assoc-in [:run :position] (inc tgtndx)))
                            (swap-ice state side cice target)))})]
    {:abilities [{:cost [(->c :credit 2)]
                  :msg "swap a piece of Barrier ice"
                  :req (req (and (get-current-encounter state)
                                 (rezzed? current-ice)
                                 (has-subtype? current-ice "Barrier")))
                  :label "Swap the piece of Barrier ice currently being encountered with a piece of ice directly before or after it"
                  :async true
                  :effect (effect (continue-ability (surf state current-ice) card nil))}]}))

(defcard "Surveillance Network Key"
  {:implementation "Only implemented for click to draw"
   :events [{:event :corp-click-draw
             :msg (msg "reveal that they drew " (:title (:card context)))}]})

(defcard "Switchblade"
  (auto-icebreaker {:abilities [(break-sub (->c :credit 1 {:stealth 1}) 0 "Sentry")
                                (strength-pump (->c :credit 1 {:stealth 1}) 7 :end-of-encounter)]}))

(defcard "Takobi"
  {:events [{:event :subroutines-broken
             :optional {:req (req (all-subs-broken? target))
                        :prompt (msg "Place 1 power counter on " (:title card) "?")
                        :autoresolve (get-autoresolve :auto-place-counter)
                        :yes-ability
                        {:msg "place 1 power counter on itself"
                         :effect (effect (add-counter card :power 1))}}}]
   :abilities [{:req (req (get-current-encounter state))
                :cost [(->c :power 2)]
                :label "Give non-AI icebreaker +3 strength"
                :prompt "Choose an installed non-AI icebreaker"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                :msg (msg "give +3 strength to " (:title target))
                :effect (effect (pump target 3))}
               (set-autoresolve :auto-place-counter "Takobi placing power counters on itself")]})

(defcard "Tapwrm"
  (let [ability {:label "Gain [Credits] (start of turn)"
                 :msg (msg "gain " (quot (:credit corp) 5) " [Credits]")
                 :once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :async true
                 :effect (effect (gain-credits eid (quot (:credit corp) 5)))}]
    {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
     :flags {:drip-economy true}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)
              {:event :purge
               :async true
               :msg "trash itself"
               :effect (req (trash state :runner eid card {:cause :purge :cause-card card}))}]}))

(defcard "Torch"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "Tracker"
  (let [ability {:prompt "Choose a server"
                 :choices (req servers)
                 :msg (msg "target " target)
                 :req (req (not (:card-target card)))
                 :effect (effect (update! (assoc card :card-target target)))}
        prevent-sub {:event :pre-resolve-subroutine
                     :duration :end-of-run
                     :unregister-once-resolved true
                     :req (req true)
                     :effect (req (update-current-encounter state :prevent-subroutine true))
                     :msg (msg (str "prevent a subroutine (" (:label target) ") from resolving"))}]
    {:abilities [{:label "Make a run on targeted server"
                  :cost [(->c :click 1) (->c :credit 2)]
                  :req (req (some #(= (:card-target card) %) runnable-servers))
                  :msg (msg "make a run on " (:card-target card) ". Prevent the first subroutine that would resolve from resolving")
                  :async true
                  :effect (effect (register-events card [prevent-sub])
                                  (make-run eid (:card-target card) card))}]
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-turn-ends
               :effect (effect (update! (dissoc card :card-target)))}]}))

(defcard "Tranquilizer"
  (let [action (req (add-counter state side card :virus 1)
                    (when (and (rezzed? (get-card state (:host card)))
                               (<= 3 (get-virus-counters state (get-card state card))))
                      (derez state side (get-card state (:host card)))))]
    {:implementation "[Erratum] Program: Virus - Trojan"
     :hosting {:req (req (and (ice? target)
                              (installed? target)
                              (can-host? state target)))}
     :on-install {:interactive (req true)
                  :effect action}
     :events [{:event :runner-turn-begins
               :effect action}]}))

(defcard "Tremolo"
  (letfn [(credit-discount [s] (->> (all-installed-runner-type s :hardware)
                                    (filter #(has-subtype? % "Cybernetic"))
                                    count
                                    -))]
    (auto-icebreaker {:abilities [(break-sub 3 2 "Barrier"
                                             {:label "Break up to 2 Barrier subroutine"
                                              :break-cost-bonus (req [(->c :credit (max -3 (credit-discount state)))])})
                                  (strength-pump 2 2)]})))

(defcard "Trope"
  {:events [{:event :runner-turn-begins
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:action true
                :req (req (not (zone-locked? state :runner :discard)))
                :label "shuffle cards from heap into stack"
                :async true
                :effect
                (effect
                  (continue-ability
                    {:cost [(->c :click 1) (->c :remove-from-game)]
                     :label "Reshuffle cards from heap into stack"
                     :show-discard true
                     :choices {:max (min (get-counters card :power) (count (:discard runner)))
                               :all true
                               :card #(and (runner? %)
                                           (in-discard? %))}
                     :msg (msg "shuffle " (enumerate-str (map :title targets))
                               " into the stack")
                     :effect (req (doseq [c targets] (move state side c :deck))
                                  (shuffle! state side :deck))}
                    card nil))}]})

(defcard "Trypano"
  (let [trash-if-5 (req (let [h (get-card state (:host card))]
                          (if (and h
                                   (>= (get-virus-counters state card) 5)
                                   (not (and (card-flag? h :untrashable-while-rezzed true)
                                             (rezzed? h))))
                            (do (system-msg state :runner (str "uses " (:title card) " to trash " (card-str state h)))
                                (unregister-events state side card)
                                (trash state :runner eid h {:cause-card card}))
                            (effect-completed state side eid))))]
    {:implementation "[Erratum] Program: Virus - Trojan"
     :hosting {:req (req (and (ice? target)
                              (installed? target)
                              (can-host? state target)))}
     :on-install {:async true
                  :effect trash-if-5}
     :abilities [(set-autoresolve :auto-place-counter "Trypano placing virus counters on itself")]
     :events [{:event :runner-turn-begins
               :optional
               {:prompt (msg "Place 1 virus counter on " (:title card) "?")
                :autoresolve (get-autoresolve :auto-place-counter)
                :yes-ability {:msg "place 1 virus counter on itself"
                              :effect (req (add-counter state side card :virus 1))}}}
              {:event :counter-added
               :async true
               :effect trash-if-5}
              {:event :card-moved
               :async true
               :effect trash-if-5}
              {:event :runner-install
               :async true
               :effect trash-if-5}]}))

(defcard "Tunnel Vision"
  (auto-icebreaker {:events [mark-changed-event
                             (assoc identify-mark-ability :event :runner-turn-begins)]
                    :abilities [(break-sub 2 2 "All" {:req (req (= (:mark @state) (first (:server run))))})
                                (strength-pump 2 2)]}))

(defcard "Tycoon"
  (auto-icebreaker {:abilities [(break-sub 1 2 "Barrier")
                                (strength-pump 2 3)]
                    :events [{:event :end-of-encounter
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :msg "give the Corp 2 [Credits]"
                              :async true
                              :effect (effect (gain-credits :corp eid 2))}]}))

(defcard "Umbrella"
  (let [corp-draw {:optional {:prompt "Draw 1 card?"
                              :player :corp
                              :waiting-prompt true
                              :yes-ability {:async true
                                            :msg "draw 1 card"
                                            :effect (req (draw state :corp eid 1))}
                              :no-ability {:player :corp
                                           :effect (req (system-msg state side (str "declines to use " (:title card) " to draw 1 card")))}}}
        runner-draw {:label "Each player draws 1 card (manual)"
                     :optional {:prompt "Draw 1 card?"
                                :waiting-prompt true
                                :player :runner
                                :yes-ability {:async true
                                              :msg "draw 1 card"
                                              :effect (req (wait-for (draw state :runner 1)
                                                                     (continue-ability
                                                                       state side
                                                                       corp-draw
                                                                       card nil)))}
                                :no-ability {:async true
                                             :effect (req (system-msg state side (str "declines to use " (:title card) " to draw 1 card"))
                                                          (continue-ability
                                                            state side
                                                            corp-draw
                                                            card nil))}}}]
    (auto-icebreaker {:implementation "Draw doesn't work when mixing breakers"
                      :abilities [(break-sub 2 3 "Code Gate"
                                             {:req (req
                                                     (some #(and (has-subtype? % "Trojan") (program? %)) (:hosted current-ice)))})
                                  runner-draw]
                      :events [{:event :subroutines-broken
                                :req (req (every? #(or (= (:breaker %) nil) (= (:breaker %) (:cid card))) (:subroutines target)))
                                :effect (req (continue-ability state side runner-draw card nil))}]})))

(defcard "Unity"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 1 0 :end-of-encounter
                                               {:label "Add 1 strength for each installed icebreaker"
                                                :pump-bonus (req (count (filter #(and (program? %)
                                                                                      (has-subtype? % "Icebreaker"))
                                                                                (all-active-installed state :runner))))})]}))

(defcard "Upya"
  {:implementation "Power counters added automatically"
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :rd (target-server context)))
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:cost [(->c :click 1) (->c :power 3)]
                :once :per-turn
                :msg "gain [Click][Click]"
                :effect (effect (gain-clicks 2))}]})

(defcard "Utae"
  (let [break-req (:break-req (break-sub 1 1 "Code Gate"))]
    (auto-icebreaker {:abilities [{:label "Break X Code Gate subroutines"
                                   :cost [(->c :x-credits)]
                                   :break-cost [(->c :x-credits)]
                                   :once :per-run
                                   :req (req (and (break-req state side eid card targets)
                                                  (<= (get-strength current-ice) (get-strength card))))
                                   ; no break-req to not enable auto-pumping
                                   :msg (msg "break " (quantify (cost-value eid :x-credits) "subroutine")
                                             " on " (card-str state current-ice))
                                   :effect (effect
                                             (continue-ability
                                               (when (pos? (cost-value eid :x-credits))
                                                 (break-sub nil (cost-value eid :x-credits) "Code Gate"))
                                               card nil))}
                                  (break-sub 1 1 "Code Gate" {:label "Break 1 Code Gate subroutine (Virtual restriction)"
                                                              :req (req (<= 3 (count (filter #(has-subtype? % "Virtual")
                                                                                             (all-active-installed state :runner)))))})
                                  (strength-pump 1 1)]})))

(defcard "Vamadeva"
  (swap-with-in-hand "Vamadeva"
                     {:req (req (and (= 1 (count (:subroutines current-ice)))
                                     (<= (get-strength current-ice) (get-strength card))))}))

(defcard "Wari"
  (letfn [(prompt-for-subtype []
            {:prompt "Choose one"
             :choices ["Barrier" "Code Gate" "Sentry"]
             :async true
             :effect (req (wait-for (trash state side card {:unpreventable true
                                                            :cause-card card})
                                    (continue-ability state side
                                                      (expose-and-maybe-bounce target)
                                                      card nil)))})
          (expose-and-maybe-bounce [chosen-subtype]
            {:choices {:card #(and (ice? %)
                                   (not (rezzed? %)))}
             :async true
             :msg (str "name " chosen-subtype)
             :effect (req (wait-for (expose state side target)
                                    (when (has-subtype? async-result chosen-subtype)
                                      (do (move state :corp async-result :hand)
                                          (system-msg state :runner
                                                      (str "add " (:title async-result) " to HQ"))))
                                    (effect-completed state side eid)))})]
    {:events [{:event :successful-run
               :interactive (req true)
               :optional
               {:req (req (and (= :hq (target-server context))
                               (first-successful-run-on-server? state :hq)
                               (some #(and (ice? %)
                                           (not (rezzed? %)))
                                     (all-installed state :corp))))
                :prompt "Trash Wari to expose a piece of ice?"
                :yes-ability (prompt-for-subtype)}}]}))

(defcard "World Tree"
  (let [search-and-install
        (fn [trashed-card]
          {:prompt (msg "Choose a " (:type trashed-card) " to install")
           :req (req (not (install-locked? state side)))
           :msg (msg (if (= target "Done")
                       "shuffle the stack"
                       (str "install " (:title target) " from the stack, paying 3 [Credits] less")))
           :choices (req (concat
                           (->> (:deck runner)
                                (filter
                                  #(and (is-type? % (:type trashed-card))
                                        (can-pay? state side
                                                  (assoc eid :source card :source-type :runner-install)
                                                  % nil [(->c :credit (install-cost state side % {:cost-bonus -3}))])))
                                (sort-by :title)
                                (seq))
                           ["Done"]))
           :async true
           :effect (req (trigger-event state side :searched-stack)
                        (shuffle! state side :deck)
                        (if (= target "Done")
                          (effect-completed state side eid)
                          (runner-install state side (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3})))})]
    {:events [{:event :successful-run
               :interactive (req true)
               :req (req (and (first-event? state :runner :successful-run)
                              (>= (count (all-installed state :runner)) 2)))
               :async true
               :choices {:not-self true
                         :req (req (and (runner? target)
                                        (installed? target)))}
               :msg (msg "trash " (:title target))
               :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                      (effect-completed eid))
               :effect
               (req (let [facedown-target (facedown? target)]
                      (wait-for (trash state side target {:unpreventable true :cause-card card})
                                (if facedown-target
                                  (effect-completed state side eid)
                                  (continue-ability state side (search-and-install target) card nil)))))}]}))

(defcard "Wyrm"
  (auto-icebreaker {:abilities [(break-sub 3 1 "All" {:label "break 1 subroutine on a piece of ice with 0 or less strength"
                                                      :req (req (not (pos? (get-strength current-ice))))})
                                {:cost [(->c :credit 1)]
                                 :label "Give -1 strength to current piece of ice"
                                 :req (req (and (active-encounter? state)
                                                (<= (get-strength current-ice) (get-strength card))))
                                 :msg (msg "give -1 strength to " (:title current-ice))
                                 :effect (effect (pump-ice current-ice -1))}
                                (strength-pump 1 1)]}))

(defcard "Yog.0"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Code Gate")]}))

(defcard "Yusuf"
  (virus-breaker "Barrier"))

(defcard "ZU.13 Key Master"
  (cloud-icebreaker
    (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                  (strength-pump 1 1)]})))
