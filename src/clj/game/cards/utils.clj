(in-ns 'game.core)

;; Agenda-specific helper functions
(declare is-scored?)

(defn ice-boost-agenda [subtype]
  (letfn [(count-ice [corp]
            (reduce (fn [c server]
                      (+ c (count (filter #(and (has-subtype? % subtype)
                                                (rezzed? %))
                                          (:ices server)))))
                    0 (flatten (seq (:servers corp)))))]
    {:msg (msg "gain " (count-ice corp) " [Credits]")
     :interactive (req true)
     :effect (effect (gain :credit (count-ice corp))
                     (update-all-ice))
     :swapped {:effect (req (update-all-ice state side))}
     :events {:pre-ice-strength {:req (req (has-subtype? target subtype))
                                 :effect (effect (ice-strength-bonus 1 target))}}}))

;;; Asset-specific helper functions
(declare expose-prevent)
(declare in-server?)

(defn installed-access-trigger
  "Effect for triggering ambush on access.
  Ability is what happends upon access. If cost is specified Corp needs to pay that to trigger."
  ([cost ability]
   (let [ab (if (pos? cost) (assoc ability :cost [:credit cost]) ability)
         prompt (if (pos? cost)
                  (req (str "Pay " cost " [Credits] to use " (:title card) " ability?"))
                  (req (str "Use " (:title card) " ability?")))]
     (installed-access-trigger cost ab prompt)))
  ([cost ability prompt]
   {:access {:req (req (and installed (>= (:credit corp) cost)))
             :delayed-completion true
             :effect (effect (show-wait-prompt :runner (str "Corp to use " (:title card)))
                             (continue-ability
                              {:optional
                               {:prompt prompt
                                :yes-ability ability
                                :end-effect (effect (clear-wait-prompt :runner))}}
                              card nil))}}))

(defn advance-ambush
  "Creates advanceable ambush structure with specified ability for specified cost"
  ([cost ability] (assoc (installed-access-trigger cost ability) :advanceable :always))
  ([cost ability prompt] (assoc (installed-access-trigger cost ability prompt)
                           :advanceable :always)))

(defn campaign
  "Creates a Campaign with X counters draining Y per-turn.
  Trashes itself when out of counters"
  [counters per-turn]
  (let [ability {:msg (str "gain " per-turn " [Credits]")
                 :counter-cost [:credit per-turn]
                 :once :per-turn
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain " per-turn " [Credits] (start of turn)")
                 :effect (req (gain state :corp :credit per-turn)
                              (when (zero? (get-in card [:counter :credit]))
                                (trash state :corp card)))}]
    {:effect (effect (add-counter card :credit counters))
     :derezzed-events {:runner-turn-ends corp-rez-toast}
     :events {:corp-turn-begins ability}
     :abilities [ability]}))

(defn as-trashed-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points after resolving the trash prompt."
  ([state side card n] (as-trashed-agenda state side card n nil))
  ([state side card n options]
  (or (move state :runner (assoc (deactivate state side card) :agendapoints n) :scored options) ; if the runner did not trash the card on access, then this will work
      (move state :runner (assoc (deactivate state side card) :agendapoints n :zone [:discard]) :scored options)) ; allow force option in case of Blacklist/News Team
  (gain-agenda-point state side n)))

;; Event-specific helper functions
(defn- run-event
  ([] (run-event nil))
  ([run-ability] (run-event nil run-ability))
  ([cdef run-ability] (run-event cdef run-ability nil))
  ([cdef run-ability pre-run-effect]
   (run-event cdef run-ability pre-run-effect nil))
  ([cdef run-ability pre-run-effect post-run-effect]
   (merge {:prompt "Choose a server"
           :choices (req runnable-servers)
           :effect (effect ((or pre-run-effect (effect)) eid card targets)
                           (run target run-ability card)
                           ((or post-run-effect (effect)) eid card targets))}
          cdef)))

;; Helper functions specific for ICE
;;; Runner abilites for breaking subs
(declare trash-program trash-hardware trash-resource-sub trash-installed)

(defn runner-break
  "Ability to break a subroutine by spending a resource (Bioroids, Negotiator, Turing etc)"
  [cost subs]
  (let [cost-str (build-cost-str [cost])
        subs-str (quantify subs "subroutine")]
    {:cost cost
     :label (str "break " subs-str)
     :effect (req (system-msg state :runner (str "spends " cost-str " to break " subs-str " on " (:title card))))}))

;;; General subroutines
(def end-the-run
  "Basic ETR subroutine"
  {:label "End the run"
   :msg "end the run"
   :effect (effect (end-run))})

(def end-the-run-if-tagged
  "ETR subroutine if tagged"
  {:label "End the run if the Runner is tagged"
   :req (req tagged)
   :msg "end the run"
   :effect (effect (end-run))})

(def give-tag
  "Basic give runner 1 tag subroutine
   Mostly used with tag-trace"
  {:label "Give the Runner 1 tag"
   :msg "give the Runner 1 tag"
   :delayed-completion true
   :effect (effect (tag-runner :runner eid 1))})

(def add-power-counter
  "Adds 1 power counter to the card."
  {:label "Add 1 power counter"
   :msg "add 1 power counter"
   :effect (effect (add-counter card :power 1))})

(defn trace-ability
  "Run a trace with specified base strength.
   If successful trigger specified ability"
  [base ability]
  {:label (str "Trace " base " - " (:label ability))
   :trace (assoc ability :base base)})

(defn tag-trace
  "Trace ability for giving a tag, at specified base strength"
  [base]
  (trace-ability base give-tag))

(defn do-net-damage
  "Do specified amount of net-damage."
  [dmg]
  {:label (str "Do " dmg " net damage")
   :delayed-completion true
   :msg (str "do " dmg " net damage")
   :effect (effect (damage eid :net dmg {:card card}))})

(defn do-brain-damage
  "Do specified amount of brain damage."
  [dmg]
  {:label (str "Do " dmg " brain damage")
   :delayed-completion true
   :msg (str "do " dmg " brain damage")
   :effect (effect (damage eid :brain dmg {:card card}))})

(defn gain-credits
  "Gain specified amount of credits"
  [credits]
  {:label (str "Gain " credits " [Credits]")
   :msg (str "gain " credits " [Credits]")
   :effect (effect (gain :credit credits))})

(defn power-counter-ability
  "Does specified ability using a power counter."
  [{:keys [label message] :as ability}]
  (assoc ability :label (str "Hosted power counter: " label)
                 :msg (str message " using 1 power counter")
                 :counter-cost [:power 1]))

(defn do-psi
  "Start a psi game, if not equal do ability"
  ([{:keys [label] :as ability}]
  {:label (str "Psi Game - " label)
   :msg (str "start a psi game (" label ")")
   :psi {:not-equal ability}})
  ([{:keys [label-neq] :as neq-ability} {:keys [label-eq] :as eq-ability}]
   {:label (str "Psi Game - " label-neq " / " label-eq)
    :msg (str "start a psi game (" label-neq " / " label-eq ")")
    :psi {:not-equal neq-ability
          :equal     eq-ability}}))

(def take-bad-pub
  "Bad pub on rez effect."
  (effect (gain-bad-publicity :corp 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))

(def runner-loses-click
  "Runner loses a click effect"
  (req (if (:runner-phase-12 @state)
    ; this handles Jak Sinclair losing clicks before they are given
    (do (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
        (toast state :runner "Runner loses a click at start of turn" "warning")
        (toast state :corp "Runner loses a click at start of turn" "warning"))
    (lose state :runner :click 1))))

;;; For Advanceable ICE
(def advance-counters
  "Number of advancement counters - for advanceable ICE."
  (req (+ (:advance-counter card 0) (:extra-advance-counter card 0))))

(def space-ice-rez-bonus
  "Amount of rez reduction for the Space ICE."
  (req (* -3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))))

(defn space-ice
  "Creates data for Space ICE with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :rez-cost-bonus space-ice-rez-bonus})

;;; For Grail ICE
(defn grail-in-hand
  "Req that specified card is a Grail card in the Corp's hand."
  [card]
  (and (= (:side card) "Corp")
       (in-hand? card)
       (has-subtype? card "Grail")))

(def reveal-grail
  "Ability for revealing Grail ICE from HQ."
  {:label "Reveal up to 2 Grail ICE from HQ"
   :choices {:max 2
             :req grail-in-hand}
   :msg (let [sub-label #(:label (first (:subroutines (card-def %))))]
          (msg "reveal " (join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail ICE in HQ."
  {:label "Resolve a Grail ICE subroutine from HQ"
   :choices {:req grail-in-hand}
   :effect (req (doseq [ice targets]
                  (let [subroutine (first (:subroutines (card-def ice)))]
                    (resolve-ability state side subroutine card nil))))})

(defn grail-ice
  "Creates data for grail ICE"
  [ability]
  {:abilities [reveal-grail]
   :subroutines [ability resolve-grail]})

;;; For NEXT ICE
(defn next-ice-count
  "Counts number of rezzed NEXT ICE - for use with NEXT Bronze and NEXT Gold"
  [corp]
  (let [servers (flatten (seq (:servers corp)))
        rezzed-next? #(and (rezzed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c server] (+ c (count (filter rezzed-next? (:ices server))))) 0 servers)))

;;; For Morph ICE
(defn morph [state side card new old]
  (update! state side (assoc card
                        :subtype-target new
                        :subtype (combine-subtypes true
                                                   (remove-subtypes (:subtype card) old)
                                                   new)))
  (update-ice-strength state side card))

(defn morph-effect
  "Creates morph effect for ICE. Morphs from base type to other type"
  [base other]
  (req (if (odd? (get (get-card state card) :advance-counter 0))
         (morph state side card other base)
         (morph state side card base other))))

(defn morph-ice
  "Creates the data for morph ICE with specified types and ability."
  [base other ability]
  (let [ab {:req (req (= (:cid card) (:cid target)))
            :effect (morph-effect base other)}]
    {:advanceable :always
     :effect (morph-effect base other)
     :subroutines [ability]
     :events {:advance ab :advancement-placed ab}}))

;;; For Constellation ICE
(defn constellation-ice
  "Generates map for Constellation ICE with specified effect."
  [ability]
  {:subroutines [(trace-ability 2 (assoc ability :kicker (assoc ability :min 5)))]})

;;; Helper function for adding implementation notes to ICE defined with functions
(defn- implementation-note
  "Adds an implementation note to the ice-definition"
  [note ice-def]
  (assoc ice-def :implementation note))

;; Icebreaker-specific helper functions
(declare add-icon remove-icon can-host?)

(def breaker-auto-pump
  "Updates an icebreaker's abilities with a pseudo-ability to trigger the
  auto-pump routine in core, IF we are encountering a rezzed ice with a subtype
  we can break."
  {:effect
   (req (let [abs (filter #(not= (:dynamic %) :auto-pump) (:abilities card))
              pumpabi (some #(when (:pump %) %) abs)
              pumpcst (when pumpabi (second (drop-while #(and (not= % :credit)
                                                              (not= % "credit"))
                                                        (:cost pumpabi))))
              current-ice (when-not (get-in @state [:run :ending]) (get-card state current-ice))
              strdif (when current-ice (max 0 (- (or (:current-strength current-ice)
                                                     (:strength current-ice))
                                                 (or (:current-strength card)
                                                     (:strength card)))))
              pumpnum (when strdif (int (Math/ceil (/ strdif (:pump pumpabi)))))]
          (update! state side
                   (assoc card :abilities
                          (if (and pumpcst
                                   pumpnum
                                   (rezzed? current-ice)
                                   (or (some #(has-subtype? current-ice %) (:breaks card))
                                       (= (first (:breaks card)) "All"))
                                   (pos? strdif))
                            (vec (cons {:dynamic :auto-pump
                                        :cost [:credit (* pumpcst pumpnum)]
                                        :label (str "Match strength of " (:title current-ice))}
                                       abs))
                            abs)))))})

;; Takes a vector of ice subtypes that can be broken (or ["All"] for
;; AI breakers) and a card definition, and returns a new card definition that
;; hooks up breaker-auto-pump to the necessary events.
;; IMPORTANT: Events on cdef take precedence, and should call
;; (:effect breaker-auto-pump) themselves.
(defn auto-icebreaker [breaks cdef]
  (assoc cdef :data (merge {:breaks breaks} (:data cdef))
              :events (merge {:run breaker-auto-pump
                              :pass-ice breaker-auto-pump
                              :run-ends breaker-auto-pump
                              :ice-strength-changed breaker-auto-pump
                              :ice-subtype-changed breaker-auto-pump
                              :breaker-strength-changed breaker-auto-pump
                              :approach-ice breaker-auto-pump }
                             (:events cdef))))

(defn cloud-icebreaker [cdef]
  (assoc cdef :effect (req (add-watch state (keyword (str "cloud" (:cid card)))
                        (fn [k ref old new]
                          (when (and (< (get-in old [:runner :link]) 2)
                                     (> (get-in new [:runner :link]) 1))
                            (gain state :runner :memory (:memoryunits card)))
                          (when (and (> (get-in old [:runner :link]) 1)
                                     (< (get-in new [:runner :link]) 2))
                            (lose state :runner :memory (:memoryunits card))))))
              :leave-play (req (remove-watch state (keyword (str "cloud" (:cid card))))
                               (when (> (get-in @state [:runner :link]) 1)
                                 (lose state :runner :memory (:memoryunits card))))
              :install-cost-bonus (req (when (> (get-in @state [:runner :link]) 1)
                                         [:memory (* -1 (:memoryunits card))]))))

(defn- strength-pump
  "Creates a strength pump ability.
  Cost can be a credit amount or a list of costs e.g. [:credit 2]."
  ([cost strength] (strength-pump cost strength nil))
  ([cost strength all-run]
   {:msg (str "add " strength " strength" (when all-run " for the remainder of the run"))
    :cost [:credit cost]
    :effect (effect (pump card strength (or all-run :encounter)))
    :pump strength}))

(defn- break-sub
  "Creates a break subroutine ability.
  If n = 0 then any number of subs are broken."
  ([cost n] (break-sub cost n nil))
  ([cost n subtype] (break-sub cost n subtype nil))
  ([cost n subtype effect]
   {:msg (str "break "
              (when (> n 1) "up to ")
              (if (pos? n) n "any number of")
              (when subtype (str " " subtype))
              (pluralize " subroutine" n))
    :cost [:credit cost]
    :effect effect}))

;;; Breaker sets
(defn- cerberus
  "Breaker from the dog set"
  [type]
  (auto-icebreaker [type]
                   {:data {:counter {:power 4}}
                    :abilities [{:counter-cost [:power 1]
                                 :msg (str "break up to 2 " (lower-case type) " subroutines")}
                                (strength-pump 1 1)]}))

(defn- break-and-enter
  "Breakers from the Break and Entry set"
  [type]
  (cloud-icebreaker {:abilities [{:label (str "[Trash]: Break up to 3 " (lower-case type) "subroutines")
                                  :msg (str "break up to 3 " (lower-case type) " subroutines")
                                  :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:silent (req true)
                                           :req (req (has-subtype? target "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:runner-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                          (all-active-installed state :runner))))}))

(defn- global-sec-breaker
  "GlobalSec breakers for Sunny"
  [type]
  (cloud-icebreaker (auto-icebreaker [type] {:abilities [(break-sub 2 0 (lower-case type))
                                                         (strength-pump 2 3)]})))

(defn- deva
  "Deva breakers"
  [name]
  (auto-icebreaker ["All"]
                   {:abilities [(break-sub 1 1 "ICE")
                                (strength-pump 1 1)
                                {:req (req (seq (filter #(has-subtype? % "Deva") (:hand runner))))
                                 :label "Swap with a deva program from your Grip" :cost [:credit 2]
                                 :prompt (str "Select a deva program in your Grip to swap with " name)
                                 :choices {:req #(and in-hand? (has-subtype? % "Deva"))}
                                 :msg (msg "swap in " (:title target) " from their Grip")
                                 :effect (req (if-let [hostcard (:host card)]
                                                (let [hosted (host state side (get-card state hostcard) target)]
                                                  (card-init state side hosted {:resolve-effect false
                                                                                :init-data true}))
                                                (let [devavec (get-in @state [:runner :rig :program])
                                                      devaindex (first (keep-indexed #(when (= (:cid %2) (:cid card)) %1) devavec))
                                                      newdeva (assoc target :zone (:zone card) :installed true)
                                                      newvec (apply conj (subvec devavec 0 devaindex) newdeva (subvec devavec devaindex))]
                                                  (lose state :runner :memory (:memoryunits card))
                                                  (swap! state assoc-in [:runner :rig :program] newvec)
                                                  (swap! state update-in [:runner :hand] (fn [coll] (remove-once #(= (:cid %) (:cid target)) coll)))
                                                  (card-init state side newdeva {:resolve-effect false
                                                                                 :init-data true})))
                                              (move state side card :hand))}]}))

(defn- conspiracy
  "Install-from-heap breakers"
  [title type abilities]
  (let [install-prompt {:req (req (and (= (:zone card) [:discard])
                                       (rezzed? current-ice)
                                       (has-subtype? current-ice type)
                                       (not (install-locked? state side))
                                       (not (some #(= title (:title %)) (all-active-installed state :runner)))
                                       (not (get-in @state [:run :register :conspiracy (:cid current-ice)]))))
                        :optional {:player :runner
                                   :prompt (str "Install " title "?")
                                   :yes-ability {:effect (effect (unregister-events card)
                                                                 (runner-install :runner card))}
                                   :no-ability {:effect (req  ;; Add a register to note that the player was already asked about installing,
                                                              ;; to prevent multiple copies from prompting multiple times.
                                                              (swap! state assoc-in [:run :register :conspiracy (:cid current-ice)] true))}}}
        heap-event (req (when (= (:zone card) [:discard])
                          (unregister-events state side card)
                          (register-events state side
                                           {:rez install-prompt
                                            :approach-ice install-prompt
                                            :run install-prompt}
                                           (assoc card :zone [:discard]))))]
    {:move-zone heap-event
     :events {:rez nil
              :approach-ice nil
              :run nil}
     :abilities abilities}))

(defn- central-breaker
  "'Cannot be used on a remote server' breakers"
  [type break pump]
  (let [central-req (req (or (not (:central-breaker card)) (#{:hq :rd :archives} (first (:server run)))))]
    (auto-icebreaker [type]
                     {:abilities [(assoc break :req central-req)
                                  (assoc pump :req central-req)]
                      :effect (effect (update! (assoc card :central-breaker true)))})))

;; Identity-specific helper functions
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

;; Program-specific helper functions
(declare can-host?)

;; Resource-specific helper functions
(declare close-access-prompt)

(defn- genetics-trigger?
  "Returns true if Genetics card should trigger - does not work with Adjusted Chronotype"
  [state side event]
  (or (first-event? state side event)
      (and (has-flag? state side :persistent :genetics-trigger-twice)
           (second-event? state side event))))

(defn- shard-constructor
  "Function for constructing a Shard card"
  ([target-server message effect-fn] (shard-constructor target-server message nil effect-fn))
  ([target-server message ability-options effect-fn]
   (letfn [(can-install-shard? [state run] (and run
                                                (= (:server run) [target-server])
                                                (zero? (:position run))
                                                (not (:access @state))))]
     {:implementation "Click Shard to install when last ICE is passed, but before hitting Successful Run button"
      :abilities [(merge {:effect (effect (trash card {:cause :ability-cost}) (effect-fn eid card target))
                          :msg message}
                         ability-options)]
      :install-cost-bonus (req (when (can-install-shard? state run) [:credit -15 :click -1]))
      :effect (req (when (can-install-shard? state run)
                     (when-completed (register-successful-run state side (:server run))
                                     (do (clear-wait-prompt state :corp)
                                         (swap! state update-in [:runner :prompt] rest)
                                         (handle-end-run state side)))))})))

;; Upgrade-specific helper functions
(declare expose-prevent)
