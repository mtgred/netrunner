(ns game.cards.programs
  (:require [game.core :refer :all]
            [game.core.eid :refer [effect-completed make-eid]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.core.card :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability when-let*]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer :all]))

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
              strdif (when (and (get-strength current-ice)
                                (get-strength card))
                       (max 0 (- (get-strength current-ice)
                                 (get-strength card))))
              pumpnum (when (and strdif
                                 (:pump pumpabi))
                        (int (Math/ceil (/ strdif (:pump pumpabi)))))]
          (update! state side
                   (assoc card :abilities
                          (if (and pumpcst
                                   pumpnum
                                   (rezzed? current-ice)
                                   (or (some #(has-subtype? current-ice %) (:breaks card))
                                       (= (first (:breaks card)) "All"))
                                   (pos? pumpnum))
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
  (assoc cdef
         :data (merge {:breaks breaks} (:data cdef))
         :events (merge {:run breaker-auto-pump
                         :pass-ice breaker-auto-pump
                         :run-ends breaker-auto-pump
                         :ice-strength-changed breaker-auto-pump
                         :ice-subtype-changed breaker-auto-pump
                         :breaker-strength-changed breaker-auto-pump
                         :approach-ice breaker-auto-pump}
                        (:events cdef))))

(defn- wrestling-breaker
  "Laamb and Engolo. Makes currently encountered ice gain chosen type until end of encounter."
  [cost ice-type]
  {:once :per-turn
   :cost [:credit cost]
   :label (str "Make currently encountered ice gain " ice-type)
   :msg (msg "make " (:title current-ice) " gain " ice-type)
   :req (req (and current-ice
                  (rezzed? current-ice)
                  (not (has-subtype? current-ice ice-type))))
   :effect (req (let [ice current-ice
                      stargets (:subtype-target ice)
                      stypes (:subtype ice)
                      remove-subtype {:effect
                                      (effect (update! (assoc (get-card state ice)
                                                              :subtype-target stargets
                                                              :subtype stypes))
                                              (unregister-events card)
                                              (register-events (:events (card-def card)) card))}]
                  (update! state side (assoc ice
                                             :subtype-target (combine-subtypes true stargets ice-type)
                                             :subtype (combine-subtypes true stypes ice-type)))
                  (update-ice-strength state side (get-card state ice))
                  (register-events state side {:pass-ice remove-subtype
                                               :run-ends remove-subtype} card)))})

(defn cloud-icebreaker [cdef]
  (assoc cdef
         :effect (req (let [link (get-in @state [:runner :link] 0)]
                        (when (>= link 2)
                          (free-mu state (:memoryunits card))))
                      (add-watch state (keyword (str "cloud" (:cid card)))
                                 (fn [k ref old new]
                                   (let [old-link (get-in old [:runner :link] 0)
                                         new-link (get-in new [:runner :link] 0)
                                         cloud-turned-on (and (< old-link 2)
                                                              (>= new-link 2))
                                         cloud-turned-off (and (>= old-link 2)
                                                               (< new-link 2))]
                                     (cond
                                       cloud-turned-on
                                       (free-mu state (:memoryunits card))
                                       cloud-turned-off
                                       (use-mu state (:memoryunits card)))))))
         :leave-play (req (remove-watch state (keyword (str "cloud" (:cid card))))
                          (let [link (get-in @state [:runner :link] 0)]
                            (when (>= link 2)
                              ;; To counteract the normal freeing of MU on program `:leave-play`
                              (use-mu state (:memoryunits card)))))))

(defn- strength-pump
  "Creates a strength pump ability.
  Cost can be a credit amount or a list of costs e.g. [:credit 2]."
  ([cost strength] (strength-pump cost strength :encounter))
  ([cost strength duration]
   {:msg (str "add " strength " strength"
              (cond
                (= duration :all-run)
                " for the remainder of the run"
                (= duration :all-turn)
                " for the remainder of the turn"))
    :cost [:credit cost]
    :effect (effect (pump card strength duration))
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
  [ice-type]
  (auto-icebreaker [ice-type]
                   {:data {:counter {:power 4}}
                    :abilities [{:counter-cost [:power 1]
                                 :msg (str "break up to 2 " (lower-case ice-type) " subroutines")}
                                (strength-pump 1 1)]}))

(defn- break-and-enter
  "Breakers from the Break and Entry set"
  [ice-type]
  (cloud-icebreaker {:abilities [{:label (str "Break up to 3 " (lower-case ice-type) "subroutines")
                                  :msg (str "break up to 3 " (lower-case ice-type) " subroutines")
                                  :cost [:trash]}]
                     :events (let [cloud {:silent (req true)
                                          :req (req (has-subtype? target "Icebreaker"))
                                          :effect (effect (update-breaker-strength card))}]
                               {:runner-install cloud :trash cloud :card-moved cloud})
                     :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                         (all-active-installed state :runner))))}))

(defn- global-sec-breaker
  "GlobalSec breakers for Sunny"
  [ice-type]
  (cloud-icebreaker (auto-icebreaker [ice-type] {:abilities [(break-sub 2 0 (lower-case ice-type))
                                                             (strength-pump 2 3)]})))

(defn- deva
  "Deva breakers"
  [card-name]
  (auto-icebreaker ["All"]
                   {:abilities [(break-sub 1 1 "ICE")
                                (strength-pump 1 1)
                                {:req (req (seq (filter #(has-subtype? % "Deva") (:hand runner))))
                                 :label "Swap with a deva program from your Grip"
                                 :cost [:credit 2]
                                 :prompt (str "Select a deva program in your Grip to swap with " card-name)
                                 :choices {:req #(and in-hand? (has-subtype? % "Deva"))}
                                 :msg (msg "swap in " (:title target) " from their Grip")
                                 :effect (req (if-let [hostcard (:host card)]
                                                (let [hosted (host state side (get-card state hostcard) target)]
                                                  (card-init state side hosted {:resolve-effect false
                                                                                :init-data true}))
                                                (let [devavec (get-in @state [:runner :rig :program])
                                                      devaindex (first (keep-indexed #(when (same-card? %2 card) %1) devavec))
                                                      newdeva (assoc target :zone (:zone card) :installed true)
                                                      newvec (apply conj (subvec devavec 0 devaindex) newdeva (subvec devavec devaindex))]
                                                  (lose state :runner :memory (:memoryunits card))
                                                  (swap! state assoc-in [:runner :rig :program] newvec)
                                                  (swap! state update-in [:runner :hand] (fn [coll] (remove-once #(same-card? % target) coll)))
                                                  (card-init state side newdeva {:resolve-effect false
                                                                                 :init-data true})))
                                              (move state side card :hand))}]}))

(defn- conspiracy
  "Install-from-heap breakers"
  [title ice-type abilities]
  (let [install-prompt {:req (req (and (in-discard? card)
                                       (rezzed? current-ice)
                                       (has-subtype? current-ice ice-type)
                                       (not (install-locked? state :runner))))
                        :async true
                        :effect (effect (continue-ability
                                          {:optional {:req (req (and (not-any? #(= title (:title %)) (all-active-installed state :runner))
                                                                     (not (get-in @state [:run :register :conspiracy (:cid current-ice)]))))
                                                      :player :runner
                                                      :prompt (str "Install " title "?")
                                                      :yes-ability {:effect (effect (unregister-events card)
                                                                                    (runner-install :runner card))}
                                                      ;; Add a register to note that the player was already asked about installing,
                                                      ;; to prevent multiple copies from prompting multiple times.
                                                      :no-ability {:effect (req (swap! state assoc-in [:run :register :conspiracy (:cid current-ice)] true))}}}
                                          card targets))}
        heap-event (req (when (in-discard? card)
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
  [ice-type break pump]
  (let [central-req (req (or (not (:central-breaker card)) (#{:hq :rd :archives} (first (:server run)))))]
    (auto-icebreaker [ice-type]
                     {:abilities [(assoc break :req central-req)
                                  (assoc pump :req central-req)]
                      :effect (effect (update! (assoc card :central-breaker true)))})))

(defn- ancient-greek-breaker
  "Adept, Sage and Savant. Strength depends on available memory units."
  [card-name abilities]
  {:abilities abilities
   :effect (req (add-watch state (keyword (str card-name (:cid card)))
                           (fn [k ref old new]
                             (when (not= (available-mu (atom old))
                                         (available-mu (atom new)))
                               (update-breaker-strength ref side card))))
                (update-breaker-strength state side card))
   :leave-play (req (remove-watch state (keyword (str card-name (:cid card)))))
   :strength-bonus (req (available-mu state))})

(defn- khumalo-breaker
  "Spends virus counters from any card to pump/break, gains virus counters for successful runs."
  [ice-type]
  (let [type-subroutine (str ice-type " subroutine")
        add-strength (fn [state card message n]
                       (dotimes [_ n]
                         (pump state :runner (get-card state card) 1))
                       (system-msg state :runner
                                   (str "spends " message
                                        " to add " n
                                        " strength")))]
    {:events {:successful-run {:silent (req true)
                               :effect (effect (system-msg (str "adds 1 virus counter to " (:title card)))
                                               (add-counter card :virus 1))}}
     :abilities [{:label (str "Break one or more " type-subroutine "s")
                  :effect (req (wait-for (resolve-ability
                                           state side (pick-virus-counters-to-spend) card nil)
                                         (when-let* [message (:msg async-result)
                                                     n (:number async-result)]
                                           (system-msg state :runner
                                                       (str "spends " message
                                                            " to break "
                                                            (quantify n type-subroutine))))))}
                 {:label "Match strength of currently encountered ice"
                  :req (req (and current-ice
                                 (> (ice-strength state side current-ice)
                                    (get-strength card))))
                  :effect (req (wait-for (resolve-ability
                                           state side
                                           (pick-virus-counters-to-spend
                                             (- (ice-strength state side current-ice)
                                                (get-strength card)))
                                           card nil)
                                         (when-let* [message (:msg async-result)
                                                     n (:number async-result)]
                                           (add-strength state card message n))))}
                 {:label "Add strength"
                  :effect (req (wait-for (resolve-ability
                                           state side (pick-virus-counters-to-spend) card nil)
                                         (when-let* [message (:msg async-result)
                                                     n (:number async-result)]
                                           (add-strength state card message n))))}]}))

;; Card definitions
(def card-definitions
  {"Abagnale"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 2 2)
                                 {:label "Bypass Code Gate being encountered"
                                  :req (req (has-subtype? current-ice "Code Gate"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :cost [:trash]}]})

   "Adept"
   (ancient-greek-breaker "adept" [{:cost [:credit 2]
                                    :req (req (or (has-subtype? current-ice "Barrier")
                                                  (has-subtype? current-ice "Sentry")))
                                    :msg "break 1 Sentry or Barrier subroutine"}])

   "Aghora"
   (deva "Aghora")

   "Algernon"
   {:events
    {:runner-turn-begins
     {:req (req (can-pay? state :runner eid card nil [:credit 2]))
      :optional
      {:prompt (msg "Pay 2 [Credits] to gain [Click]")
       :player :runner
       :yes-ability {:msg "gain [Click]"
                     :effect (req (gain state :runner :click 1)
                                  (update! state :runner (assoc-in (get-card state card) [:special :used-algernon] true)))
                     :cost [:credit 2]}}}
     :runner-turn-ends
     {:async true
      :effect (req (if (get-in card [:special :used-algernon])
                     (do
                       (update! state :runner (dissoc-in card [:special :used-algernon]))
                       (if-not (:successful-run runner-reg)
                         (do
                           (system-msg state :runner "trashes Algernon because a successful run did not occur")
                           (trash state :runner eid card nil))
                         (effect-completed state side eid)))
                     (effect-completed state side eid)))}}}

   "Alias"
   (central-breaker "Sentry"
                    (break-sub 1 1 "Sentry")
                    (strength-pump 2 3))

   "Alpha"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (= (:position run) (count run-ices)))
                                  :msg "break 1 subroutine on the outermost ICE protecting this server"}
                                 (strength-pump 1 1)]})

   "Amina"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 3 "Code Gate")
                                 (strength-pump 2 3)
                                 {:once :per-turn
                                  :label "Corp loses 1 [Credits]"
                                  :req (req (and (has-subtype? current-ice "Code Gate")
                                                 (rezzed? current-ice)))
                                  :msg (msg "make the Corp lose 1 [Credits]")
                                  :effect (effect (lose-credits :corp 1))}]})

   "Analog Dreamers"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on R&D"
                 :makes-run true
                 :effect (effect
                           (make-run :rd
                                     {:req (req (= target :rd))
                                      :replace-access
                                      {:prompt "Choose a card to shuffle into R&D"
                                       :choices {:req #(and (not (ice? %))
                                                            (not (rezzed? %))
                                                            (zero? (get-counters % :advancement)))}
                                       :effect (req (move state :corp target :deck)
                                                    (shuffle! state :corp :deck)
                                                    (swap! state update-in [:runner :prompt] rest)
                                                    (handle-end-run state side)) ; remove the replace-access prompt
                                       :msg "shuffle a card into R&D"}}
                                     card))}]}

   "Ankusa"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 1 1)
                                 {:label "Add Barrier to HQ"
                                  :req (req (and (has-subtype? current-ice "Barrier")
                                                 (rezzed? current-ice)))
                                  :msg (msg "add " (:title current-ice) " to HQ after breaking all its subroutines")
                                  :effect (req (let [c current-ice]
                                                 (move state :corp c :hand nil)
                                                 (continue state side nil)))}]})

   "Atman"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :abilities [(break-sub 1 1)]
    :strength-bonus (req (get-counters card :power))
    :events {:counter-added {:req (req (same-card? target card))
                             :effect (effect (update-breaker-strength card))}}}

   "Au Revoir"
   {:events {:jack-out {:effect (effect (gain-credits 1))
                        :msg "gain 1 [Credits]"}}}

   "Aumakua"
   {:implementation "Add counters manually for access outside of a run or cards that replace access like Ash"
    ; We would need a :once :per-access key to make this work for Gang Sign etc.
    :abilities [(break-sub 1 1)
                {:label "Add a virus counter"
                 :effect (effect (system-msg "manually adds a virus counter to Aumakua")
                                 (add-counter card :virus 1))}]
    :strength-bonus (req (get-virus-counters state card))
    :events {:run-ends {:req (req (and (not (or (get-in @state [:run :did-trash])
                                                (get-in @state [:run :did-steal])))
                                       (get-in @state [:run :did-access])))
                        :effect (effect (add-counter card :virus 1))}
             :expose {:effect (effect (add-counter card :virus 1))}
             :counter-added {:req (req (same-card? target card))
                             :effect (effect (update-breaker-strength card))}}}

   "Aurora"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 2 3)]})

   "Baba Yaga"
   (let [host-click {:cost [:click 1]
                     :label "Install a non-AI icebreaker on Baba Yaga"
                     :prompt "Choose a non-AI icebreaker in your Grip to install on Baba Yaga"
                     :choices {:req #(and (has-subtype? % "Icebreaker")
                                          (not (has-subtype? % "AI"))
                                          (in-hand? %))}
                     :effect (effect (runner-install target {:host-card card}))}
         host-free {:label "Host an installed non-AI icebreaker on Baba Yaga"
                    :prompt "Choose an installed non-AI icebreaker to host on Baba Yaga"
                    :choices {:req #(and (has-subtype? % "Icebreaker")
                                         (not (has-subtype? % "AI"))
                                         (installed? %))}
                    :effect (req (when (host state side card target)
                                   (gain :memory (:memoryunits target))))}
         gain-abis (req (let [new-abis (mapcat (fn [c] (map-indexed #(assoc %2
                                                                            :dynamic :copy
                                                                            :source (:title c)
                                                                            :index %1
                                                                            :label (make-label %2))
                                                                    (filter #(not= :manual-state (:ability-type %))
                                                                            (:abilities (card-def c)))))
                                               (:hosted card))]
                          (update! state :runner (assoc card :abilities (concat new-abis [host-click host-free])))))]
     {:abilities [host-click host-free]
      :hosted-gained gain-abis
      :hosted-lost gain-abis})

   "Bankroll"
   {:implementation "Bankroll gains credits automatically."
    :events {:successful-run {:effect (effect (add-counter card :credit 1)
                                              (system-msg "places 1 [Credit] on Bankroll"))}}
    :abilities [{:label "Take all credits from Bankroll"
                 :async true
                 ;; Cannot trash unless there are counters (so game state changes)
                 :req (req (pos? (get-counters card :credit)))
                 :msg (msg "gain " (get-counters card :credit) " credits")
                 :cost [:trash]
                 :effect (effect (gain-credits (get-counters card :credit)))}]}

   "Battering Ram"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 1 1 :all-run)]})

   "Berserker"
   {:implementation "Encounter effect is manual. Only gains strength on printed subs"
    :abilities [{:label "Gain strength per barrier subroutine"
                 :req (req (and (rezzed? current-ice)
                                (has-subtype? current-ice "Barrier")))
                 :msg (msg "gain " (count-num-subroutines current-ice) " strength")
                 :effect (effect (pump card (count-num-subroutines current-ice)))}
                (break-sub 2 2 "Barrier")]}

   "Bishop"
   {:abilities [{:cost [:click 1]
                 :effect (req (let [b (get-card state card)
                                    hosted? (ice? (:host b))
                                    remote? (is-remote? (second (:zone (:host b))))]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Host Bishop on a piece of ICE protecting "
                                                (if hosted? (if remote? "a central" "a remote") "any") " server")
                                   :choices {:req #(if hosted?
                                                     (and (if remote?
                                                            (is-central? (second (:zone %)))
                                                            (is-remote? (second (:zone %))))
                                                          (ice? %)
                                                          (can-host? %)
                                                          (= (last (:zone %)) :ices)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa"))
                                                                    (:hosted %)))
                                                     (and (ice? %)
                                                          (can-host? %)
                                                          (= (last (:zone %)) :ices)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa"))
                                                                    (:hosted %))))}
                                   :msg (msg "host it on " (card-str state target))
                                   :effect (effect (host target card))} card nil)))}]
    :events {:pre-ice-strength
             {:req (req (and (= (:cid target)
                                (:cid (:host card)))
                             (:rezzed target)))
              :effect (effect (ice-strength-bonus -2 target))}}}

   "Black Orchestra"
   (conspiracy "Black Orchestra" "Code Gate"
               [{:cost [:credit 3]
                 :effect (effect (pump card 2))
                 :pump 2
                 :msg "add 2 strength and break up to 2 subroutines"}])

   "BlacKat"
   {:implementation "Stealth credit restriction not enforced"
    :abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 1]
                 :msg "break up to 3 Barrier subroutines (using a stealth [Credits])"}
                (strength-pump 2 1)
                {:cost [:credit 2]
                 :msg "add 2 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 2))
                 :pump 2}]}

   "Blackstone"
   {:abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 3]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run))
                 :pump 4}]}

   "Brahman"
   (auto-icebreaker ["All"]
                    {:abilities [(break-sub 1 2 "ICE" (effect (update! (assoc-in card [:special :brahman-used] true))))
                                 (strength-pump 2 1)]
                     :events (let [put-back {:req (req (get-in card [:special :brahman-used]))
                                             :player :runner ; Needed for when the run is ended by the Corp
                                             :prompt "Choose a non-virus program to put on top of your stack."
                                             :choices {:req #(and (installed? %)
                                                                  (program? %)
                                                                  (not (facedown? %))
                                                                  (not (has-subtype? % "Virus")))}
                                             :msg (msg "add " (:title target) " to the top of the Stack")
                                             :effect (effect (update! (dissoc-in card [:special :brahman-used]))
                                                             (move target :deck {:front true}))}]
                               {:pass-ice put-back
                                :run-ends put-back})})

   "Breach"
   (central-breaker "Barrier"
                    (break-sub 2 3 "Barrier")
                    (strength-pump 2 4))

   "Bug"
   {:implementation "Can only pay to see last card drawn after multiple draws"
    :req (req (some #{:hq} (:successful-run runner-reg)))
    :events {:corp-draw {:optional
                         {:prompt (msg "Pay 2 [Credits] to reveal card just drawn?")
                          :player :runner
                          :yes-ability {:msg (msg "reveal the card just drawn: " (:title (last (:hand corp))))
                                        :cost [:credit 2]}}}}}

   "Bukhgalter"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 1)
                                 {:once :per-turn
                                  :label "Gain 2 [Credits]"
                                  :req (req (and (has-subtype? current-ice "Sentry")
                                                 (rezzed? current-ice)))
                                  :msg (msg "gain 2 [Credits]")
                                  :effect (effect (gain-credits :runner 2))}]})

   "Cache"
   {:abilities [{:counter-cost [:virus 1]
                 :effect (effect (gain-credits 1))
                 :msg "gain 1 [Credits]"}]
    :data {:counter {:virus 3}}}

   "Cerberus \"Cuj.0\" H3"
   (cerberus "Sentry")

   "Cerberus \"Lady\" H1"
   (cerberus "Barrier")

   "Cerberus \"Rex\" H2"
   (cerberus "Code Gate")

   "Chakana"
   {:leave-play (effect (update-all-advancement-costs))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :virus 1))}
             :pre-advancement-cost {:req (req (>= (get-virus-counters state card) 3))
                                    :effect (effect (advancement-cost-bonus 1))}
             :counter-added
             {:req (req (or (= (:title target) "Hivemind") (same-card? target card)))
              :effect (effect (update-all-advancement-costs))}
             :purge {:effect (effect (update-all-advancement-costs))}}}

   "Chameleon"
   {:prompt "Choose one subtype"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "choose " target)
    :effect (effect (update! (assoc card :subtype-target target)))
    :events {:runner-turn-ends {:msg "add itself to Grip" :effect (effect (move card :hand))}}
    :abilities [{:cost [:credit 1] :msg (msg "break 1 " (:subtype-target card) " subroutine")}]}

   "Chisel"
   {:implementation "Encounter effect is manual."
    :hosting {:req (every-pred ice? can-host?)}
    :abilities [{:req (req (and (same-card? current-ice (:host card))
                                (rezzed? current-ice)))
                 :effect (req (if (not (pos? (get-strength current-ice)))
                                (do (system-msg state side (str "uses Chisel to trash " (card-str state current-ice)))
                                    (trash state side current-ice))
                                (do (system-msg state side (str "places 1 virus counter on " (card-str state current-ice)))
                                    (add-counter state side card :virus 1))))}]
    :events {:counter-added {:req (req (or (same-card? target card)
                                           (= (:title target) "Hivemind")))
                             :effect (effect (update-ice-strength (:host card)))}
             :pre-ice-strength {:req (req (same-card? target (:host card)))
                                :effect (effect (ice-strength-bonus (- (get-virus-counters state card)) target))}}}

   "Cloak"
   {:recurring 1
    :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                                (has-subtype? target "Icebreaker")))
                                 :type :recurring}}}

   "Clot"
   {:effect (req (let [agendas (map first (filter #(agenda? (first %))
                                                  (turn-events state :corp :corp-install)))]
                   (swap! state assoc-in [:corp :register :cannot-score] agendas)))
    :events {:purge {:effect (req (swap! state update-in [:corp :register] dissoc :cannot-score)
                                  (trash state side card {:cause :purge}))}
             :corp-install {:req (req (agenda? target))
                            :effect (req (swap! state update-in [:corp :register :cannot-score] #(cons target %)))}}
    :leave-play (req (swap! state update-in [:corp :register] dissoc :cannot-score))}

   "Collective Consciousness"
   {:events {:rez {:req (req (ice? target))
                   :msg "draw 1 card"
                   :async true
                   :effect (effect (draw :runner eid 1 nil))}}}

   "Consume"
   {:events {:runner-trash {:async true
                            :req (req (some corp? targets))
                            :effect (req (let [amt-trashed (count (filter corp? targets))
                                               sing-ab {:optional {:prompt "Place a virus counter on Consume?"
                                                                   :autoresolve (get-autoresolve :auto-accept)
                                                                   :yes-ability {:effect (effect (add-counter :runner card :virus 1))
                                                                                 :msg "place 1 virus counter on Consume"}}}
                                               mult-ab {:prompt "Place virus counters on Consume?"
                                                        :choices {:number (req amt-trashed)
                                                                  :default (req amt-trashed)}
                                                        :msg (msg "place " (quantify target "virus counter") " on Consume")
                                                        :effect (effect (add-counter :runner card :virus target))}
                                               ab (if (= 1 amt-trashed) sing-ab mult-ab)]
                                           (continue-ability state side ab card targets)))}}
    :abilities [{:req (req (pos? (get-virus-counters state card)))
                 :cost [:click 1]
                 :label "Gain 2 [Credits] for each hosted virus counter, then remove all virus counters."
                 :effect (req (gain-credits state side (* 2 (get-virus-counters state card)))
                              (update! state side (assoc-in card [:counter :virus] 0))
                              (when-let [hiveminds (filter #(= "Hivemind" (:title %)) (all-active-installed state :runner))]
                                (doseq [h hiveminds]
                                  (update! state side (assoc-in h [:counter :virus] 0)))))
                 :msg (msg (let [local-virus (get-counters card :virus)
                                 global-virus (get-virus-counters state card)
                                 hivemind-virus (- global-virus local-virus)]
                             (str "gain " (* 2 global-virus) " [Credits], removing " (quantify local-virus "virus counter") " from Consume"
                                  (when (pos? hivemind-virus)
                                    (str " (and " hivemind-virus " from Hivemind)")))))}
                (set-autoresolve :auto-accept "adding virus counters")]}

   "Copycat"
   {:abilities [{:req (req (and (:run @state)
                                (:rezzed current-ice)))
                 :effect (req (let [icename (:title current-ice)]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Choose a rezzed copy of " icename)
                                   :choices {:req #(and (rezzed? %)
                                                        (ice? %)
                                                        (= (:title %) icename))}
                                   :msg "redirect the run"
                                   :cost [:trash]
                                   :effect (req (let [dest (second (:zone target))
                                                      tgtndx (ice-index state target)]
                                                  (swap! state update-in [:run]
                                                         #(assoc % :position tgtndx :server [dest]))))}
                                  card nil)))}]}

   "Corroder"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 1 1)]})

   "Cradle"
   {:abilities [(break-sub 2 0 "Code Gate")]
    :events {:card-moved {:silent (req true)
                          :req (req (and (= "Runner" (:side target))
                                         (= [:hand] (or (:zone target)
                                                        (:previous-zone target)))))
                          :effect (effect (update-breaker-strength card))}
             :runner-draw {:silent (req true)
                           :req (req (when-let [drawn (-> @state :runner :register :most-recent-drawn first)]
                                       (= [:hand] (or (:zone drawn)
                                                      (:previous-zone drawn)))))
                           :effect (effect (update-breaker-strength card))} }
    :strength-bonus (req (-> @state :runner :hand count -))}

   "Creeper"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [(break-sub 2 1 "Sentry")
                                   (strength-pump 1 1)]}))

   "Crescentus"
   {:implementation "Does not check that all subroutines were broken"
    :abilities [{:req (req (rezzed? current-ice))
                 :msg (msg "derez " (:title current-ice))
                 :cost [:trash]
                 :effect (effect (derez current-ice))}]}

   "Crowbar"
   (break-and-enter "Code Gate")

   "Crypsis"
   (auto-icebreaker ["All"]
                    {:abilities [(break-sub 1 1 "ICE" (effect (update! (assoc card :crypsis-broke true))))
                                 (strength-pump 1 1)
                                 {:cost [:click 1]
                                  :msg "place 1 virus counter"
                                  :effect (effect (add-counter card :virus 1))}]
                     :events (let [encounter-ends-effect
                                   {:req (req (:crypsis-broke card))
                                    :effect (req ((:effect breaker-auto-pump) state side eid card targets)
                                                 (if (pos? (get-counters card :virus))
                                                   (add-counter state side card :virus -1)
                                                   (trash state side card {:cause :self-trash}))
                                                 (update! state side (dissoc (get-card state card) :crypsis-broke)))}]
                               {:pass-ice encounter-ends-effect
                                :run-ends encounter-ends-effect})
                     :move-zone (req (when (= [:discard] (:zone card))
                                       (update! state side (dissoc card :crypsis-broke))))})

   "Customized Secretary"
   (letfn [(custsec-host [cards]
             {:prompt "Choose a program to host on Customized Secretary"
              :choices (cons "None" cards)
              :async true
              :effect (req (if (or (= target "None") (not (program? target)))
                             (do (clear-wait-prompt state :corp)
                                 (shuffle! state side :deck)
                                 (system-msg state side (str "shuffles their Stack"))
                                 (effect-completed state side eid))
                             (do (host state side (get-card state card) target)
                                 (system-msg state side (str "hosts " (:title target) " on Customized Secretary"))
                                 (continue-ability state side (custsec-host (remove-once #(= % target) cards))
                                                   card nil))))})]
     {:async true
      :interactive (req (some #(card-flag? % :runner-install-draw true) (all-active state :runner)))
      :msg (msg "reveal the top 5 cards of their Stack: " (join ", " (map :title (take 5 (:deck runner)))))
      :effect (req (reveal state side (take 5 (:deck runner)))
                   (show-wait-prompt state :corp "Runner to host programs on Customized Secretary")
                   (let [from (take 5 (:deck runner))]
                     (continue-ability state side (custsec-host from) card nil)))
      :abilities [{:cost [:click 1]
                   :prompt "Choose a program hosted on Customized Secretary to install"
                   :choices (req (cancellable (filter #(can-pay? state side eid card nil :credit (:cost %))
                                                      (:hosted card))))
                   :msg (msg "install " (:title target))
                   :effect (req (when (can-pay? state side eid card nil :credit (:cost target))
                                  (runner-install state side (make-eid state {:source card :source-type :runner-install}) target nil)))}]})

   "Cyber-Cypher"
   (auto-icebreaker ["Code Gate"]
                    {:prompt "Choose a server where this copy of Cyber-Cypher can be used:"
                     :msg (msg "target " target)
                     :choices (req servers)
                     :effect (effect (update! (assoc card :server-target target)))
                     :leave-play (effect (update! (dissoc card :server-target)))
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "D4v1d"
   {:implementation "Does not check that ICE strength is 5 or greater"
    :data {:counter {:power 3}}
    :abilities [{:counter-cost [:power 1]
                 :msg "break 1 subroutine"}]}

   "Dagger"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 5)]})

   "Dai V"
   (auto-icebreaker ["All"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [{:cost [:credit 2]
                                  :msg "break all ICE subroutines (using stealth [Credits])"}
                                 (strength-pump 1 1)]})

   "Darwin"
   {:flags {:runner-phase-12 (req true)}
    :events {:purge {:effect (effect (update-breaker-strength card))}}
    :abilities [(break-sub 2 1 "ICE")
                {:label "Place 1 virus counter (start of turn)"
                 :once :per-turn
                 :cost [:credit 1]
                 :msg "place 1 virus counter"
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (add-counter card :virus 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (or (get-virus-counters state card) 0))}

   "Datasucker"
   {:events (let [ds {:effect (req (update! state side (dissoc card :datasucker-count)))}]
              {:successful-run {:silent (req true)
                                :effect (effect (add-counter card :virus 1))
                                :req (req (#{:hq :rd :archives} target))}
               :pre-ice-strength {:req (req (and (same-card? target current-ice)
                                                 (:datasucker-count card)))
                                  :effect (req (let [c (:datasucker-count (get-card state card))]
                                                 (ice-strength-bonus state side (- c) target)))}
               :pass-ice ds :run-ends ds})
    :abilities [{:counter-cost [:virus 1]
                 :msg (msg "give -1 strength to " (:title current-ice))
                 :req (req (and current-ice (:rezzed current-ice)))
                 :effect (req (update! state side (update-in card [:datasucker-count] (fnil #(+ % 1) 0)))
                              (update-ice-strength state side current-ice))}]}

   "DaVinci"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:effect
                 (req (let [c card]
                        (resolve-ability state side
                                         {:prompt "Choose a card to install from your Grip"
                                          :choices {:req #(and (<= (:cost %) (get-counters c :power))
                                                               (#{"Hardware" "Program" "Resource"} (:type %))
                                                               (in-hand? %))}
                                          :req (req (not (install-locked? state side)))
                                          :msg (msg "install " (:title target) " at no cost")
                                          :cost [:trash]
                                          :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:ignore-install-cost true}))}
                                         card nil)))}]}

   "Deep Thought"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :virus 1))
                              :req (req (= target :rd))}
             :runner-turn-begins
             {:req (req (>= (get-virus-counters state card) 3)) :msg "look at the top card of R&D"
              :effect (effect (prompt! card (str "The top card of R&D is "
                                                 (:title (first (:deck corp)))) ["OK"] {}))}}}

   "Demara"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 2 3)
                                 {:label "Bypass Barrier being encountered"
                                  :req (req (has-subtype? current-ice "Barrier"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :cost [:trash]}]})

   "Deus X"
   {:interactions {:prevent [{:type #{:net}
                              :req (req true)}]}
    :abilities [{:msg "break any number of AP subroutines"
                 :cost [:trash]}
                {:msg "prevent any amount of net damage"
                 :cost [:trash]
                 :effect (effect (damage-prevent :net Integer/MAX_VALUE))}]}

   "Dhegdheer"
   {:abilities [{:label "Install a program on Dhegdheer"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Dhegdheer"
                                    :choices {:req #(and (program? %)
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg (str "host " (:title target)
                                                   (when (-> target :cost pos?)
                                                     ", lowering its cost by 1 [Credit]")))
                                    :effect (effect (when (-> target :cost pos?)
                                                      (install-cost-bonus state side [:credit -1]))
                                                    (runner-install (assoc eid :source card :source-type :runner-install) target {:host-card card :no-mu true})
                                                    (update! (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}
                                   card nil))}
                {:label "Host an installed program on Dhegdheer with [Credit] discount"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :prompt "Choose an installed program to host on Dhegdheer with [Credit] discount"
                 :choices {:req #(and (program? %)
                                      (installed? %))}
                 :msg (msg (str "host " (:title target)
                                (when (-> target :cost pos?)
                                  ", lowering its cost by 1 [Credit]")))
                 :effect (req (free-mu state (:memoryunits target))
                              (when (-> target :cost pos?)
                                (gain-credits state side 1))
                              (update-breaker-strength state side target)
                              (host state side card (get-card state target))
                              (update! state side (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}
                {:label "Host an installed program on Dhegdheer"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :prompt "Choose an installed program to host on Dhegdheer"
                 :choices {:req #(and (program? %)
                                      (installed? %))}
                 :msg (msg (str "host " (:title target)
                                (when (-> target :cost pos?)
                                  ", lowering its cost by 1 [Credit]")))
                 :effect (effect (free-mu (:memoryunits target))
                                 (update-breaker-strength target)
                                 (host card (get-card state target))
                                 (update! (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}]
    :events {:card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :dheg-prog])))
                          :effect (effect (update! (dissoc-in card [:special :dheg-prog]))
                                          (use-mu (:memoryunits target)))}}}

   "Disrupter"
   {:events
    {:pre-init-trace
     {:async true
      :effect (effect (show-wait-prompt :corp "Runner to use Disrupter")
                      (continue-ability
                        :runner
                        {:optional
                         {:prompt "Use Disrupter's ability?"
                          :yes-ability
                          {:cost [:trash]
                           :effect (req (swap! state assoc-in [:trace :force-base] 0))}
                          :end-effect (effect (clear-wait-prompt :corp))}}
                        card nil))}}}

   "Diwan"
   {:prompt "Choose the server that this copy of Diwan is targeting:"
    :choices (req servers)
    :effect (effect (update! (assoc card :server-target target)))
    :events {:purge {:effect (effect (trash card {:cause :purge}))}
             :pre-corp-install
             {:req (req (let [c target
                              serv (:server (second targets))]
                          (and (= serv (:server-target card))
                               (not (and (is-central? serv)
                                         (upgrade? c))))))
              :effect (effect (install-cost-bonus [:credit 1]))}}}

   "Djinn"
   {:abilities [{:label "Search your Stack for a virus program and add it to your Grip"
                 :prompt "Choose a Virus"
                 :msg (msg "add " (:title target) " to their Grip")
                 :choices (req (cancellable (filter #(and (program? %)
                                                          (has-subtype? % "Virus"))
                                                    (:deck runner)) :sorted))
                 :cost [:click 1 :credit 1]
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (move target :hand))}
                {:label "Install a non-Icebreaker program on Djinn"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a non-Icebreaker program in your Grip to install on Djinn"
                                    :choices {:req #(and (program? %)
                                                         (runner-can-install? state side % false)
                                                         (not (has-subtype? % "Icebreaker"))
                                                         (in-hand? %))}
                                    :msg (msg "install and host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc (get-card state card)
                                                                    :hosted-programs
                                                                    (cons (:cid target) (:hosted-programs card)))))}
                                   card nil))}
                {:label "Host an installed non-Icebreaker program on Djinn"
                 :prompt "Choose an installed non-Icebreaker program to host on Djinn"
                 :choices {:req #(and (program? %)
                                      (not (has-subtype? % "Icebreaker"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (free-mu (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (use-mu (:memoryunits target)))}}}

   "Eater"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :msg "break ICE subroutine and access 0 cards this run"
                                  :effect (effect (max-access 0))}
                                 (strength-pump 1 1)]})

   "Egret"
   {:implementation "Added subtypes don't get removed when Egret is moved/trashed"
    :hosting {:req #(and (ice? %) (can-host? %) (rezzed? %))}
    :msg (msg "make " (card-str state (:host card)) " gain Barrier, Code Gate and Sentry subtypes")
    :effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (update-ice-strength state side h)
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))
    :events {:ice-strength-changed
             {:effect (req (unregister-events state side card)
                           (when (get-in card [:special :installing])
                             (update! state side (assoc (:host (get-card state card)) :subtype (combine-subtypes false(-> card :host :subtype) "Barrier" "Code Gate" "Sentry")))
                             (update! state side (update-in card [:special] dissoc :installing))
                             (trigger-event state side :runner-install card))
                           (continue state side nil))}}}

   "Endless Hunger"
   {:abilities [{:label "Break 1 \"End the run.\" subroutine"
                 :msg (msg "break 1 \"[Subroutine] End the run.\" subroutine")
                 :cost [:installed 1]}]}

   "Engolo"
   (auto-icebreaker
     ["Code Gate"]
     {:abilities [(break-sub 1 1 "Code Gate")
                  (strength-pump 2 4)
                  (wrestling-breaker 2 "Code Gate")]})

   "Equivocation"
   (let [force-draw (fn [title]
                      {:optional {:prompt (str "Force the Corp to draw " title "?")
                                  :yes-ability {:async true
                                                :effect (req (show-wait-prompt state :runner "Corp to draw")
                                                             (wait-for (draw state :corp 1 nil)
                                                                       (do (system-msg state :corp (str "is forced to draw " title))
                                                                           (clear-wait-prompt state :runner)
                                                                           (effect-completed state side eid))))}}})
         reveal {:optional {:prompt "Reveal the top card of R&D?"
                            :yes-ability {:async true
                                          :effect (req (let [topcard (-> corp :deck first :title)]
                                                         (reveal state side topcard)
                                                         (system-msg state :runner (str "reveals " topcard
                                                                                        " from the top of R&D"))
                                                         (continue-ability state side (force-draw topcard) card nil)))}}}]
     {:events {:successful-run {:req (req (= target :rd))
                                :async true
                                :interactive (req true)
                                :effect (effect (continue-ability reveal card nil))}}})

   "eXer"
   {:in-play [:rd-access 1]
    :events {:purge {:effect (effect (trash card {:cause :purge}))}}}

   "Expert Schedule Analyzer"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on HQ"
                 :makes-run true
                 :effect (effect (make-run :hq {:req (req (= target :hq))
                                                :replace-access
                                                {:msg (msg "reveal cards in HQ: "
                                                           (join ", " (map :title (:hand corp))))}} card))}]}

   "Faerie"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 0 1 "Sentry" (effect (update! (assoc-in card [:special :faerie-used] true))))
                                 (strength-pump 1 1)]
                     :events {:pass-ice {:req (req (get-in card [:special :faerie-used]))
                                         :effect (effect (trash card))}}})

   "False Echo"
   {:abilities [{:req (req (and run
                                (< (:position run) (count run-ices))
                                (not (rezzed? current-ice))))
                 :msg "make the Corp rez the passed ICE or add it to HQ"
                 :effect (req (let [s (:server run)
                                    ice (nth (get-in @state (vec (concat [:corp :servers] s [:ices]))) (:position run))
                                    icename (:title ice)
                                    icecost (rez-cost state side ice)]
                                (continue-ability
                                  state side
                                  {:prompt (msg "Rez " icename " or add it to HQ?") :player :corp
                                   :choices (req (if (< (:credit corp) icecost)
                                                   ["Add to HQ"]
                                                   ["Rez" "Add to HQ"]))
                                   :effect (req (if (= target "Rez")
                                                  (rez state side ice)
                                                  (do (move state :corp ice :hand nil)
                                                      (system-msg state :corp (str "chooses to add the passed ICE to HQ"))))
                                                (trash state side card))}
                                  card nil)))}]}

   "Faust"
   {:abilities [{:label "Trash 1 card from Grip to break 1 subroutine"
                 :cost [:trash-from-hand 1]
                 :msg (msg "break 1 subroutine")}
                {:label "Trash 1 card from Grip to add 2 strength"
                 :cost [:trash-from-hand 1]
                 :msg (msg "add 2 strength")
                 :effect (effect (pump card 2))}]}

   "Fawkes"
   {:implementation "Stealth credit restriction not enforced"
    :abilities [(break-sub 1 1 "Sentry")
                {:label "X [Credits]: +X strength for the remainder of the run (using at least 1 stealth [Credits])"
                 :choices :credit
                 :prompt "How many credits?"
                 :effect (effect (pump card target :all-run))
                 :msg (msg "increase strength by " target " for the remainder of the run")}]}

   "Femme Fatale"
   (auto-icebreaker ["Sentry"]
                    {:prompt "Select a piece of ICE to target for bypassing"
                     :choices {:req ice?}
                     :leave-play (req (remove-icon state side card))
                     :effect (req (let [ice target]
                                    (add-icon state side card ice "F" "blue")
                                    (system-msg state side
                                                (str "selects " (card-str state ice)
                                                     " for Femme Fatale's bypass ability"))))
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1)]})

   "Flashbang"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(strength-pump 1 1)
                                 {:label "Derez a Sentry being encountered"
                                  :cost [:credit 6]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Sentry")))
                                  :msg (msg "derez " (:title current-ice))
                                  :effect (effect (derez current-ice))}]})

   "Force of Nature"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 2 "Code Gate")
                                 (strength-pump 1 1)]})

   "Garrote"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 1)]})

   "Gauss"
   (auto-icebreaker ["Barrier"]
                    {:strength-bonus (req (if (= :this-turn (installed? card)) 3 0))
                     :events (let [losestr {:effect (effect (update-breaker-strength card))}]
                               {:runner-turn-ends losestr
                                :corp-turn-ends losestr})
                     :abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 2)]})

   "Gingerbread"
   (auto-icebreaker ["Tracer"]
                    {:abilities [(break-sub 1 1 "Tracer")
                                 (strength-pump 2 3)]})

   "God of War"
   (auto-icebreaker ["All"]
                    {:flags {:runner-phase-12 (req true)}
                     :abilities [(strength-pump 2 1)
                                 {:counter-cost [:virus 1]
                                  :msg "break 1 subroutine"}
                                 {:label "Take 1 tag to place 2 virus counters (start of turn)"
                                  :once :per-turn
                                  :effect (req (wait-for (gain-tags state :runner 1)
                                                         (if (not (get-in @state [:tag :tag-prevent]))
                                                           (do (add-counter state side card :virus 2)
                                                               (system-msg state side
                                                                           (str "takes 1 tag to place 2 virus counters on God of War"))
                                                               (effect-completed state side eid))
                                                           (effect-completed state side eid))))}]})

   "Golden"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 2 2 "Sentry")
                                 (strength-pump 2 4)
                                 {:label "Derez a Sentry"
                                  :cost [:credit 2 :return-to-hand]
                                  :req (req (and (rezzed? current-ice)
                                                 (has-subtype? current-ice "Sentry")))
                                  :msg (msg "derez " (:title current-ice))
                                  :effect (effect (derez current-ice))}]})

   "Gordian Blade"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1 :all-run)]})

   "Gorman Drip v1"
   {:abilities [{:cost [:click 1 :trash]
                 :effect (effect (gain-credits (get-virus-counters state card)))
                 :msg (msg "gain " (get-virus-counters state card) " [Credits]")}]
    :events {:corp-click-credit {:effect (effect (add-counter :runner card :virus 1))}
             :corp-click-draw {:effect (effect (add-counter :runner card :virus 1))}}}

   "Grappling Hook"
   {:abilities [{:msg "break all but 1 subroutine"
                 :cost [:trash]}]}

   "Gravedigger"
   {:events (let [e {:req (req (and (installed? target) (= (:side target) "Corp")))
                     :effect (effect (add-counter :runner card :virus 1))}]
              {:runner-trash e :corp-trash e})
    :abilities [{:counter-cost [:virus 1]
                 :cost [:click 1]
                 :msg "force the Corp to trash the top card of R&D"
                 :effect (effect (mill :corp))}]}

   "GS Sherman M3"
   (global-sec-breaker "Barrier")

   "GS Shrike M2"
   (global-sec-breaker "Sentry")

   "GS Striker M1"
   (global-sec-breaker "Code Gate")

   "Harbinger"
   {:trash-effect
    {:req (req (not-any? #{:facedown :hand} (:previous-zone card)))
     :effect (req (let [lock (get-in @state [:runner :locked :discard])]
                    (swap! state assoc-in [:runner :locked] nil)
                    (runner-install state :runner (assoc eid :source card :source-type :runner-install) card {:facedown true})
                    (swap! state assoc-in [:runner :locked] lock)))}}

   "Hemorrhage"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :virus 1))}}
    :abilities [{:counter-cost [:virus 2]
                 :cost [:click 1]
                 :req (req (pos? (count (:hand corp))))
                 :msg "force the Corp to trash 1 card from HQ"
                 :effect (req (show-wait-prompt state :runner "Corp to trash a card from HQ")
                              (resolve-ability
                                state :corp
                                {:prompt "Choose a card to trash"
                                 :choices (req (filter corp? (:hand corp)))
                                 :effect (effect (trash target)
                                                 (clear-wait-prompt :runner))}
                                card nil))}]}

   "Hivemind"
   (let [update-programs (req (let [virus-programs (->> (all-installed state :runner)
                                                        (filter #(and (program? %)
                                                                      (has-subtype? % "Virus")
                                                                      (not (facedown? %)))))]
                                (doseq [p virus-programs]
                                  (update-breaker-strength state side p))))]
     {:data {:counter {:virus 1}}
      :effect update-programs
      :trash-effect {:effect update-programs}
      :events {:counter-added {:req (req (same-card? target card))
                               :effect update-programs}}
      :abilities [{:req (req (pos? (get-counters card :virus)))
                   :priority true
                   :prompt "Move a virus counter to which card?"
                   :choices {:req #(has-subtype? % "Virus")}
                   :effect (req (let [abilities (:abilities (card-def target))
                                      virus target]
                                  (add-counter state :runner virus :virus 1)
                                  (add-counter state :runner card :virus -1)
                                  (if (= (count abilities) 1)
                                    (do (swap! state update-in [side :prompt] rest) ; remove the Hivemind prompt so Imp works
                                        (resolve-ability state side (first abilities) (get-card state virus) nil))
                                    (resolve-ability
                                      state side
                                      {:prompt "Choose an ability to trigger"
                                       :choices (vec (map :msg abilities))
                                       :effect (req (swap! state update-in [side :prompt] rest)
                                                    (resolve-ability
                                                      state side
                                                      (first (filter #(= (:msg %) target) abilities))
                                                      card nil))}
                                      (get-card state virus) nil))))
                   :msg (msg "trigger an ability on " (:title target))}]})

   "Houdini"
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run)) :pump 4}]}

   "Hyperdriver"
   {:flags {:runner-phase-12 (req true)}
    :abilities [{:label "Remove Hyperdriver from the game to gain [Click] [Click] [Click]"
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (move card :rfg) (gain :click 3))
                 :msg "gain [Click][Click][Click]"}]}

   "Ika"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 2 "Sentry")
                                 (strength-pump 2 3)
                                 {:label "Host Ika on a piece of ICE"
                                  :prompt (msg "Host Ika on a piece of ICE")
                                  :cost [:credit 2]
                                  :choices {:req #(and (ice? %)
                                                       (installed? %)
                                                       (can-host? %))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))}]})

   "Imp"
   {:data {:counter {:virus 2}}
    :interactions {:access-ability {:label "Trash card"
                                    :req (req (and (not (get-in @state [:per-turn (:cid card)]))
                                                   (pos? (get-counters card :virus))))
                                    :counter-cost [:virus 1]
                                    :msg (msg "trash " (:title target) " at no cost")
                                    :once :per-turn
                                    :async true
                                    :effect (effect (trash-no-cost eid target))}}}

   "Incubator"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :virus 1))}}
    :abilities [{:cost [:click 1 :trash]
                 :msg (msg "move " (get-counters card :virus) " virus counter to " (:title target))
                 :choices {:req #(and (installed? %)
                                      (has-subtype? % "Virus"))}
                 :effect (effect (add-counter target :virus (get-counters card :virus)))}]}

   "Inti"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 1 :all-run)]})

   "Inversificator"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "No restriction on which pieces of ICE are chosen"
                     :abilities [{:label "Swap the Code Gate you just passed with another ICE"
                                  :once :per-turn
                                  :req (req (:run @state))
                                  :prompt "Select the Code Gate you just passed and another piece of ICE to swap positions"
                                  :choices {:req #(and (installed? %) (ice? %)) :max 2}
                                  :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                  :effect (req (when (= (count targets) 2)
                                                 (swap-ice state side (first targets) (second targets))))}
                                 (break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Ixodidae"
   {:events {:corp-credit-loss {:msg "gain 1 [Credits]"
                                :effect (effect (gain-credits :runner 1))}
             :purge {:effect (effect (trash card {:cause :purge}))}}}

   "Keyhole"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on R&D"
                 :makes-run true
                 :effect (effect (make-run :rd
                                           {:req (req (= target :rd))
                                            :replace-access
                                            {:prompt "Choose a card to trash"
                                             :not-distinct true
                                             :msg (msg "trash " (:title target))
                                             :choices (req (take 3 (:deck corp)))
                                             :mandatory true
                                             :effect (effect (trash (assoc target :seen true))
                                                             (shuffle! :corp :deck))}} card))}]}

   "Knight"
   {:abilities [{:label "Host Knight on a piece of ICE"
                 :effect (req (let [k (get-card state card)
                                    hosted (ice? (:host k))
                                    icepos (ice-index state (get-card state (:host k)))]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Host Knight on a piece of ICE" (when hosted " not before or after the current host ICE"))
                                   :cost [:click 1]
                                   :choices {:req #(if hosted
                                                     (and (or (when (= (:zone %) (:zone (:host k)))
                                                                (not= 1 (abs (- (ice-index state %) icepos))))
                                                              (not= (:zone %) (:zone (:host k))))
                                                          (ice? %)
                                                          (can-host? %)
                                                          (installed? %)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %)))
                                                     (and (ice? %)
                                                          (installed? %)
                                                          (can-host? %)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %))))}
                                   :msg (msg "host it on " (card-str state target))
                                   :effect (effect (host target card))} card nil)))}
                {:cost [:credit 2]
                 :req (req (ice? (get-nested-host card)))
                 :msg "break 1 subroutine on the host ICE"}]}

   "Kyuban"
   {:hosting {:req #(and (ice? %) (can-host? %))}
    :events {:pass-ice {:req (req (same-card? target (:host card)))
                        :msg "gain 2 [Credits]"
                        :effect (effect (gain-credits :runner 2))}}}

   "Laamb"
   (auto-icebreaker
     ["Barrier"]
     {:abilities [(break-sub 2 0 "Barrier")
                  (strength-pump 3 6)
                  (wrestling-breaker 2 "Barrier")]})

   "Lamprey"
   {:events {:successful-run {:req (req (= target :hq))
                              :msg "force the Corp to lose 1 [Credits]"
                              :effect (effect (lose-credits :corp 1))}
             :purge {:effect (effect (trash card {:cause :purge}))}}}

   "Leprechaun"
   {:abilities [{:label "Install a program on Leprechaun"
                 :req (req (< (count (get-in card [:special :hosted-programs])) 2))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Leprechaun"
                                    :choices {:req #(and (program? %)
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc-in (get-card state card)
                                                                       [:special :hosted-programs]
                                                                       (cons (:cid target)
                                                                             (get-in card [:special :hosted-programs])))))}
                                   card nil))}
                {:label "Host an installed program on Leprechaun"
                 :req (req (< (count (get-in card [:special :hosted-programs])) 2))
                 :prompt "Choose an installed program to host on Leprechaun"
                 :choices {:req #(and (program? %)
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (free-mu (:memoryunits target))
                                 (update-breaker-strength target)
                                 (host card (get-card state target))
                                 (update! (assoc-in (get-card state card)
                                                    [:special :hosted-programs]
                                                    (cons (:cid target)
                                                          (get-in card [:special :hosted-programs])))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (get-in card [:special :hosted-programs])))
                          :effect (effect (update! (assoc-in card
                                                             [:special :hosted-programs]
                                                             (remove #(= (:cid target) %)
                                                                     (get-in card [:special :hosted-programs]))))
                                          (use-mu (:memoryunits target)))}}}

   "Leviathan"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 3 3 "Code Gate")
                                 (strength-pump 3 5)]})

   "LLDS Energy Regulator"
   {:interactions {:prevent [{:type #{:trash-hardware}
                              :req (req true)}]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent a hardware from being trashed"
                 :effect (effect (trash-prevent :hardware 1))}
                {:label "Prevent a hardware from being trashed"
                 :msg "prevent a hardware from being trashed"
                 :cost [:trash]
                 :effect (effect (trash-prevent :hardware 1))}]}

   "Lustig"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)
                                 {:label "Bypass Sentry being encountered"
                                  :req (req (has-subtype? current-ice "Sentry"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :cost [:trash]}]})

   "Magnum Opus"
   {:abilities [{:cost [:click 1]
                 :effect (effect (gain-credits 2))
                 :msg "gain 2 [Credits]"}]}

   "Mammon"
   (auto-icebreaker ["All"]
                    {:flags {:runner-phase-12 (req (pos? (:credit runner)))}
                     :abilities [{:label "X [Credits]: Place X power counters"
                                  :prompt "How many power counters to place on Mammon?" :once :per-turn
                                  :choices {:number (req (:credit runner))}
                                  :req (req (:runner-phase-12 @state))
                                  :effect (effect (lose-credits target)
                                                  (add-counter card :power target))
                                  :msg (msg "place " target " power counters on it")}
                                 {:counter-cost [:power 1]
                                  :label "Hosted power counter: Break ICE subroutine"
                                  :msg "break 1 ICE subroutine"}
                                 (strength-pump 2 2)]
                     :events {:runner-turn-ends {:effect (effect (update! (assoc-in card [:counter :power] 0)))}}})

   "Mass-Driver"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "Prevention of subroutine resolution on next ICE is manual"
                     :abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Maven"
   {:abilities [(break-sub 2 1 "ICE")]
    :events (let [maven {:silent (req true)
                         :req (req (program? target))
                         :effect (effect (update-breaker-strength card))}]
              {:runner-install maven :trash maven :card-moved maven})
    :strength-bonus (req (count (filter program? (all-active-installed state :runner))))}

   "Medium"
   {:events
    {:successful-run {:req (req (= target :rd))
                      :effect (effect (add-counter card :virus 1))}
     :pre-access {:async true
                  :req (req (= target :rd))
                  :effect (effect (continue-ability
                                    {:req (req (< 1 (get-virus-counters state card)))
                                     :prompt "Choose how many additional R&D accesses to make with Medium"
                                     :choices {:number (req (dec (get-virus-counters state card)))
                                               :default (req (dec (get-virus-counters state card)))}
                                     :msg (msg "access " target " additional cards from R&D")
                                     :effect (effect (access-bonus :rd (max 0 target)))}
                                    card nil))}}}

   "Mimic"
   {:abilities [(break-sub 1 1 "Sentry")]}

   "Misdirection"
   {:abilities [{:cost [:click 2]
                 :prompt "How many [Credits] to spend to remove that number of tags?"
                 :choices {:number (req (min (:credit runner) (get-in runner [:tag :base])))}
                 :msg (msg "spend " target " [Credits] and remove " target " tags")
                 :effect (effect (lose-credits target)
                                 (lose-tags target))}]}

   "MKUltra"
   (conspiracy "MKUltra" "Sentry"
               [{:cost [:credit 3]
                 :effect (effect (pump card 2)) :pump 2
                 :msg "add 2 strength and break up to 2 subroutines"}])

   "Mongoose"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Usage restriction is not implemented"
                     :abilities [(break-sub 1 2 "Sentry")
                                 (strength-pump 2 2)]})

   "Morning Star"
   {:abilities [(break-sub 1 0 "Barrier")]}

   "Multithreader"
   {:recurring 2
    :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                                (program? target)))
                                 :type :recurring}}}

   "Musaazi"
   (khumalo-breaker "sentry")

   "Na'Not'K"
   (auto-icebreaker ["Sentry"]
                    {:effect (req (add-watch state (keyword (str "nanotk" (:cid card)))
                                             (fn [k ref old new]
                                               (let [server (first (get-in @state [:run :server]))]
                                                 (when (or
                                                         ; run initiated or ended
                                                         (not= (get-in old [:run])
                                                               (get-in new [:run]))
                                                         ; server configuration changed (redirected or newly installed ICE)
                                                         (not= (get-in old [:corp :servers server :ices])
                                                               (get-in new [:corp :servers server :ices])))
                                                   (update-breaker-strength ref side card))))))
                     :strength-bonus (req (if-let [numice (count run-ices)] numice 0))
                     :leave-play (req (remove-watch state (keyword (str "nanotk" (:cid card)))))
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 2)]})

   "Nerve Agent"
   {:events
    {:successful-run {:req (req (= target :hq))
                      :effect (effect (add-counter card :virus 1))}
     :pre-access {:async true
                  :req (req (= target :hq))
                  :effect (effect (continue-ability
                                    {:req (req (< 1 (get-virus-counters state card)))
                                     :prompt "Choose how many additional HQ accesses to make with Nerve Agent"
                                     :choices {:number (req (dec (get-virus-counters state card)))
                                               :default (req (dec (get-virus-counters state card)))}
                                     :msg (msg "access " target " additional cards from HQ")
                                     :effect (effect (access-bonus :hq (max 0 target)))}
                                    card nil))}}}

   "Net Shield"
   {:interactions {:prevent [{:type #{:net}
                              :req (req true)}]}
    :abilities [{:cost [:credit 1] :once :per-turn :msg "prevent the first net damage this turn"
                 :effect (effect (damage-prevent :net 1))}]}

   "Nfr"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Place 1 power counter on Nfr"
                 :msg "place 1 power counter on it"
                 :ability-type :manual-state
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}
                (break-sub 1 1 "Barrier")]
    :strength-bonus (req (get-counters card :power))}

   "Ninja"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)]})

   "Nyashia"
   {:data {:counter {:power 3}}
    :events {:pre-access {:async true
                          :req (req (and (pos? (get-counters card :power))
                                         (= target :rd)))
                          :effect (effect (show-wait-prompt :corp "Runner to use Nyashia")
                                          (continue-ability
                                            {:optional
                                             {:prompt "Spend a power counter on Nyashia to access 1 additional card?"
                                              :autoresolve (get-autoresolve :auto-nyashia)
                                              :yes-ability {:msg "access 1 additional card from R&D"
                                                            :effect (effect (access-bonus :rd 1)
                                                                            (add-counter card :power -1)
                                                                            (clear-wait-prompt :corp))}
                                              :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                                            card nil))}}
    :abilities [(set-autoresolve :auto-nyashia "Nyashia")]}

   "Omega"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :req (req (= 1 (:position run)))
                                  :msg "break 1 subroutine on the innermost ICE protecting this server"}
                                 (strength-pump 1 1)]})

   "Origami"
   {:effect (effect (gain :hand-size
                          {:mod (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                         (all-active-installed state :runner)))))}))
    :leave-play (effect (lose :hand-size
                              {:mod (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                             (all-active-installed state :runner)))))}))}

   "Overmind"
   (auto-icebreaker ["All"]
                    {:effect (effect (add-counter card :power (available-mu state)))
                     :abilities [{:counter-cost [:power 1]
                                  :msg "break 1 subroutine"}
                                 (strength-pump 1 1)]})

   "Paintbrush"
   {:abilities [{:cost [:click 1]
                 :choices {:req #(and (installed? %) (ice? %) (rezzed? %))}
                 :effect (req (let [ice target
                                    stypes (:subtype ice)]
                                (resolve-ability
                                  state :runner
                                  {:prompt (msg "Choose a subtype")
                                   :choices ["Sentry" "Code Gate" "Barrier"]
                                   :msg (msg "spend [Click] and make " (card-str state ice) " gain " (.toLowerCase target)
                                             " until the end of the next run this turn")
                                   :effect (effect (update! (assoc ice :subtype (combine-subtypes true stypes target)))
                                                   (update-ice-strength (get-card state ice))
                                                   (register-events {:run-ends
                                                                     {:effect (effect (update! (assoc ice :subtype stypes))
                                                                                      (unregister-events card)
                                                                                      (update-ice-strength (get-card state ice)))}} card))}
                                  card nil)))}]
    :events {:run-ends nil}}

   "Panchatantra"
   {:abilities [{:msg "add a custom subtype to currently encountered ICE"
                 :once :per-turn}]}

   "Paperclip"
   (conspiracy "Paperclip" "Barrier"
               [{:label "X [Credits]: +X strength, break X subroutines"
                 :choices {:number (req (:credit runner))
                           :default (req (if (:current-strength current-ice)
                                           (max (- (:current-strength current-ice)
                                                   (:current-strength card))
                                                1)
                                           1))}
                 :prompt "How many credits?"
                 :effect (effect (lose-credits target)
                                 (pump card target))
                 :msg (msg "spend " target " [Credits], increase strength by " target ", and break "
                           (quantify target "Barrier subroutine"))}])

   "Parasite"
   {:hosting {:req #(and (ice? %) (can-host? %) (rezzed? %))}
    :effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (update-ice-strength state side h)
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))
    :events {:runner-turn-begins
             {:effect (req (add-counter state side card :virus 1))}
             :counter-added
             {:req (req (or (= (:title target) "Hivemind") (same-card? target card)))
              :effect (effect (update-ice-strength (:host card)))}
             :pre-ice-strength
             {:req (req (same-card? target (:host card)))
              :effect (effect (ice-strength-bonus (- (get-virus-counters state card)) target))}
             :ice-strength-changed
             {:req (req (and (same-card? target (:host card))
                             (not (card-flag? (:host card) :untrashable-while-rezzed true))
                             (<= (:current-strength target) 0)))
              :effect (req (unregister-events state side card)
                           (when (get-in card [:special :installing])
                             (update! state side (update-in card [:special] dissoc :installing))
                             (trigger-event state side :runner-install card))
                           (trash state side target)
                           (continue state side nil))
              :msg (msg "trash " (:title target))}}}

   "Paricia"
   {:recurring 2
    :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                                (asset? target)))
                                 :type :recurring}}}

   "Passport"
   (central-breaker "Code Gate"
                    (break-sub 1 1 "Code Gate")
                    (strength-pump 2 2))

   "Pawn"
   {:implementation "All abilities are manual"
    :abilities [{:label "Host Pawn on the outermost ICE of a central server"
                 :cost [:click 1]
                 :prompt "Host Pawn on the outermost ICE of a central server"
                 :choices {:req #(and (ice? %)
                                      (can-host? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Advance to next ICE"
                 :prompt "Choose the next innermost ICE to host Pawn on it"
                 :choices {:req #(and (ice? %)
                                      (can-host? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Trash Pawn and install a Caïssa from your Grip or Heap, ignoring all costs"
                 :effect (req (let [this-pawn (:cid card)]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a Caïssa program to install from your Grip or Heap"
                                   :show-discard true
                                   :choices {:req #(and (has-subtype? % "Caïssa")
                                                        (not= (:cid %) this-pawn)
                                                        (#{[:hand] [:discard]} (:zone %)))}
                                   :msg (msg "install " (:title target))
                                   :effect (effect (runner-install target {:ignore-all-cost true}))}
                                  card nil)
                                (trash state side card)))}]}

   "Peacock"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 2 3)]})

   "Peregrine"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 3 3)
                                 {:label "Derez a Code Gate"
                                  :cost [:credit 2 :return-to-hand]
                                  :req (req (and (rezzed? current-ice)
                                                 (has-subtype? current-ice "Code Gate")))
                                  :msg (msg "derez " (:title current-ice))
                                  :effect (effect (derez current-ice))}]})

   "Persephone"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Requires runner to input the number of subroutines allowed to resolve"
                     :abilities [(break-sub 2 1 "Sentry")
                                 (strength-pump 1 1)]
                     :events {:pass-ice {:req (req (and (has-subtype? target "Sentry") (rezzed? target)) (pos? (count (:deck runner))))
                                         :optional {:prompt (msg "Use Persephone's ability??")
                                                    :yes-ability {:prompt "How many subroutines resolved on the passed ICE?"
                                                                  :async true
                                                                  :choices {:number (req 10)}
                                                                  :msg (msg (if (pos? target)
                                                                              (str "trash " (:title (first (:deck runner))) " from their Stack and trash " target " cards from R&D")
                                                                              (str "trash " (:title (first (:deck runner))) " from their Stack and nothing from R&D")))
                                                                  :effect (effect (mill :runner)
                                                                                  (mill :runner :corp target))}}}}})

   "Pelangi"
   {:data {:counter {:virus 2}}
    :events {:pass-ice nil
             :run-ends nil}
    :abilities [{:once :per-turn
                 :req (req (and current-ice
                                (rezzed? current-ice)))
                 :counter-cost [:virus 1]
                 :label "Make currently encountered ice gain a subtype"
                 :prompt "Choose an ICE subtype"
                 :choices (req (->> (server-cards)
                                    (reduce (fn [acc card]
                                              (if (ice? card)
                                                (apply conj acc (split (:subtype card) #" - "))
                                                acc))
                                            #{})
                                    sort))
                 :msg (msg "make " (card-str state current-ice) " gain " target)
                 :effect (req (let [ice current-ice
                                    chosen-type target
                                    stypes (:subtype ice)
                                    remove-subtype
                                    {:effect (effect (update! (assoc (get-card state ice) :subtype stypes))
                                                     (system-say (str (card-str state ice) " loses " chosen-type))
                                                     (unregister-events card)
                                                     (register-events (:events (card-def card)) card))}]
                                (update! state side (assoc ice :subtype (combine-subtypes false stypes chosen-type)))
                                (update-ice-strength state side (get-card state ice))
                                (register-events state side {:pass-ice remove-subtype
                                                             :run-ends remove-subtype} card)))}]}

   "Pheromones"
   {:recurring (req (when (< (get-counters card :recurring) (get-counters card :virus))
                      (set-prop state side card :rec-counter (get-counters card :virus))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :hq))
                              :effect (effect (add-counter card :virus 1))}}
    :interactions {:pay-credits {:req (req (= :hq (get-in @state [:run :server 0])))
                                 :type :recurring}}}

   "Pipeline"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1 :all-run)]})

   "Plague"
   {:prompt "Choose a server for Plague"
    :choices (req servers)
    :msg (msg "target " target)
    :req (req (not (get-in card [:special :server-target])))
    :effect (effect (update! (assoc-in card [:special :server-target] target)))
    :events {:successful-run
             {:req (req (= (zone->name (get-in @state [:run :server]))
                           (get-in (get-card state card) [:special :server-target])))
              :msg "gain 2 virus counters"
              :effect (effect (add-counter :runner card :virus 2))}}}

   "Progenitor"
   {:abilities [{:label "Install a virus program on Progenitor"
                 :req (req (empty? (:hosted card)))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a Virus program to install on Progenitor"
                                    :choices {:req #(and (program? %)
                                                         (has-subtype? % "Virus")
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc (get-card state card)
                                                                    :hosted-programs
                                                                    (cons (:cid target) (:hosted-programs card)))))}
                                   card nil))}
                {:label "Host an installed virus on Progenitor"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed virus program to host on Progenitor"
                 :choices {:req #(and (program? %)
                                      (has-subtype? % "Virus")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (free-mu (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:pre-purge {:effect (req (when-let [c (first (:hosted card))]
                                        (update! state side (assoc-in card [:special :numpurged] (get-counters c :virus)))))}
             :purge {:req (req (pos? (get-in card [:special :numpurged] 0)))
                     :effect (req (when-let [c (first (:hosted card))]
                                    (add-counter state side c :virus 1)))}
             :card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (use-mu (:memoryunits target)))}}}

   "Puffer"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Memory use must be manually tracked by the Runner"
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1)
                                 {:cost [:click 1] :msg "place one power counter"
                                  :label "Place 1 power counter"
                                  :effect (effect (add-counter card :power 1)
                                                  (update-breaker-strength card))}
                                 {:cost [:click 1] :msg "remove one power counter"
                                  :label "Remove 1 power counter"
                                  :effect (effect (add-counter card :power -1)
                                                  (update-breaker-strength card))}]
                     :strength-bonus (req (get-counters card :power))})

   "Reaver"
   {:events {:runner-trash {:req (req (and (first-installed-trash? state side)
                                           (installed? target)))
                            :async true
                            :effect (effect (draw :runner eid 1 nil))
                            :msg "draw 1 card"}}}

   "Refractor"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 3)]})

   "Rezeki"
   {:events {:runner-turn-begins {:msg "gain 1 [Credits]"
                                  :effect (effect (gain-credits 1))}}}

   "RNG Key"
   {:events {:pre-access-card {:req (req (get-in card [:special :rng-guess]))
                               :async true
                               :msg (msg "reveal " (:title target))
                               :effect (req (if-let [guess (get-in card [:special :rng-guess])]
                                              (if (installed? target)
                                                ;; Do not trigger on installed cards (can't "reveal" an installed card per UFAQ)
                                                (do (toast state :runner "Installed cards cannot be revealed, so RNG Key does not pay out." "info")
                                                    (effect-completed state side eid))
                                                (if (or (= guess (:cost target))
                                                        (= guess (:advancementcost target)))
                                                  (continue-ability state side
                                                                    {:prompt "Choose RNG Key award"
                                                                     :choices ["Gain 3 [Credits]" "Draw 2 cards"]
                                                                     :async true
                                                                     :effect (req (if (= target "Draw 2 cards")
                                                                                    (do (system-msg state :runner "uses RNG Key to draw 2 cards")
                                                                                        (draw state :runner eid 2 nil))
                                                                                    (do (system-msg state :runner "uses RNG Key to gain 3 [Credits]")
                                                                                        (gain-credits state :runner 3)
                                                                                        (effect-completed state side eid))))}
                                                                    card nil)
                                                  (effect-completed state side eid)))
                                              (effect-completed state side eid)))}
             :post-access-card {:effect (effect (update! (assoc-in card [:special :rng-guess] nil)))}
             :successful-run {:req (req (and (#{:hq :rd} target)
                                             (first-event? state :runner :successful-run #{[:hq] [:rd]})))
                              :optional {:prompt "Fire RNG Key?"
                                         :autoresolve (get-autoresolve :auto-fire)
                                         :yes-ability {:prompt "Guess a number"
                                                       :choices {:number (req 20)}
                                                       :msg (msg "guess " target)
                                                       :effect (effect (update! (assoc-in card [:special :rng-guess] target)))}}}}
    :abilities [(set-autoresolve :auto-fire "RNG Key")]}

   "Rook"
   {:abilities [{:cost [:click 1]
                 :effect (req (let [r (get-card state card)
                                    hosted? (ice? (:host r))
                                    icepos (ice-index state (get-card state (:host r)))]
                                (resolve-ability
                                  state side
                                  {:prompt (if hosted?
                                             (msg "Host Rook on a piece of ICE protecting this server or at position "
                                                  icepos " of a different server")
                                             (msg "Host Rook on a piece of ICE protecting any server"))
                                   :choices {:req #(if hosted?
                                                     (and (or (= (:zone %) (:zone (:host r)))
                                                              (= (ice-index state %) icepos))
                                                          (= (last (:zone %)) :ices)
                                                          (ice? %)
                                                          (can-host? %)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %)))
                                                     (and (ice? %)
                                                          (can-host? %)
                                                          (= (last (:zone %)) :ices)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %))))}
                                   :msg (msg "host it on " (card-str state target))
                                   :effect (effect (host target card))} card nil)))}]
    :events {:pre-rez-cost {:req (req (= (:zone (:host card)) (:zone target)))
                            :effect (effect (rez-cost-bonus 2))}}}

   "Sadyojata"
   (deva "Sadyojata")

   "Sage"
   (ancient-greek-breaker "sage" [{:cost [:credit 2]
                                   :req (req (or (has-subtype? current-ice "Barrier")
                                                 (has-subtype? current-ice "Code Gate")))
                                   :msg "break 1 Code Gate or Barrier subroutine"}])

   "Sahasrara"
   {:recurring 2
    :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                                (program? target)))
                                 :type :recurring}}}

   "Saker"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 2)
                                 {:label "Derez a Barrier"
                                  :cost [:credit 2 :return-to-hand]
                                  :req (req (and (rezzed? current-ice)
                                                 (has-subtype? current-ice "Barrier")))
                                  :msg (msg "derez " (:title current-ice))
                                  :effect (effect (derez current-ice))}]})

   "Savant"
   (ancient-greek-breaker "savant" [{:cost [:credit 2] :req (req (has-subtype? current-ice "Sentry"))
                                     :msg "break 1 Sentry subroutine"}
                                    {:cost [:credit 2] :req (req (has-subtype? current-ice "Code Gate"))
                                     :msg "break 2 Code Gate subroutines"}])

   "Savoir-faire"
   {:abilities [{:cost [:credit 2]
                 :once :per-turn
                 :req (req (not (install-locked? state side)))
                 :msg (msg "install " (:title target))
                 :prompt "Choose a program to install from your grip"
                 :choices {:req #(and (program? %)
                                      (in-hand? %))}
                 :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]}

   "Scheherazade"
   {:abilities [{:label "Install and host a program from Grip"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program to install on Scheherazade from your grip"
                                    :choices {:req #(and (program? %)
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target) " and gain 1 [Credits]")
                                    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:host-card card}) (gain-credits 1))}
                                   card nil))}
                {:label "Host an installed program"
                 :prompt "Choose a program to host on Scheherazade" :priority 2
                 :choices {:req #(and (program? %)
                                      (installed? %))}
                 :msg (msg "host " (:title target) " and gain 1 [Credits]")
                 :effect (req (when (host state side card target)
                                (gain-credits state side 1)))}]}

   "Self-modifying Code"
   {:abilities [{:req (req (not (install-locked? state side)))
                 :cost [:trash]
                 :effect (effect (continue-ability
                                   {:prompt "Choose a program to install"
                                    :msg (req (if (not= target "No install")
                                                (str "install " (:title target))
                                                (str "shuffle their Stack")))
                                    :priority true
                                    :choices (req (cancellable
                                                    (conj (vec (sort-by :title (filter program?
                                                                                       (:deck runner))))
                                                          "No install")))
                                    :cost [:credit 2]
                                    :effect (req (trigger-event state side :searched-stack nil)
                                                 (shuffle! state side :deck)
                                                 (when (not= target "No install")
                                                   (runner-install state side (make-eid state {:source card :source-type :runner-install}) target nil)))}
                                   card nil))}]}

   "Sharpshooter"
   (auto-icebreaker ["Destroyer"]
                    {:abilities [{:label "Break any number of Destroyer subroutines"
                                  :msg "break any number of Destroyer subroutines"
                                  :cost [:trash]}
                                 (strength-pump 1 2)]})

   "Shiv"
   (break-and-enter "Sentry")

   "Sneakdoor Beta"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on Archives"
                 :makes-run true
                 :effect (effect (make-run :archives
                                           {:req (req (= target :archives))
                                            :successful-run
                                            {:silent (req true)
                                             :effect (req (swap! state assoc-in [:run :server] [:hq])
                                                          ; remove the :req from the run-effect, so that other cards that replace
                                                          ; access don't use Sneakdoor's req. (Security Testing, Ash 2X).
                                                          (swap! state dissoc-in [:run :run-effect :req])
                                                          (trigger-event state :corp :no-action)
                                                          (system-msg state side
                                                                      (str "uses Sneakdoor Beta to make a successful run on HQ")))}}
                                           card))}]}

   "Snitch"
   {:abilities [{:once :per-run :req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :async true
                 :effect (req (wait-for (expose state side current-ice)
                                        (continue-ability
                                          state side
                                          {:optional {:prompt "Jack out?"
                                                      :yes-ability {:msg "jack out"
                                                                    :effect (effect (jack-out nil))}
                                                      :no-ability {:msg "continue the run"}}}
                                          card nil)))}]}

   "Snowball"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 Barrier subroutine"
                                  :effect (effect (pump card 1 :all-run))}
                                 (strength-pump 1 1)]})

   "Spike"
   (break-and-enter "Barrier")

   "Stargate"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :msg "make a run on R&D"
                 :makes-run true
                 :effect (effect (make-run
                                   :rd
                                   {:req (req (= target :rd))
                                    :replace-access
                                    {:msg (msg "reveal "
                                               (->>
                                                 (:deck corp)
                                                 (take 3)
                                                 (map :title)
                                                 (join ", ")))
                                     :mandatory true
                                     :effect
                                     (effect
                                       (reveal (take 3 (:deck corp)))
                                       (continue-ability
                                         {:prompt "Choose a card to trash"
                                          :msg (msg "trash " (:title target))
                                          :not-distinct true
                                          :choices (req (take 3 (:deck corp)))
                                          :effect (effect (trash :runner (assoc target :seen true)))}
                                         card nil))}}
                                   card))}]}

   "Study Guide"
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2] :msg "place 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (get-counters card :power))}

   "Sūnya"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Place 1 power counter on Sūnya"
                 :ability-type :manual-state
                 :effect (effect (add-counter card :power 1)
                                 (system-msg (str "places 1 power counter on Sūnya"))
                                 (update-breaker-strength card))}
                (break-sub 2 1 "Sentry")]
    :strength-bonus (req (get-counters card :power))}

   "Surfer"
   (letfn [(surf [state cice]
             {:prompt (msg "Choose an ICE before or after " (:title cice))
              :choices {:req #(and (ice? %)
                                   (= (:zone %) (:zone cice))
                                   (= 1 (abs (- (ice-index state %)
                                                (ice-index state cice)))))}
              :effect (req (let [tgtndx (ice-index state target)
                                 cidx (ice-index state cice)]
                             (system-msg state :runner (str "uses Surfer to swap "
                                                            (card-str state cice)
                                                            " and "
                                                            (card-str state (nth run-ices tgtndx))))
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % tgtndx cice))
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % cidx target))
                             (swap! state update-in [:run] #(assoc % :position (inc tgtndx)))
                             (update-all-ice state side)
                             (trigger-event state side :approach-ice current-ice)))})]
     {:abilities [{:cost [:credit 2]
                   :msg "swap a piece of Barrier ICE"
                   :req (req (and (:run @state)
                                  (rezzed? current-ice)
                                  (has-subtype? current-ice "Barrier")))
                   :label "Swap the Barrier ICE currently being encountered with a piece of ICE directly before or after it"
                   :effect (effect (resolve-ability (surf state current-ice) card nil))}]})

   "Switchblade"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 0 "Sentry")
                                 (strength-pump 1 7)]})

   "Takobi"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (system-msg "adds a power counter to Takobi"))}
                {:req (req (and (:run @state)
                                (rezzed? current-ice)
                                (>= (get-counters card :power) 2)))
                 :counter-cost [:power 2]
                 :label "Increase non-AI icebreaker strength by +3 until end of encounter"
                 :prompt "Choose an installed non-AI icebreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "add +3 strength to " (:title target) " for remainder of encounter")
                 :effect (effect (pump target 3 :encounter))}]}

   "Tapwrm"
   (let [ability {:label "Gain [Credits] (start of turn)"
                  :msg (msg "gain " (quot (:credit corp) 5) " [Credits]")
                  :once :per-turn
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (gain-credits (quot (:credit corp) 5)))}]
     {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
      :flags {:drip-economy true}
      :abilities [ability]
      :events {:runner-turn-begins ability
               :purge {:effect (effect (trash card {:cause :purge}))}}})

   "Torch"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Tracker"
   (let [ability {:prompt "Choose a server for Tracker" :choices (req servers)
                  :msg (msg "target " target)
                  :req (req (not (:server-target card)))
                  :effect (effect (update! (assoc card :server-target target)))}]
     {:abilities [{:label "Make a run on targeted server" :cost [:click 1 :credit 2]
                   :req (req (some #(= (:server-target card) %) runnable-servers))
                   :msg (msg "make a run on " (:server-target card) ". Prevent the first subroutine that would resolve from resolving")
                   :effect (effect (make-run (:server-target card) nil card))}]
      :events {:runner-turn-begins ability
               :runner-turn-ends {:effect (effect (update! (dissoc card :server-target)))}}})

   "Trope"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :power 1))}}
    :abilities [{:effect
                 (effect
                   (continue-ability
                     {:cost [:click 1 :remove-from-game]
                      :label "Reshuffle cards from Heap back into Stack"
                      :show-discard true
                      :choices {:max (min (get-counters card :power) (count (:discard runner)))
                                :all true
                                :req #(and (runner? %)
                                           (in-discard? %))}
                      :msg (msg "shuffle " (join ", " (map :title targets))
                                " into their Stack")
                      :effect (req (doseq [c targets] (move state side c :deck))
                                   (shuffle! state side :deck))}
                     card nil))}]}

   "Trypano"
   (let [trash-if-5 (req (when-let [h (get-card state (:host card))]
                           (if (and (>= (get-virus-counters state card) 5)
                                    (not (and (card-flag? h :untrashable-while-rezzed true)
                                              (rezzed? h))))
                             (do (system-msg state :runner (str "uses Trypano to trash " (card-str state h)))
                                 (unregister-events state side card)
                                 (trash state :runner eid h nil))
                             (effect-completed state side eid))))]
     {:hosting {:req #(and (ice? %) (can-host? %))}
      :effect trash-if-5
      :abilities [(set-autoresolve :auto-accept "add virus counter to Trypano")]
      :events {:runner-turn-begins
               {:optional {:prompt (msg "Place a virus counter on Trypano?")
                           :autoresolve (get-autoresolve :auto-accept)
                           :yes-ability {:effect (req (system-msg state :runner "places a virus counter on Trypano")
                                                      (add-counter state side card :virus 1))}}}
               :counter-added {:async true
                               :effect trash-if-5}
               :card-moved {:effect trash-if-5
                            :async true}
               :runner-install {:effect trash-if-5
                                :async true}}})

   "Tycoon"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 2 "Barrier" (effect (update! (assoc-in card [:special :tycoon-used] true))))
                                 (strength-pump 2 3)]
                     :events (let [give-credits {:req (req (get-in card [:special :tycoon-used]))
                                                 :msg "give the Corp 2 [Credits]"
                                                 :effect (effect (update! (dissoc-in card [:special :tycoon-used]))
                                                                 (gain-credits :corp 2))}]
                               {:pass-ice give-credits
                                :run-ends give-credits})})

   "Upya"
   {:implementation "Power counters added automatically"
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1]
                 :counter-cost [:power 3]
                 :once :per-turn
                 :msg "gain [Click][Click]"
                 :effect (effect (gain :click 2))}]}

   "Utae"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:label "X [Credits]: Break X Code Gate subroutines"
                                  :once :per-run
                                  :req (req (pos? (:credit runner)))
                                  :prompt "How many credits?"
                                  :choices :credit
                                  :msg (msg "spend " target " [Credits] to break " (quantify target "Code Gate subroutine"))}
                                 {:label "Break 1 Code Gate subroutine (Virtual restriction)"
                                  :req (req (<= 3 (count (filter #(has-subtype? % "Virtual")
                                                                 (all-active-installed state :runner)))))
                                  :cost [:credit 1]
                                  :msg "break 1 Code Gate subroutine"}
                                 (strength-pump 1 1)]})

   "Vamadeva"
   (deva "Vamadeva")

   "Wari"
   (letfn [(prompt-for-subtype []
             {:prompt "Choose a subtype"
              :choices ["Barrier" "Code Gate" "Sentry"]
              :async true
              :effect (req (wait-for (trash state side card {:unpreventable true})
                                     (continue-ability state side
                                                       (expose-and-maybe-bounce target)
                                                       card nil)))})
           (expose-and-maybe-bounce [chosen-subtype]
             {:choices {:req #(and (ice? %) (not (rezzed? %)))}
              :async true
              :msg (str "name " chosen-subtype)
              :effect (req (wait-for (expose state side target)
                                     (do (if (and async-result
                                                  (has-subtype? target chosen-subtype))
                                           (do (move state :corp target :hand)
                                               (system-msg state :runner
                                                           (str "add " (:title target) " to HQ"))))
                                         (effect-completed state side eid))))})]
     {:events {:successful-run
               {:interactive (req true)
                :async true
                :req (req (and (= target :hq)
                               (first-successful-run-on-server? state :hq)
                               (some #(and (ice? %) (not (rezzed? %)))
                                     (all-installed state :corp))))
                :effect (effect (continue-ability
                                  {:prompt "Use Wari?"
                                   :choices ["Yes" "No"]
                                   :async true
                                   :effect (req (if (= target "Yes")
                                                  (continue-ability state side
                                                                    (prompt-for-subtype)
                                                                    card nil)
                                                  (effect-completed state side eid)))}
                                  card nil))}}})

   "Wyrm"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 3]
                                  :msg "break 1 subroutine on ICE with 0 or less strength"}
                                 {:cost [:credit 1]
                                  :label "Give -1 strength to current ICE"
                                  :req (req (rezzed? current-ice))
                                  :msg (msg "give -1 strength to " (:title current-ice))
                                  :effect (req (update! state side (update-in card [:wyrm-count] (fnil #(+ % 1) 0)))
                                               (update-ice-strength state side current-ice))}
                                 (strength-pump 1 1)]
                     :events (let [auto-pump (fn [state side eid card targets]
                                               ((:effect breaker-auto-pump) state side eid card targets))
                                   wy {:effect (effect (update! (dissoc card :wyrm-count))
                                                       (auto-pump eid (get-card state card) targets))}]
                               {:pre-ice-strength {:req (req (and (same-card? target current-ice)
                                                                  (:wyrm-count card)))
                                                   :effect (req (let [c (:wyrm-count (get-card state card))]
                                                                  (ice-strength-bonus state side (- c) target)
                                                                  (auto-pump state side eid card targets)))}
                                :pass-ice wy
                                :run-ends wy})})

   "Yog.0"
   {:abilities [(break-sub 0 1 "Code Gate")]}

   "Yusuf"
   (khumalo-breaker "barrier")

   "ZU.13 Key Master"
   (cloud-icebreaker
     (auto-icebreaker ["Code Gate"]
                      {:abilities [(break-sub 1 1 "Code Gate")
                                   (strength-pump 1 1)]}))})
