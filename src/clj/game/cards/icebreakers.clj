(ns game.cards.icebreakers
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [jinteki.utils :refer [str->int other-side]]
            [jinteki.cards :refer [all-cards]]))

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
              strdif (when (and (or (:current-strength current-ice)
                                    (:strength current-ice))
                                (or (:current-strength card)
                                    (:strength card)))
                       (max 0 (- (or (:current-strength current-ice)
                                     (:strength current-ice))
                                 (or (:current-strength card)
                                     (:strength card)))))
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

; Takes a vector of ice subtypes that can be broken (or ["All"] for
; AI breakers) and a card definition, and returns a new card definition that
; hooks up breaker-auto-pump to the necessary events.
; IMPORTANT: Events on cdef take precedence, and should call
; (:effect breaker-auto-pump) themselves.
(defn auto-icebreaker [breaks cdef]
  (assoc cdef :data (merge {:breaks breaks} (:data cdef))
              :events (merge {:run breaker-auto-pump
                              :pass-ice breaker-auto-pump
                              :run-ends breaker-auto-pump
                              :ice-strength-changed breaker-auto-pump
                              :ice-subtype-changed breaker-auto-pump
                              :breaker-strength-changed breaker-auto-pump
                              :approach-ice breaker-auto-pump}
                             (:events cdef))))

(defn wrestling-breaker
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
                                      (effect (update! (assoc ice
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
  (assoc cdef :effect (req (let [link (get-in @state [:runner :link] 0)]
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

(defn strength-pump
  "Creates a strength pump ability.
  Cost can be a credit amount or a list of costs e.g. [:credit 2]."
  ([cost strength] (strength-pump cost strength :encounter))
  ([cost strength duration]
   {:msg (str "add " strength " strength" (cond
                                            (= duration :all-run)
                                            " for the remainder of the run"
                                            (= duration :all-turn)
                                            " for the remainder of the turn"))
    :cost [:credit cost]
    :effect (effect (pump card strength duration))
    :pump strength}))

(defn break-sub
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

;; Breaker sets
(defn cerberus
  "Breaker from the dog set"
  [ice-type]
  (auto-icebreaker [ice-type]
                   {:data {:counter {:power 4}}
                    :abilities [{:counter-cost [:power 1]
                                 :msg (str "break up to 2 " (lower-case ice-type) " subroutines")}
                                (strength-pump 1 1)]}))

(defn break-and-enter
  "Breakers from the Break and Entry set"
  [ice-type]
  (cloud-icebreaker {:abilities [{:label (str "[Trash]: Break up to 3 " (lower-case ice-type) "subroutines")
                                  :msg (str "break up to 3 " (lower-case ice-type) " subroutines")
                                  :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:silent (req true)
                                           :req (req (has-subtype? target "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:runner-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                          (all-active-installed state :runner))))}))

(defn global-sec-breaker
  "GlobalSec breakers for Sunny"
  [ice-type]
  (cloud-icebreaker (auto-icebreaker [ice-type] {:abilities [(break-sub 2 0 (lower-case ice-type))
                                                         (strength-pump 2 3)]})))

(defn deva
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
                                                      devaindex (first (keep-indexed #(when (= (:cid %2) (:cid card)) %1) devavec))
                                                      newdeva (assoc target :zone (:zone card) :installed true)
                                                      newvec (apply conj (subvec devavec 0 devaindex) newdeva (subvec devavec devaindex))]
                                                  (lose state :runner :memory (:memoryunits card))
                                                  (swap! state assoc-in [:runner :rig :program] newvec)
                                                  (swap! state update-in [:runner :hand] (fn [coll] (remove-once #(= (:cid %) (:cid target)) coll)))
                                                  (card-init state side newdeva {:resolve-effect false
                                                                                 :init-data true})))
                                              (move state side card :hand))}]}))

(defn conspiracy
  "Install-from-heap breakers"
  [title ice-type abilities]
  (let [install-prompt {:req (req (and (= (:zone card) [:discard])
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

(defn central-breaker
  "'Cannot be used on a remote server' breakers"
  [ice-type break pump]
  (let [central-req (req (or (not (:central-breaker card)) (#{:hq :rd :archives} (first (:server run)))))]
    (auto-icebreaker [ice-type]
                     {:abilities [(assoc break :req central-req)
                                  (assoc pump :req central-req)]
                      :effect (effect (update! (assoc card :central-breaker true)))})))

(defn ancient-greek-breaker
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

(defn khumalo-breaker
  "Spends virus counters from any card to pump/break, gains virus counters for successful runs."
  [ice-type]
  {:events {:successful-run {:silent (req true)
                             :effect (effect (system-msg "adds 1 virus counter to " (:title card))
                                             (add-counter card :virus 1))}}
   :abilities [{:label (str  "Break " ice-type " subroutine(s)")
                :effect (req (wait-for (resolve-ability
                                         state side (pick-virus-counters-to-spend) card nil)
                                       (do (if-let [msg (:msg async-result)]
                                             (do (system-msg state :runner
                                                             (str "spends " msg" to break " (:number async-result)
                                                                  " " ice-type " subroutine(s)")))))))}
               {:label "Match strength of currently encountered ice"
                :req (req (and current-ice
                               (> (ice-strength state side current-ice)
                                  (or (:current-strength card) (:strength card)))))
                :effect (req (wait-for (resolve-ability
                                         state side
                                         (pick-virus-counters-to-spend
                                           (- (ice-strength state side current-ice)
                                              (or (:current-strength card) (:strength card))))
                                         card nil)
                                       (if-let [msg (:msg async-result)]
                                         (do (system-msg state :runner (str "spends " msg " to add "
                                                                            (:number async-result) " strength"))
                                             (dotimes [_ (:number async-result)]
                                               (pump state side (get-card state card) 1))))))}
               {:label "Add strength"
                :effect (req (wait-for
                               (resolve-ability
                                 state side (pick-virus-counters-to-spend) card nil)
                               (if-let [msg (:msg async-result)]
                                 (do (system-msg state :runner (str "spends " msg" to add "
                                                                    (:number async-result)
                                                                    " strength"))
                                     (dotimes [_ (:number async-result)]
                                       (pump state side (get-card state card) 1))))))}]})
