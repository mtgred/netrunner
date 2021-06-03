(ns game.cards.programs
  (:require [game.core :refer :all]
            [game.core.cost-fns :refer [all-stealth min-stealth]]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.string :as string]))

(defn- power-counter-break
  "Only break ability uses power counters
  (Cerberus suite: Cerberus \"Lady\" H1, Cerberus \"Rex\" H2, Cerberus \"Cuj.0\" H3)"
  [ice-type]
  (auto-icebreaker {:data {:counter {:power 4}}
                    :abilities [(break-sub [:power 1] 2 ice-type)
                                (strength-pump 1 1)]}))

(defn- swap-with-in-hand
  "Swap with a deva program from your grip
  (Deva suite: Aghora, Sadyojata, Vamadeva)"
  [card-name break-req]
  (auto-icebreaker
    {:abilities [(break-sub 1 1 "All" break-req)
                 (strength-pump 1 1)
                 {:req (req (seq (filter #(has-subtype? % "Deva") (:hand runner))))
                  :label "Swap with a deva program from your Grip"
                  :cost [:credit 2]
                  :prompt (str "Choose a deva program in your Grip to swap with " card-name)
                  :choices {:card #(and (in-hand? %)
                                        (has-subtype? % "Deva"))}
                  :msg (msg "swap in " (:title target) " from their Grip")
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
                            (not (install-locked? state :runner))
                            (not (zone-locked? state :runner :discard))
                            (can-pay? state :runner (assoc eid :source card :source-type :runner-install) card nil [:credit (install-cost state side card)])))
             :effect (effect
                       (continue-ability
                         {:optional
                          {:req (req (and (not-any? #(= title (:title %)) (all-active-installed state :runner))
                                          (not (get-in @state [:run :register (keyword (str "conspiracy-" title)) (:cid current-ice)]))))
                           :player :runner
                           :prompt (str "Install " title "?")
                           :yes-ability {:async true
                                         :effect (effect (runner-install :runner (assoc eid :source card :source-type :runner-install) card nil))}
                           ;; Add a register to note that the player was already asked about installing,
                           ;; to prevent multiple copies from prompting multiple times.
                           :no-ability {:effect (req (swap! state assoc-in [:run :register (keyword (str "conspiracy-" title)) (:cid current-ice)] true))}}}
                         card targets))}]})

(defn- pump-and-break
  "Paid ability for conspiracy breakers
  (Conspiracy suite: Black Orchestra, MKUltra, Paperclip)"
  [cost strength subtype]
  (merge
    (dissoc-req (break-sub cost strength subtype))
    {:label (str "add " strength " strength and "
                 " break up to " strength
                 " " subtype
                 " subroutines")
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
  IF we are encountering a rezzed ice with a subtype we can break."
  {:effect
   (req (let [abs (remove #(or (= (:dynamic %) :auto-pump)
                               (= (:dynamic %) :auto-pump-and-break))
                          (:abilities card))
              current-ice (when-not (get-in @state [:run :ended])
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
                             [:credit x-number]
                             (repeat ability-uses-needed (:cost pump-ability))))]
          (update! state side
                   (assoc card :abilities
                          (if (and (seq total-cost)
                                   (rezzed? current-ice)
                                   (= :encounter-ice (:phase run))
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
  (auto-icebreaker {:abilities abilities
                    :strength-bonus (req (available-mu state))}))

(defn- break-multiple-types
  "Single ability to break multiple types of ice
  (Greek/Philosopher suite: Adept, Sage, Savant)"
  [first-qty first-type second-qty second-type]
  {:cost [:credit 2]
   :req (req (and (rezzed? current-ice)
                  (or (and (has-subtype? current-ice first-type)
                           (<= first-qty (count (remove :broken (:subroutines current-ice)))))
                      (and (has-subtype? current-ice second-type)
                           (<= second-qty (count (remove :broken (:subroutines current-ice))))))))
   :label (str "break "
               (quantify first-qty (str first-type " subroutine")) " or "
               (quantify second-qty (str second-type " subroutine")))
   :effect (effect
             (continue-ability
               (if (has-subtype? current-ice first-type)
                 (break-sub nil first-qty first-type {:all (< 1 first-qty)})
                 (break-sub nil second-qty second-type {:all (< 1 second-qty)}))
               card nil))})

(defn- give-ice-subtype
  "Make currently encountered ice gain chosen type until end of encounter
  (Wrestling suite: Laamb, Engolo)"
  [cost ice-type abilities]
  (auto-icebreaker
    {:abilities abilities
     :events [{:event :encounter-ice
               :req (req (and (not-used-once? state {:once :per-turn} card)
                              (not (has-subtype? (:ice context) ice-type))
                              (can-pay? state :runner eid card nil [:credit 2])))
               :async true
               :effect
               (effect
                 (continue-ability
                   {:eid (assoc eid :source-type :ability)
                    :optional
                    {:prompt (str "Pay " cost
                                  " [Credits] to make " (:title (:ice context))
                                  " gain " ice-type "?")
                     :yes-ability
                     {:cost [:credit cost]
                      :msg (msg "make " (:title current-ice) " gain " ice-type)
                      :effect (effect (register-once {:once :per-turn} card)
                                      (register-floating-effect
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
               :effect (effect (system-msg (str "adds 1 virus counter to " (:title card)))
                               (add-counter card :virus 1))}]
     :abilities [(break-sub [:any-virus-counter 1] 1 ice-type)
                 (strength-pump [:any-virus-counter 1] 1)]}))

(defn- central-only
  "Break ability cannot be used on remote servers
  (Central suite: Alias, Breach, Passport)"
  [break pump]
  (auto-icebreaker
    {:abilities [(break-sub (:break-cost break) (:break break) (:breaks break)
                            {:req (req (and (#{:hq :rd :archives} (target-server run))
                                            (<= (get-strength current-ice) (get-strength card))
                                            (has-subtype? current-ice (:breaks break))))})
                 pump]}))

(defn- return-and-derez
  "Return to grip to derez current ice
  (Bird suite: Golden, Peregrine, Saker)"
  [break pump]
  (let [ice-type (:breaks break)]
    (auto-icebreaker
      {:abilities [break
                   pump
                   {:label (str "Derez " ice-type " being encountered")
                    :cost [:credit 2 :return-to-hand]
                    :req (req (and (= :encounter-ice (:phase run))
                                   (rezzed? current-ice)
                                   (has-subtype? current-ice ice-type)
                                   (all-subs-broken-by-card? current-ice card)))
                    :msg (msg "derez " (:title current-ice))
                    :effect (effect (derez current-ice))}]})))

(defn- trash-to-bypass
  "Trash to bypass current ice
  (Fraud suite: Abagnale, Demara, Lustig)"
  [break pump]
  (let [ice-type (:breaks break)]
    (auto-icebreaker
      {:abilities [break
                   pump
                   {:label (str "Bypass " ice-type " being encountered")
                    :cost [:trash]
                    :req (req (and (= :encounter-ice (:phase run))
                                   (rezzed? current-ice)
                                   (has-subtype? current-ice ice-type)))
                    :msg (msg "bypass " (:title current-ice))
                    :effect (req (bypass-ice state)
                                 (continue state :runner nil))}]})))

(defn- cloud-icebreaker
  "Reduce MU cost to 0 with 2+ link
  (Cloud subtype: Creeper, ZU.13 Key Master, B&E, GlobalSec)"
  [cdef]
  (update cdef :constant-effects conj {:type :used-mu
                                       :req (req (<= 2 (get-link state)))
                                       :value (req (- (:memoryunits card)))}))

(defn- break-and-enter
  "No MU with 2+ link, strength based on installed Icebreakers, trash to break 3 subs
  (Breaking and Entering suite: Crowbar, Shiv, Spike)"
  [ice-type]
  (auto-icebreaker (cloud-icebreaker {:abilities [(break-sub [:trash] 3 ice-type)]
                                      :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                                          (all-active-installed state :runner))))})))

(defn- global-sec-breaker
  "No MU with 2+ link, break any number of subs for 2, pump 2 for 3
  (GlobalSec suite: GS Strike M1, GS Shrike M2, GS Sherman M3)"
  [ice-type]
  (cloud-icebreaker (auto-icebreaker {:abilities [(break-sub 2 0 ice-type)
                                                  (strength-pump 2 3)]})))

;; Card definitions

(defcard "Abagnale"
  (trash-to-bypass (break-sub 1 1 "Code Gate")
                   (strength-pump 2 2)))

(defcard "Adept"
  (mu-based-strength [(break-multiple-types
                        1 "Barrier"
                        1 "Sentry")]))

(defcard "Afterimage"
  (auto-icebreaker {:events [{:event :encounter-ice
                              :interactive (req true)
                              :optional
                              {:req (req (and (has-subtype? (:ice context) "Sentry")
                                              (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil [:credit 2])))
                               :once :per-turn
                               :prompt (msg "Pay 2 [Credits] to bypass " (:title (:ice context)))
                               :yes-ability
                               {:async true
                                :effect
                                (effect
                                  (continue-ability
                                    {:eid (assoc eid :source-type :ability)
                                     :cost [:credit 2]
                                     :cost-req all-stealth
                                     :msg (msg "bypass " (:title (:ice context)))
                                     :effect (req (bypass-ice state))}
                                    card targets))}}}]
                    :abilities [(break-sub 1 2 "Sentry")
                                (strength-pump 1 2 {:cost-req (min-stealth 1)})]}))

(defcard "Aghora"
  (swap-with-in-hand "Aghora"
                     {:req (req (and (<= 5 (:cost current-ice 0))
                                     (<= (get-strength current-ice) (get-strength card))))}))

(defcard "Algernon"
  {:events
   [{:event :runner-turn-begins
     :optional
     {:prompt (msg "Pay 2 [Credits] to gain [Click]")
      :req (req (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil [:credit 2]))
      :player :runner
      :yes-ability {:cost [:credit 2]
                    :msg "gain [Click]"
                    :effect (req (gain state :runner :click 1)
                                 (update! state :runner (assoc-in (get-card state card) [:special :used-algernon] true)))}}}
    {:event :runner-turn-ends
     :async true
     :effect (req (if (get-in card [:special :used-algernon])
                    (do
                      (update! state :runner (dissoc-in card [:special :used-algernon]))
                      (if-not (:successful-run runner-reg)
                        (do
                          (system-msg state :runner "trashes Algernon because a successful run did not occur")
                          (trash state :runner eid card nil))
                        (effect-completed state side eid)))
                    (effect-completed state side eid)))}]})

(defcard "Alias"
  (central-only (break-sub 1 1 "Sentry")
                (strength-pump 2 3)))

(defcard "Alpha"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All" {:req (req (= (:position run) (count run-ices)))})
                                (strength-pump 1 1 :end-of-encounter {:req (req (= (:position run) (count run-ices)))})]}))

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
  {:abilities [{:cost [:click 1]
                :msg "make a run on R&D"
                :makes-run true
                :async true
                :effect (effect (make-run eid :rd card))}]
   :events [(successful-run-replace-access
              {:target-server :rd
               :this-card-run true
               :ability
               {:prompt "Choose a card to shuffle into R&D"
                :choices {:card #(and (not (ice? %))
                                      (not (rezzed? %))
                                      (zero? (get-counters % :advancement)))}
                :msg (msg "shuffle " (card-str state target) " into R&D")
                :effect (effect (move :corp target :deck)
                                (shuffle! :corp :deck))}})]})

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
  {:on-install {:effect (effect
                          (continue-ability {:eid (assoc eid :source-type :ability :source card)
                                             :cost [:x-credits]
                                             :msg (msg "add " (cost-value eid :x-credits) " power counters")
                                             :effect (effect (add-counter card :power (cost-value eid :x-credits)))}
                                            card targets))}
   :abilities [(break-sub 1 1 "All" {:req (req (= (get-strength current-ice) (get-strength card)))})]
   :strength-bonus (req (get-counters card :power))})

(defcard "Au Revoir"
  {:events [{:event :jack-out
             :async true
             :effect (effect (gain-credits eid 1))
             :msg "gain 1 [Credits]"}]})

(defcard "Aumakua"
  (auto-icebreaker {:implementation "Add counters manually for access outside of a run or cards that replace access like Ash"
                    ; We would need a :once :per-access key to make this work for Gang Sign etc.
                    :abilities [(break-sub 1 1)
                                {:label "Add a virus counter"
                                 :effect (effect (system-msg "manually adds a virus counter to Aumakua")
                                                 (add-counter card :virus 1))}]
                    :strength-bonus (req (get-virus-counters state card))
                    :events [{:event :run-ends
                              :req (req (and (not (or (:did-trash target)
                                                      (:did-steal target)))
                                             (:did-access target)))
                              :effect (effect (add-counter card :virus 1))}
                             {:event :expose
                              :effect (effect (add-counter card :virus 1))}]}))

(defcard "Aurora"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Barrier")
                                (strength-pump 2 3)]}))

(defcard "Baba Yaga"
  (let [host-click {:cost [:click 1]
                    :label "Install a non-AI icebreaker on Baba Yaga"
                    :prompt "Choose a non-AI icebreaker in your Grip to install on Baba Yaga"
                    :choices {:card #(and (has-subtype? % "Icebreaker")
                                          (not (has-subtype? % "AI"))
                                          (in-hand? %))}
                    :async true
                    :effect (effect (runner-install eid target {:host-card card}))}
        host-free {:label "Host an installed non-AI icebreaker on Baba Yaga"
                   :prompt "Choose an installed non-AI icebreaker to host on Baba Yaga"
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
             :effect (effect (add-counter card :credit 1)
                             (system-msg "places 1 [Credit] on Bankroll"))}]
   :abilities [{:label "Take all credits from Bankroll"
                :async true
                ;; Cannot trash unless there are counters (so game state changes)
                :req (req (pos? (get-counters card :credit)))
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :cost [:trash]
                :effect (effect (gain-credits eid (get-counters card :credit)))}]})

(defcard "Battering Ram"
  (auto-icebreaker {:abilities [(break-sub 2 2 "Barrier")
                                (strength-pump 1 1 :end-of-run)]}))

(defcard "Berserker"
  (auto-icebreaker {:events [{:event :encounter-ice
                              :req (req (has-subtype? (:ice context) "Barrier"))
                              :msg (msg "gain " (count (:subroutines (:ice context))) " strength")
                              :effect (effect (pump card (count (:subroutines (:ice context)))))}]
                    :abilities [(break-sub 2 2 "Barrier")]}))

(defcard "Bishop"
  {:abilities [{:cost [:click 1]
                :label "move to another ice"
                :effect (req (let [b (get-card state card)
                                   hosted? (ice? (:host b))
                                   remote? (is-remote? (second (get-zone (:host b))))]
                               (continue-ability
                                 state side
                                 {:prompt (msg "Host Bishop on a piece of ice protecting "
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
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))}
                                 card nil)))}]
   :constant-effects [{:type :ice-strength
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
                                 [(pump-and-break [:credit 3] 2 "Code Gate")])]
     (assoc cdef :events (apply conj events (:events cdef)))))

(defcard "BlacKat"
  (auto-icebreaker {:implementation "Stealth credit restriction not enforced"
                    :abilities [(break-sub 1 1 "Barrier")
                                (break-sub 1 3 "Barrier" {:label "break up to 3 Barrier subroutines (using a stealth [Credits])" :cost-req (min-stealth 1)})
                                (strength-pump 2 1)
                                (strength-pump 2 2 :end-of-encounter {:label "add 2 strength (using at least 1 stealth [Credits])" :cost-req (min-stealth 1)})]}))

(defcard "Blackstone"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 3 4 :end-of-run {:label "add 4 strength (using at least 1 stealth [Credits])" :cost-req (min-stealth 1)})]}))

(defcard "Botulus"
  {:data {:counter {:virus 1}}
   :hosting {:card #(and (ice? %)
                         (can-host? %))}
   :events [{:event :runner-turn-begins
             :effect (effect (add-counter card :virus 1))}]
   :abilities [(break-sub
                 [:virus 1] 1 "All"
                 {:req (req (same-card? current-ice (:host card)))})]})

(defcard "Brahman"
  (auto-icebreaker {:abilities [(break-sub 1 2 "All")
                                (strength-pump 2 1)]
                    :events [{:event :end-of-encounter
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :player :runner ; Needed for when the run is ended by the Corp
                              :prompt "Choose a non-virus program to put on top of your stack."
                              :choices {:card #(and (installed? %)
                                                    (program? %)
                                                    (not (facedown? %))
                                                    (not (has-subtype? % "Virus")))}
                              :msg (msg "add " (:title target) " to the top of the Stack")
                              :effect (effect (move (get-card state target) :deck {:front true}))}]}))

(defcard "Breach"
  (central-only (break-sub 2 3 "Barrier")
                (strength-pump 2 4)))

(defcard "Bug"
  {:implementation "Can only pay to see last card drawn after multiple draws"
   :req (req (some #{:hq} (:successful-run runner-reg)))
   :events [{:event :corp-draw
             :optional
             {:prompt "Pay 2 [Credits] to reveal card just drawn?"
              :player :runner
              :yes-ability {:cost [:credit 2]
                            :msg (msg "reveal the card just drawn: " (:title (last (:hand corp))))}}}]})

(defcard "Bukhgalter"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 1)]
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (first-event? state side :subroutines-broken #(all-subs-broken-by-card? (first %) card))))
                              :msg (msg "gain 2 [Credits]")
                              :async true
                              :effect (effect (gain-credits :runner eid 2))}]}))

(defcard "Buzzsaw"
  (auto-icebreaker {:abilities [(break-sub 1 2 "Code Gate")
                                (strength-pump 3 1)]}))

(defcard "Cache"
  {:abilities [{:cost [:virus 1]
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

(defcard "Chakana"
  {:constant-effects [{:type :advancement-requirement
                       :req (req (<= 3 (get-virus-counters state card)))
                       :value 1}]
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :rd (target-server context)))
             :effect (effect (add-counter card :virus 1))}]})

(defcard "Chameleon"
  (auto-icebreaker {:on-install {:prompt "Choose one subtype"
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
  {:hosting {:card #(and (ice? %)
                         (can-host? %))}
   :constant-effects [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value (req (- (get-virus-counters state card)))}]
   :events [{:event :encounter-ice
             :req (req (same-card? (:ice context) (:host card)))
             :async true
             :effect (req (if (pos? (ice-strength state side (:ice context)))
                            (do (system-msg state side "places 1 virus counter on Chisel")
                                (add-counter state side card :virus 1)
                                (effect-completed state side eid))
                            (do (system-msg state side (str "uses Chisel to trash " (card-str state (:ice context))))
                                (trash state side eid (:ice context) nil))))}]})

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
                                    (filter #(agenda? (:card (first %))))
                                    (map first))]
                   (swap! state assoc-in [:corp :register :cannot-score] agendas)))}
   :events [{:event :purge
             :async true
             :effect (req (swap! state update-in [:corp :register] dissoc :cannot-score)
                          (trash state side eid card {:cause :purge}))}
            {:event :corp-install
             :req (req (agenda? (:card context)))
             :effect (req (swap! state update-in [:corp :register :cannot-score] #(cons (:card context) %)))}]
   :leave-play (req (swap! state update-in [:corp :register] dissoc :cannot-score))})

(defcard "Collective Consciousness"
  {:events [{:event :rez
             :req (req (ice? (:card target)))
             :msg "draw 1 card"
             :async true
             :effect (effect (draw :runner eid 1 nil))}]})

(defcard "Conduit"
  {:events [{:event :run-ends
             :optional
             {:req (req (and (:successful context)
                             (= :rd (target-server context))))
              :player :runner
              :waiting-prompt "Runner to decide if they will use Conduit"
              :autoresolve (get-autoresolve :auto-conduit)
              :prompt "Use Conduit?"
              :yes-ability {:msg "add 1 virus counter to Conduit"
                            :effect (effect (add-counter card :virus 1))}
              :no-ability {:effect (effect (system-msg "does not add counter to Conduit"))}}}
            {:event :successful-run
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (req (access-bonus state side :rd (max 0 (get-virus-counters state card))))}]
   :abilities [{:cost [:click 1]
                :msg "make a run on R&D"
                :makes-run true
                :async true
                :effect (req (make-run state side eid :rd card))}
               (set-autoresolve :auto-conduit "Conduit")]})

(defcard "Consume"
  {:events [{:event :runner-trash
             :once-per-instance true
             :async true
             :req (req (some #(corp? (:card %)) targets))
             :effect (req (let [amt-trashed (count (filter #(corp? (:card %)) targets))
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
                            (continue-ability state side ab card targets)))}]
   :abilities [{:req (req (pos? (get-virus-counters state card)))
                :cost [:click 1]
                :label "Gain 2 [Credits] for each hosted virus counter, then remove all virus counters."
                :async true
                :effect (req (wait-for (gain-credits state side (* 2 (get-virus-counters state card)))
                                       (update! state side (assoc-in card [:counter :virus] 0))
                                       (doseq [h (filter #(= "Hivemind" (:title %)) (all-active-installed state :runner))]
                                         (update! state side (assoc-in h [:counter :virus] 0)))
                                       (effect-completed state side eid)))
                :msg (msg (let [local-virus (get-counters card :virus)
                                global-virus (get-virus-counters state card)
                                hivemind-virus (- global-virus local-virus)]
                            (str "gain " (* 2 global-virus) " [Credits], removing " (quantify local-virus "virus counter") " from Consume"
                                 (when (pos? hivemind-virus)
                                   (str " (and " hivemind-virus " from Hivemind)")))))}
               (set-autoresolve :auto-accept "adding virus counters")]})

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
                               (swap! state update-in [:run]
                                      #(assoc % :position tgtndx :server [dest]))
                               (set-current-ice state)
                               (trash state side eid card {:unpreventable true})))}]})

(defcard "Cordyceps"
  {:data {:counter {:virus 2}}
   :events [{:event :successful-run
             :interactive (req true)
             :optional
             {:req (req (and (is-central? (target-server context))
                             (can-pay? state side eid card nil [:virus 1])
                             (not-empty run-ices)
                             (<= 2 (count (filter ice? (all-installed state :corp))))))
              :once :per-turn
              :prompt "Use Cordyceps to swap ice?"
              :yes-ability
              {:prompt "Choose ice protecting this server"
               :choices {:req (req (and (installed? target)
                                        (ice? target)
                                        (= (target-server (:run @state)) (second (get-zone target)))))}
               :async true
               :effect (effect
                         (continue-ability
                           (let [first-ice target]
                             {:prompt "Choose ice to swap with"
                              :choices {:req (req (and (installed? target)
                                                       (ice? target)
                                                       (not= first-ice target)))}
                              :msg (msg "swap the positions of " (card-str state first-ice)
                                        " and " (card-str state target))
                              :async true
                              :effect (req (wait-for (pay state side card [:virus 1])
                                                     (system-msg state side (:msg async-result))
                                                     (swap-ice state side first-ice target)
                                                     (effect-completed state side eid)))})
                           card nil))}}}]})

(defcard "Corroder"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 1 1)]}))

(defcard "Cradle"
  (auto-icebreaker {:abilities [(break-sub 2 0 "Code Gate")]
                    :strength-bonus (req (- (count (:hand runner))))}))

(defcard "Creeper"
  (cloud-icebreaker
    (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")
                                  (strength-pump 1 1)]})))

(defcard "Crescentus"
  {:abilities [{:req (req (and run
                               (= :encounter-ice (:phase run))
                               (all-subs-broken? current-ice)))
                :label "derez an ice"
                :cost [:trash]
                :msg (msg "derez " (:title current-ice))
                :effect (effect (derez current-ice))}]})

(defcard "Crowbar"
  (break-and-enter "Code Gate"))

(defcard "Crypsis"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All")
                                (strength-pump 1 1)
                                {:cost [:click 1]
                                 :keep-open :while-clicks-left
                                 :msg "place 1 virus counter"
                                 :effect (effect (add-counter card :virus 1))}]
                    :events [{:event :end-of-encounter
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :msg (msg (if (can-pay? state side eid card nil [:virus 1])
                                          "remove a virus token from Crypsis"
                                          "trash Crypsis"))
                              :async true
                              :effect (req (wait-for (pay state :runner card [:virus 1])
                                                     (if-let [payment-str (:msg async-result)]
                                                       (do (system-msg state :runner payment-str)
                                                           (effect-completed state side eid))
                                                       (trash state side eid card nil))))}]}))

(defcard "Customized Secretary"
  (letfn [(custsec-host [cards]
            (when (seq (filter program? cards))
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
                  :msg (msg "reveal the top 5 cards of their Stack: " (string/join ", " (map :title (take 5 (:deck runner)))))
                  :waiting-prompt "Runner to host programs on Customized Secretary"
                  :effect (req (let [from (take 5 (:deck runner))]
                                 (wait-for (reveal state side from)
                                           (continue-ability state side (custsec-host from) card nil))))}
     :abilities [{:cost [:click 1]
                  :keep-open :while-clicks-left
                  :label "Install a hosted program"
                  :prompt "Choose a program to install"
                  :choices (req (cancellable (filter #(can-pay? state side (assoc eid :source card :source-type :runner-install)
                                                                % nil [:credit (install-cost state side %)])
                                                     (:hosted card))))
                  :msg (msg "install " (:title target))
                  :async true
                  :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]}))

(defcard "Cyber-Cypher"
  (auto-icebreaker
    {:on-install {:prompt "Choose a server"
                  :msg (msg "target " target)
                  :choices (req servers)
                  :effect (effect (update! (assoc card :server-target target)))}
     :leave-play (effect (update! (dissoc card :server-target)))
     :abilities [(break-sub 1 1 "Code Gate" {:req (req (if (:server-target card)
                                                         (#{(last (server->zone state (:server-target card)))} (target-server run))
                                                         true))})
                 (strength-pump 1 1 :end-of-encounter {:req (req (if (:server-target card)
                                                                   (#{(last (server->zone state (:server-target card)))} (target-server run))
                                                                   true))})]}))

(defcard "D4v1d"
  (let [david-req (req (<= 5 (get-strength current-ice)))]
    {:data {:counter {:power 3}}
     :abilities [(break-sub [:power 1] 1 "All" {:req david-req})]}))

(defcard "Dagger"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 5 {:cost-req (min-stealth 1)})]}))

(defcard "Dai V"
  (auto-icebreaker {:abilities [(break-sub 2 0 "All" {:all true :cost-req all-stealth})
                                (strength-pump 1 1)]}))

(defcard "Darwin"
  (auto-icebreaker {:flags {:runner-phase-12 (req true)}
                    :abilities [(break-sub 2 1)
                                {:label "Place 1 virus counter (start of turn)"
                                 :once :per-turn
                                 :cost [:credit 1]
                                 :msg "place 1 virus counter"
                                 :req (req (:runner-phase-12 @state))
                                 :effect (effect (add-counter card :virus 1))}]
                    :strength-bonus (req (get-virus-counters state card))}))

(defcard "Datasucker"
  {:events [{:event :successful-run
             :silent (req true)
             :req (req (is-central? (target-server context)))
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:cost [:virus 1]
                :label "Give -1 strength to current piece of ice"
                :req (req (and (rezzed? current-ice)
                               (= :encounter-ice (:phase run))))
                :msg (msg "give -1 strength to " (:title current-ice))
                :effect (effect (pump-ice current-ice -1))}]})

(defcard "DaVinci"
  {:events [{:event :successful-run
             :silent (req true)
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:req (req (and (not (install-locked? state side))
                               (some #(and (or (hardware? %)
                                               (program? %)
                                               (resource? %))
                                           (<= (install-cost state side %) (get-counters card :power)))
                                     (:hand runner))))
                :label "install a card from the grip"
                :cost [:trash]
                :async true
                :effect (effect
                          (continue-ability
                            {:waiting-prompt "Runner to use DaVinci"
                             :prompt "Choose a card to install from your Grip"
                             :msg (msg "install " (:title target) " at no cost")
                             :choices {:req (req (and (in-hand? target)
                                                      (or (hardware? target)
                                                          (program? target)
                                                          (resource? target))
                                                      (<= (install-cost state side target)
                                                          (get-counters (cost-target eid :trash) :power))))}
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
             :req (req (>= (get-virus-counters state card) 3)) :msg "look at the top card of R&D"
             :effect (effect (prompt! card (str "The top card of R&D is "
                                                (:title (first (:deck corp)))) ["OK"] {}))}]})

(defcard "Demara"
  (trash-to-bypass (break-sub 2 2 "Barrier")
                   (strength-pump 2 3)))

(defcard "Deus X"
  {:interactions {:prevent [{:type #{:net}
                             :req (req true)}]}
   :abilities [(break-sub [:trash] 0 "AP")
               {:msg "prevent any amount of net damage"
                :cost [:trash]
                :effect (effect (damage-prevent :net Integer/MAX_VALUE))}]})

(defcard "Dhegdheer"
  {:abilities [{:req (req (and (not (get-in card [:special :dheg-prog]))
                               (some #(and (program? %)
                                           (runner-can-install? state side % false)
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [:credit (install-cost state side % {:cost-bonus -1})]))
                                     (:hand runner))))
                :cost [:click 1]
                :label "Install a program on Dhegdheer"
                :prompt "Choose a program in your Grip to install on Dhegdheer"
                :choices
                {:req (req (and (program? target)
                                (runner-can-install? state side target false)
                                (in-hand? target)
                                (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                          [:credit (install-cost state side target {:cost-bonus -1})])))}
                :msg (msg (str "host " (:title target)
                               (when (-> target :cost pos?)
                                 ", lowering its cost by 1 [Credit]")))
                :async true
                :effect (effect (update! (assoc-in card [:special :dheg-prog] (:cid target)))
                                (runner-install (assoc eid :source (get-card state card) :source-type :runner-install)
                                                target {:host-card (get-card state card)
                                                        :no-mu true
                                                        :cost-bonus -1}))}
               {:label "Host an installed program on Dhegdheer with [Credit] discount"
                :req (req (nil? (get-in card [:special :dheg-prog])))
                :prompt "Choose an installed program to host on Dhegdheer with [Credit] discount"
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
               {:label "Host an installed program on Dhegdheer"
                :req (req (nil? (get-in card [:special :dheg-prog])))
                :prompt "Choose an installed program to host on Dhegdheer"
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
   [{:event :pre-init-trace
     :trash-icon true
     :optional
     {:player :runner
      :waiting-prompt "Runner to use Disruptor"
      :prompt "Use Disrupter's ability?"
      :yes-ability
      {:cost [:trash]
       :effect (req (swap! state assoc-in [:trace :force-base] 0))}}}]})

(defcard "Diwan"
  {:on-install {:prompt "Choose the server that this copy of Diwan is targeting:"
                :choices (req servers)
                :effect (effect (update! (assoc card :server-target target)))}
   :constant-effects [{:type :install-cost
                       :req (req (let [serv (:server (second targets))]
                                   (and (= serv (:server-target card))
                                        (not (and (is-central? serv)
                                                  (upgrade? target))))))
                       :value 1}]
   :events [{:event :purge
             :async true
             :effect (effect (trash eid card {:cause :purge}))}]})

(defcard "Djinn"
  {:abilities [{:label "Search your Stack for a virus program and add it to your Grip"
                :prompt "Choose a Virus"
                :msg (msg "add " (:title target) " to their Grip")
                :choices (req (cancellable (filter #(and (program? %)
                                                         (has-subtype? % "Virus"))
                                                   (:deck runner)) :sorted))
                :cost [:click 1 :credit 1]
                :keep-open :while-clicks-left
                :effect (effect (trigger-event :searched-stack nil)
                                (shuffle! :deck)
                                (move target :hand))}
               {:label "Install a non-Icebreaker program on Djinn"
                :cost [:click 1]
                :prompt "Choose a non-Icebreaker program in your grip"
                :choices {:req (req (and (program? target)
                                         (runner-can-install? state side target false)
                                         (not (has-subtype? target "Icebreaker"))
                                         (in-hand? target)))}
                :msg (msg "install and host " (:title target))
                :async true
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed non-Icebreaker program on Djinn"
                :prompt "Choose an installed non-Icebreaker program"
                :choices {:card #(and (program? %)
                                      (not (has-subtype? % "Icebreaker"))
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]})

(defcard "Eater"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All" {:additional-ability {:msg (msg "access not more than 0 cards for the remainder of this run")
                                                                           :effect (effect (max-access 0))}
                                                      :label "break 1 subroutine and access 0 cards"})
                                (strength-pump 1 1)]}))

(defcard "Echelon"
  (auto-icebreaker {:strength-bonus (req (count (filter #(and (program? %)
                                                              (has-subtype? % "Icebreaker"))
                                                        (all-active-installed state :runner))))
                    :abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 3 2)]}))

(defcard "Egret"
  {:hosting {:card #(and (ice? %)
                         (can-host? %)
                         (rezzed? %))}
   :on-install {:msg (msg "make " (card-str state (:host card))
                          " gain Barrier, Code Gate and Sentry subtypes")}
   :constant-effects [{:type :gain-subtype
                       :req (req (same-card? target (:host card)))
                       :value ["Barrier" "Code Gate" "Sentry"]}]})

(defcard "Endless Hunger"
  {:implementation "ETR restriction not implemented"
   :abilities [(break-sub [:installed 1] 1 "All" {:label "break 1 \"[Subroutine] End the run.\" subroutine"})]})

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
                                     (draw state :corp eid 1 nil))}}})
        reveal {:optional
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
               :waiting-prompt "Runner to use Equivocation"
               :effect (effect (continue-ability reveal card nil))}]}))

(defcard "Euler"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Code Gate" {:req (req (= :this-turn (installed? card)))})
                                (break-sub 2 2 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "eXer"
  {:in-play [:rd-access 1]
   :events [{:event :purge
             :async true
             :effect (effect (trash eid card {:cause :purge}))}]})

(defcard "Expert Schedule Analyzer"
  {:abilities [{:cost [:click 1]
                :msg "make a run on HQ"
                :makes-run true
                :async true
                :effect (effect (make-run eid :hq card))}]
   :events [(successful-run-replace-access
              {:target-server :hq
               :this-card-run true
               :ability
               {:msg (msg "reveal all of the cards cards in HQ: "
                          (string/join ", " (map :title (:hand corp))))
                :async true
                :effect (effect (reveal eid (:hand corp)))}})]})

(defcard "Faerie"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Sentry")
                                (strength-pump 1 1)]
                    :events [{:event :end-of-encounter
                              :async true
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :msg (msg "trash " (:title card))
                              :effect (effect (trash eid card nil))}]}))

(defcard "False Echo"
  {:events [{:event :pass-ice
             :optional
             {:req (req (not (rezzed? (:ice context))))
              :prompt "Trash False Echo?"
              :yes-ability
              {:async true
               :msg "trashes False Echo to make the Corp rez the passed piece of ice or add it to HQ"
               :effect
               (req (wait-for
                      (trash state side card nil)
                      (continue-ability
                        state side
                        (let [ice (:ice context)]
                          {:async true
                           :prompt (msg "Rez " (:title ice) " or add it to HQ?")
                           :player :corp
                           :choices (req (if (can-pay? state :runner eid card nil [:credit (rez-cost state side ice)])
                                           ["Rez" "Add to HQ"]
                                           ["Add to HQ"]))
                           :effect (req (if (= target "Rez")
                                          (rez state side eid ice)
                                          (do (system-msg state :corp "chooses to add the passed piece of ice to HQ")
                                              (move state :corp ice :hand)
                                              (effect-completed state side eid))))})
                        card target)))}}}]})

(defcard "Faust"
  {:abilities [(break-sub [:trash-from-hand 1] 1)
               (strength-pump [:trash-from-hand 1] 2)]})

(defcard "Fawkes"
  {:abilities [(break-sub 1 1 "Sentry")
               {:label "+X strength for the remainder of the run (using at least 1 stealth [Credits])"
                :cost [:x-credits]
                :cost-req (min-stealth 1)
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
                     (add-icon state side card ice "F" "blue")
                     (system-msg state side
                                 (str "selects " (card-str state ice)
                                      " for Femme Fatale's bypass ability"))
                     (register-events
                       state side card
                       [{:event :encounter-ice
                         :interactive (req true)
                         :optional
                         {:req (req (and (same-card? ice (:ice context))
                                         (can-pay? state :runner eid (:ice context) nil [:credit (count (:subroutines (get-card state ice)))])))
                          :prompt (str "Pay " (count (:subroutines (get-card state ice)))
                                       " [Credits] to bypass " (:title ice) "?")
                          :yes-ability {:cost [:credit (count (:subroutines (get-card state ice)))]
                                        :msg (msg "bypass " (:title (:ice context)))
                                        :effect (req (bypass-ice state))}}}])))}
     :leave-play (req (remove-icon state side card))
     :abilities [(break-sub 1 1 "Sentry")
                 (strength-pump 2 1)]}))

(defcard "Fermenter"
  {:on-install {:effect (effect (add-counter card :virus 1))}
   :events [{:event :runner-turn-begins
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:req (req (pos? (get-virus-counters state card)))
                :cost [:click 1 :trash]
                :label "Gain 2 [Credits] for each hosted virus counter"
                :msg (msg (str "gain " (* 2 (get-virus-counters state card)) " [Credits]"))
                :async true
                :effect (effect (gain-credits eid (* 2 (get-virus-counters state card))))}]})

(defcard "Flashbang"
  (auto-icebreaker {:abilities [{:label "Derez a Sentry being encountered"
                                 :cost [:credit 6]
                                 :req (req (and (= :encounter-ice (:phase run))
                                                (has-subtype? current-ice "Sentry")))
                                 :msg (msg "derez " (:title current-ice))
                                 :effect (effect (derez current-ice))}
                                (strength-pump 1 1)]}))

(defcard "Force of Nature"
  (auto-icebreaker {:abilities [(break-sub 2 2 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "Garrote"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 1)]}))

(defcard "Gauss"
  (auto-icebreaker {:strength-bonus (req (if (= :this-turn (installed? card)) 3 0))
                    :abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 2 2)]}))

(defcard "Gingerbread"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Tracer")
                                (strength-pump 2 3)]}))

(defcard "God of War"
  (auto-icebreaker {:flags {:runner-phase-12 (req true)}
                    :abilities [(break-sub [:virus 1] 1)
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
  {:abilities [{:cost [:click 1 :trash]
                :label "gain credits"
                :async true
                :effect (effect (gain-credits eid (get-virus-counters state card)))
                :msg (msg "gain " (get-virus-counters state card) " [Credits]")}]
   :events [{:event :corp-click-credit
             :effect (effect (add-counter :runner card :virus 1))}
            {:event :corp-click-draw
             :effect (effect (add-counter :runner card :virus 1))}]})

(defcard "Grappling Hook"
  (let [break-subs (fn [state ice subroutines]
                     (doseq [sub subroutines]
                       (break-subroutine! state (get-card state ice) sub)))]
    {:abilities [{:label "break all but 1 subroutine"
                  :req (req (and current-ice
                                 (rezzed? current-ice)
                                 (< 1 (count (remove :broken (:subroutines current-ice))))))
                  :break 1 ;technically not correct, but will only be used by the engine to check for breaking abilities
                  :breaks "All"
                  :break-cost [:trash]
                  :cost [:trash]
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
                  :cost [:click 1 :virus 1]
                  :keep-open :while-virus-tokens-left
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
    :effect (req (let [lock (get-in @state [:runner :locked :discard])]
                   (swap! state assoc-in [:runner :locked] nil)
                   (flip-facedown state side card)
                   (swap! state assoc-in [:runner :locked] lock)))}})

(defcard "Hemorrhage"
  {:events [{:event :successful-run
             :silent (req true)
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:cost [:click 1 :virus 2]
                :keep-open :while-2-virus-tokens-left
                :req (req (pos? (count (:hand corp))))
                :msg "force the Corp to trash 1 card from HQ"
                :async true
                :effect (req (continue-ability
                               state :corp
                               {:waiting-prompt "Corp to trash a card"
                                :prompt "Choose a card to trash"
                                :choices (req (filter corp? (:hand corp)))
                                :async true
                                :effect (effect (trash eid target nil))}
                               card nil))}]})

(defcard "Hivemind"
  {:data {:counter {:virus 1}}
   :abilities [{:req (req (pos? (get-counters card :virus)))
                :label "move hosted virus counters"
                :prompt "Move a virus counter to which card?"
                :choices {:card #(has-subtype? % "Virus")
                          :not-self true}
                :msg (msg "manually move a virus counter from Hivemind to " (:title target))
                :effect (effect (add-counter :runner target :virus 1)
                                (add-counter :runner card :virus -1))}]})

(defcard "Houdini"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 2 4 :end-of-run {:label "add 4 strength (using at least 1 stealth [Credits])" :cost-req (min-stealth 1)})]}))

(defcard "Hyperdriver"
  {:flags {:runner-phase-12 (req true)}
   :abilities [{:label "Remove Hyperdriver from the game to gain [Click] [Click] [Click]"
                :req (req (:runner-phase-12 @state))
                :effect (effect (move card :rfg)
                                (gain :click 3))
                :msg "gain [Click][Click][Click]"}]})

(defcard "Ika"
  (auto-icebreaker {:abilities [{:label "Host Ika on a piece of ice"
                                 :prompt (msg "Host Ika on a piece of ice")
                                 :cost [:credit 2]
                                 :choices {:card #(and (ice? %)
                                                       (installed? %)
                                                       (can-host? %))}
                                 :msg (msg "host it on " (card-str state target))
                                 :effect (effect (host target card))}
                                (break-sub 1 2 "Sentry")
                                (strength-pump 2 3)]}))

(defcard "Imp"
  {:data {:counter {:virus 2}}
   :interactions {:access-ability {:label "Trash card"
                                   :req (req (not (get-in @state [:per-turn (:cid card)])))
                                   :cost [:virus 1]
                                   :msg (msg "trash " (:title target) " at no cost")
                                   :once :per-turn
                                   :async true
                                   :effect (effect (trash eid (assoc target :seen true) {:accessed true}))}}})

(defcard "Incubator"
  {:events [{:event :runner-turn-begins
             :effect (effect (add-counter card :virus 1))}]
   :abilities [{:cost [:click 1 :trash]
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
             :effect (effect (trash eid card {:cause :purge}))}]})

(defcard "Keyhole"
  {:abilities [{:cost [:click 1]
                :msg "make a run on R&D"
                :makes-run true
                :async true
                :effect (effect (make-run eid :rd card))}]
   :events [(successful-run-replace-access
              {:target-server :rd
               :this-card-run true
               :mandatory true
               :ability
               {:prompt "Choose a card to trash"
                :not-distinct true
                :msg (msg "trash " (:title target))
                :choices (req (take 3 (:deck corp)))
                :async true
                :effect (effect (shuffle! :corp :deck)
                                (trash eid (assoc target :seen true) nil))}})]})

(defcard "Knight"
  (let [knight-req (req (and (same-card? current-ice (get-nested-host card))
                             (<= (get-strength current-ice) (get-strength card))))]
    {:abilities [{:label "Host Knight on a piece of ice"
                  :async true
                  :effect (req (let [k (get-card state card)
                                     hosted (ice? (:host k))
                                     icepos (card-index state (get-card state (:host k)))]
                                 (continue-ability
                                   state side
                                   {:prompt (msg "Host Knight on a piece of ice"
                                                 (when hosted " not before or after the current host ice"))
                                    :cost [:click 1]
                                    :choices {:card #(if hosted
                                                       (and (or (when (= (get-zone %) (get-zone (:host k)))
                                                                  (not= 1 (abs (- (card-index state %) icepos))))
                                                                (not= (get-zone %) (get-zone (:host k))))
                                                            (ice? %)
                                                            (can-host? %)
                                                            (installed? %)
                                                            (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %)))
                                                       (and (ice? %)
                                                            (installed? %)
                                                            (can-host? %)
                                                            (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %))))}
                                    :msg (msg "host it on " (card-str state target))
                                    :effect (effect (host target card))}
                                   card nil)))}
                 (break-sub 2 1 "All" {:req knight-req})]}))

(defcard "Kyuban"
  {:hosting {:card #(and (ice? %)
                         (can-host? %))}
   :events [{:event :pass-ice
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
             :effect (effect (trash eid card {:cause :purge}))}]})

(defcard "Leech"
  {:events [{:event :successful-run
             :req (req (is-central? (target-server context)))
             :msg "add 1 virus counter to Leech"
             :effect (req (add-counter state side card :virus 1))}]
   :autoresolve (get-autoresolve :auto-fire)
   :abilities [{:cost [:virus 1]
                :label "Give -1 strength to current piece of ice"
                :req (req (and (rezzed? current-ice)
                               (= :encounter-ice (:phase run))))
                :msg (msg "give -1 strength to " (:title current-ice))
                :effect (effect (pump-ice current-ice -1))}
               (set-autoresolve :auto-fire "Leech")]})

(defcard "Leprechaun"
  {:abilities [{:label "Install a program on Leprechaun"
                :cost [:click 1]
                :prompt "Choose a program in your Grip to install on Leprechaun"
                :choices {:req (req (and (program? target)
                                         (runner-can-install? state side target false)
                                         (in-hand? target)))}
                :msg (msg "host " (:title target))
                :async true
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed program on Leprechaun"
                :prompt "Choose an installed program to host on Leprechaun"
                :choices {:card #(and (program? %)
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]})

(defcard "Leviathan"
  (auto-icebreaker {:abilities [(break-sub 3 3 "Code Gate")
                                (strength-pump 3 5)]}))

(defcard "LLDS Energy Regulator"
  {:interactions {:prevent [{:type #{:trash-hardware}
                             :req (req true)}]}
   :abilities [{:cost [:credit 3]
                :msg "prevent a hardware from being trashed"
                :effect (effect (trash-prevent :hardware 1))}
               {:cost [:trash]
                :msg "prevent a hardware from being trashed"
                :effect (effect (trash-prevent :hardware 1))}]})

(defcard "Lustig"
  (trash-to-bypass (break-sub 1 1 "Sentry")
                   (strength-pump 3 5)))

(defcard "Magnum Opus"
  {:abilities [{:cost [:click 1]
                :keep-open :while-clicks-left
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
                              :msg (msg "gain 1 [Credits]")
                              :async true
                              :effect (effect (gain-credits :runner eid 1))}]}))

(defcard "Mammon"
  (auto-icebreaker {:flags {:runner-phase-12 (req (pos? (:credit runner)))}
                    :abilities [{:label "Place X power counters"
                                 :prompt "How many power counters to place on Mammon?"
                                 :once :per-turn
                                 :cost [:x-credits]
                                 :req (req (:runner-phase-12 @state))
                                 :async true
                                 :effect (effect (add-counter card :power (cost-value eid :x-credits)))
                                 :msg (msg "place " (cost-value eid :x-credits) " power counters on it")}
                                (break-sub [:power 1] 1)
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
                                            :break-cost-bonus (req (when (:successful-run runner-reg) [:credit -1]))})
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

(defcard "Maven"
  (auto-icebreaker {:abilities [(break-sub 2 1)]
                    :strength-bonus (req (count (filter program? (all-active-installed state :runner))))}))

(defcard "Mayfly"
  (auto-icebreaker
    {:abilities [(break-sub
                   1 1 "All"
                   {:additional-ability
                    {:msg "will trash itself when this run ends"
                     :effect (effect
                               (register-events
                                 card
                                 [{:event :run-ends
                                   :duration :end-of-run
                                   :unregister-once-resolved true
                                   :async true
                                   :effect (effect (trash eid card))}]))}})
                 (strength-pump 1 1)]}))

(defcard "Medium"
  {:events [{:event :successful-run
             :req (req (= :rd (target-server context)))
             :effect (effect (add-counter card :virus 1))}
            {:event :pre-access
             :async true
             :req (req (= target :rd))
             :effect (effect (continue-ability
                               {:req (req (< 1 (get-virus-counters state card)))
                                :prompt "Choose how many additional R&D accesses to make with Medium"
                                :choices {:number (req (dec (get-virus-counters state card)))
                                          :default (req (dec (get-virus-counters state card)))}
                                :msg (msg "access " target " additional cards from R&D")
                                :effect (effect (access-bonus :rd (max 0 target)))}
                               card nil))}]})

(defcard "Mimic"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")]}))

(defcard "Misdirection"
  {:abilities [{:cost [:click 2 :x-credits]
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
                                [(pump-and-break [:credit 3] 2 "Sentry")])]
    (assoc cdef :events (apply conj events (:events cdef)))))

(defcard "Mongoose"
  (auto-icebreaker {:implementation "Usage restriction is not implemented"
                    :abilities [(break-sub 1 2 "Sentry")
                                (strength-pump 2 2)]}))

(defcard "Morning Star"
  (auto-icebreaker {:abilities [(break-sub 1 0 "Barrier")]}))

(defcard "Multithreader"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (program? target)))
                                :type :recurring}}})

(defcard "Musaazi"
  (virus-breaker "Sentry"))

(defcard "Na'Not'K"
  (auto-icebreaker {:strength-bonus (req (count run-ices))
                    :abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 3 2)]}))

(defcard "Nerve Agent"
  {:events [{:event :successful-run
             :req (req (= :hq (target-server context)))
             :effect (effect (add-counter card :virus 1))}
            {:event :pre-access
             :async true
             :req (req (= target :hq))
             :effect (effect (continue-ability
                               {:req (req (< 1 (get-virus-counters state card)))
                                :prompt "Choose how many additional HQ accesses to make with Nerve Agent"
                                :choices {:number (req (dec (get-virus-counters state card)))
                                          :default (req (dec (get-virus-counters state card)))}
                                :msg (msg "access " target " additional cards from HQ")
                                :effect (effect (access-bonus :hq (max 0 target)))}
                               card nil))}]})

(defcard "Net Shield"
  {:interactions {:prevent [{:type #{:net}
                             :req (req true)}]}
   :abilities [{:cost [:credit 1]
                :once :per-turn
                :msg "prevent the first net damage this turn"
                :effect (effect (damage-prevent :net 1))}]})

(defcard "Nfr"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")]
                    :strength-bonus (req (get-counters card :power))
                    :events [{:event :end-of-encounter
                              :req (req (all-subs-broken-by-card? (:ice context) card))
                              :msg "place 1 power counter on it"
                              :effect (effect (add-counter card :power 1))}]}))

(defcard "Ninja"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 3 5)]}))

(defcard "Nyashia"
  {:data {:counter {:power 3}}
   :events [{:event :pre-access
             :optional
             {:req (req (and (pos? (get-counters card :power))
                             (= target :rd)))
              :waiting-prompt "Runner to use Nyashia"
              :prompt "Spend a power counter on Nyashia to access 1 additional card?"
              :autoresolve (get-autoresolve :auto-nyashia)
              :yes-ability {:msg "access 1 additional card from R&D"
                            :effect (effect (access-bonus :rd 1)
                                            (add-counter card :power -1))}}}]
   :abilities [(set-autoresolve :auto-nyashia "Nyashia")]})

(defcard "Odore"
  (auto-icebreaker {:abilities [(break-sub 2 0 "Sentry"
                                           {:req (req (> 3 (count (filter #(has-subtype? % "Virtual")
                                                                          (all-active-installed state :runner)))))})
                                (break-sub 0 1 "Sentry"
                                           {:label "Break 1 Sentry subroutine (Virtual restriction)"
                                            :req (req (<= 3 (count (filter #(has-subtype? % "Virtual")
                                                                           (all-active-installed state :runner)))))})
                                (strength-pump 3 3)]}))

(defcard "Omega"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All" {:req (req (= 1 (:position run)))})
                                (strength-pump 1 1 :end-of-encounter {:req (req (= 1 (:position run)))})]}))

(defcard "Origami"
  {:constant-effects [{:type :hand-size
                       :req (req (= :runner side))
                       :value (req (count (filter #(= (:title %) "Origami")
                                                  (all-active-installed state :runner))))}]})

(defcard "Overmind"
  (auto-icebreaker {:on-install {:effect (effect (add-counter card :power (available-mu state)))}
                    :abilities [(break-sub [:power 1] 1)
                                (strength-pump 1 1)]}))

(defcard "Paintbrush"
  {:abilities [{:cost [:click 1]
                :label "give ice a subtype"
                :choices {:card #(and (installed? %)
                                      (ice? %)
                                      (rezzed? %))}
                :async true
                :effect (effect
                          (continue-ability
                            (let [ice target]
                              {:prompt "Choose a subtype"
                               :choices ["Sentry" "Code Gate" "Barrier"]
                               :msg (msg "spend [Click] and make " (card-str state ice)
                                         " gain " target
                                         " until the end of the next run this turn")
                               :effect (effect (register-floating-effect
                                                 card
                                                 {:type :gain-subtype
                                                  :duration :end-of-next-run
                                                  :req (req (same-card? ice target))
                                                  :value target}))})
                            card nil))}]})

(defcard "Panchatantra"
  {:events [{:event :encounter-ice
             :optional
             {:prompt "Give ice a subtype?"
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
                               (register-floating-effect
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
                    :cost [:x-credits]
                    :heap-breaker-pump :x ; strength gained
                    :heap-breaker-break :x ; number of subs broken
                    :break-req (req (and current-ice
                                         (rezzed? current-ice)
                                         (= :encounter-ice (:phase run))
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
  {:hosting {:card #(and (ice? %)
                         (can-host? %)
                         (rezzed? %))}
   :on-install
   {:effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))}
   :constant-effects [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value (req (- (get-virus-counters state card)))}]
   :events [{:event :runner-turn-begins
             :effect (req (add-counter state side card :virus 1))}
            {:event :ice-strength-changed
             :req (req (and (same-card? target (:host card))
                            (not (card-flag? (:host card) :untrashable-while-rezzed true))
                            (<= (get-strength target) 0)))
             :async true
             :effect (req (unregister-events state side card)
                          (when (get-in card [:special :installing])
                            (update! state side (update-in card [:special] dissoc :installing))
                            (trigger-event state :runner :runner-install card))
                          (trash state :runner eid target {:unpreventable true}))
             :msg (msg "trash " (:title target))}]})

(defcard "Paricia"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (asset? target)))
                                :type :recurring}}})

(defcard "Passport"
  (central-only (break-sub 1 1 "Code Gate")
                (strength-pump 2 2)))

(defcard "Pawn"
  {:implementation "All abilities are manual"
   :abilities [{:label "Host Pawn on the outermost piece of ice of a central server"
                :cost [:click 1]
                :prompt "Host Pawn on the outermost piece of ice of a central server"
                :choices {:card #(and (ice? %)
                                      (can-host? %)
                                      (= (last (get-zone %)) :ices)
                                      (is-central? (second (get-zone %))))}
                :msg (msg "host it on " (card-str state target))
                :effect (effect (host target card))}
               {:label "Advance to next piece of ice"
                :prompt "Choose the next innermost piece of ice to host Pawn on it"
                :choices {:card #(and (ice? %)
                                      (can-host? %)
                                      (= (last (get-zone %)) :ices)
                                      (is-central? (second (get-zone %))))}
                :msg (msg "host it on " (card-str state target))
                :effect (effect (host target card))}
               {:req (req (not (zone-locked? state :runner :discard)))
                :label "Trash Pawn and install a Caïssa from your Grip or Heap, ignoring all costs"
                :async true
                :effect (req (let [this-pawn (:cid card)]
                               (wait-for (trash state side card nil)
                                         (continue-ability
                                           state side
                                           {:prompt "Choose a Caïssa program to install from your Grip or Heap"
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
                :req (req (and current-ice
                               (rezzed? current-ice)
                               (= :encounter-ice (:phase run))))
                :cost [:virus 1]
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
                          " until end of the run")
                :effect (effect (register-floating-effect
                                  card
                                  (let [ice current-ice]
                                    {:type :gain-subtype
                                     :duration :end-of-run
                                     :req (req (same-card? target ice))
                                     :value target})))}]})

(defcard "Penrose"
  (auto-icebreaker {:implementation "Stealth credit restriction not enforced"
                    :abilities [(break-sub 1 1 "Barrier" {:req (req (= :this-turn (installed? card)))})
                                (break-sub 1 1 "Code Gate")
                                (strength-pump 1 3 {:cost-req (min-stealth 1)})]}))

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
                                     {:prompt (str "Use Persephone to trash " (quantify fired-subs "card") " from R&D?")
                                      :yes-ability
                                      {:async true
                                       :msg (msg (str "trash " (:title (first (:deck runner)))
                                                      " from the stack and"
                                                      " trash " (quantify fired-subs "card") " from R&D"))
                                       :effect (req (wait-for (mill state :runner :runner 1)
                                                              (mill state :runner eid :corp fired-subs)))}}})
                                  card nil))}]}))

(defcard "Pheromones"
  {:recurring (req (get-counters card :virus))
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :hq (target-server context)))
             :effect (effect (add-counter card :virus 1))}]
   :interactions {:pay-credits {:req (req (= :hq (get-in @state [:run :server 0])))
                                :type :recurring}}})

(defcard "Pipeline"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 2 1 :end-of-run)]}))

(defcard "Plague"
  {:on-install {:prompt "Choose a server for Plague"
                :choices (req servers)
                :msg (msg "target " target)
                :req (req (not (get-in card [:special :server-target])))
                :effect (effect (update! (assoc-in card [:special :server-target] target)))}
   :events [{:event :successful-run
             :req (req (= (zone->name (:server context))
                          (get-in (get-card state card) [:special :server-target])))
             :msg "gain 2 virus counters"
             :effect (effect (add-counter :runner card :virus 2))}]})

(defcard "Progenitor"
  {:abilities [{:label "Install a virus program on Progenitor"
                :req (req (empty? (:hosted card)))
                :cost [:click 1]
                :prompt "Choose a Virus program to install on Progenitor"
                :choices {:card #(and (program? %)
                                      (has-subtype? % "Virus")
                                      (in-hand? %))}
                :msg (msg "host " (:title target))
                :async true
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed virus on Progenitor"
                :req (req (empty? (:hosted card)))
                :prompt "Choose an installed virus program to host on Progenitor"
                :choices {:card #(and (program? %)
                                      (has-subtype? % "Virus")
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]
   :events [{:event :pre-purge
             :effect (req (when-let [c (first (:hosted card))]
                            (update! state side (assoc-in card [:special :numpurged] (get-counters c :virus)))))}
            {:event :purge
             :req (req (pos? (get-in card [:special :numpurged] 0)))
             :effect (req (when-let [c (first (:hosted card))]
                            (add-counter state side c :virus 1)))}]})

(defcard "Puffer"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 2 1)
                                {:cost [:click 1]
                                 :msg "place one power counter"
                                 :label "Place 1 power counter"
                                 :effect (effect (add-counter card :power 1))}
                                {:cost [:click 1]
                                 :msg "remove one power counter"
                                 :label "Remove 1 power counter"
                                 :effect (effect (add-counter card :power -1))}]
                    :strength-bonus (req (get-counters card :power))
                    :constant-effects [{:type :used-mu
                                        :duration :constant
                                        :value (req (get-counters card :power))}]}))

(defcard "Reaver"
  {:events [{:event :runner-trash
             :async true
             :interactive (req true)
             :once-per-instance true
             :req (req (and (some #(installed? (:card %)) targets)
                            (first-installed-trash? state side)))
             :msg "draw 1 card"
             :effect (effect (draw :runner eid 1 nil))}]})

(defcard "Refractor"
  (auto-icebreaker {:implementation "Stealth credit restriction not enforced"
                    :abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 1 3 :end-of-encounter {:label "add 3 strength (using at least 1 stealth [Credits])" :cost-req (min-stealth 1)})]}))

(defcard "Rezeki"
  {:events [{:event :runner-turn-begins
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "RNG Key"
  {:events [{:event :pre-access-card
             :req (req (get-in card [:special :rng-guess]))
             :async true
             :msg (msg "reveal " (:title target))
             :effect (req (wait-for
                            (reveal state side target)
                            (continue-ability
                              state side
                              (let [guess (get-in card [:special :rng-guess])]
                                (when (or (= guess (:cost target))
                                          (= guess (get-advancement-requirement target)))
                                  {:prompt "Choose RNG Key reward"
                                   :choices ["Gain 3 [Credits]" "Draw 2 cards"]
                                   :async true
                                   :msg (msg (if (= target "Draw 2 cards")
                                               "draw 2 cards"
                                               "gain 3 [Credits]"))
                                   :effect (req (if (= target "Draw 2 cards")
                                                  (draw state :runner eid 2 nil)
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
                :prompt "Fire RNG Key?"
                :autoresolve (get-autoresolve :auto-fire)
                :yes-ability {:prompt "Guess a number"
                              :choices {:number (req (highest-cost state card))}
                              :msg (msg "guess " target)
                              :effect (effect (update! (assoc-in card [:special :rng-guess] target)))}}})]
   :abilities [(set-autoresolve :auto-fire "RNG Key")]})

(defcard "Rook"
  {:abilities [{:cost [:click 1]
                :label "move to another ice"
                :async true
                :effect (req (let [r (get-card state card)
                                   hosted? (ice? (:host r))
                                   icepos (card-index state (get-card state (:host r)))]
                               (continue-ability
                                 state side
                                 {:prompt (if hosted?
                                            (msg "Host Rook on a piece of ice protecting this server or at position "
                                                 icepos " of a different server")
                                            (msg "Host Rook on a piece of ice protecting any server"))
                                  :choices {:card #(if hosted?
                                                     (and (or (= (get-zone %) (get-zone (:host r)))
                                                              (= (card-index state %) icepos))
                                                          (= (last (get-zone %)) :ices)
                                                          (ice? %)
                                                          (can-host? %)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %)))
                                                     (and (ice? %)
                                                          (can-host? %)
                                                          (= (last (get-zone %)) :ices)
                                                          (not-any? (fn [c] (has-subtype? c "Caïssa")) (:hosted %))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))}
                                 card nil)))}]
   :constant-effects [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (= (get-zone (:host card)) (get-zone target))))
                       :value 2}]})

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
  {:abilities [{:cost [:credit 2]
                :label "install a program"
                :once :per-turn
                :req (req (not (install-locked? state side)))
                :msg (msg "install " (:title target))
                :prompt "Choose a program to install from your grip"
                :choices {:card #(and (program? %)
                                      (in-hand? %))}
                :async true
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]})

(defcard "Scheherazade"
  {:abilities [{:label "Install and host a program from Grip"
                :async true
                :cost [:click 1]
                :keep-open :while-clicks-left
                :prompt "Choose a program to install on Scheherazade from your grip"
                :choices {:req (req (and (program? target)
                                      (runner-can-install? state side target false)
                                      (in-hand? target)))}
                :msg (msg "host " (:title target) " and gain 1 [Credits]")
                :effect (req (wait-for (gain-credits state side 1)
                                       (runner-install state side
                                                       (assoc eid :source card :source-type :runner-install)
                                                       target {:host-card card})))}
               {:label "Host an installed program"
                :prompt "Choose a program to host on Scheherazade"
                :choices {:card #(and (program? %)
                                      (installed? %))}
                :msg (msg "host " (:title target) " and gain 1 [Credits]")
                :async true
                :effect (req (if (host state side card target)
                               (gain-credits state side eid 1)
                               (effect-completed state side eid)))}]})

(defcard "Self-modifying Code"
  {:abilities [{:req (req (not (install-locked? state side)))
                :label "install a program"
                :cost [:trash :credit 2]
                :async true
                :effect (effect (continue-ability
                                  {:prompt "Choose a program to install"
                                   :msg (req (if (not= target "No install")
                                               (str "install " (:title target))
                                               (str "shuffle their Stack")))
                                   :choices (req (conj (filter #(can-pay? state side
                                                                          (assoc eid :source card :source-type :runner-install)
                                                                          % nil [:credit (install-cost state side %)])
                                                               (vec (sort-by :title (filter program? (:deck runner)))))
                                                       "No install"))
                                   :async true
                                   :effect (req (trigger-event state side :searched-stack nil)
                                                (shuffle! state side :deck)
                                                (if (not= target "No install")
                                                  (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)
                                                  (effect-completed state side eid)))}
                                  card nil))}]})

(defcard "Sharpshooter"
  (auto-icebreaker {:abilities [(break-sub [:trash] 0 "Destroyer")
                                (strength-pump 1 2)]}))

(defcard "Shiv"
  (break-and-enter "Sentry"))

(defcard "Sneakdoor Beta"
  {:abilities [{:cost [:click 1]
                :msg "make a run on Archives"
                :makes-run true
                :async true
                :effect (effect (register-events
                                  card
                                  [{:event :pre-successful-run
                                    :duration :end-of-run
                                    :unregister-once-resolved true
                                    :interactive (req true)
                                    :req (req (= :archives (-> run :server first)))
                                    :effect (req (swap! state assoc-in [:run :server] [:hq])
                                                 (trigger-event state :corp :no-action)
                                                 (system-msg state side (str "uses Sneakdoor Beta to make a successful run on HQ")))}])
                                (make-run eid :archives (get-card state card)))}]})

(defcard "Snitch"
  {:events [{:event :approach-ice
             :optional
             {:req (req (not (rezzed? (:ice context))))
              :prompt "Use Snitch to expose approached ice?"
              :yes-ability
              {:async true
               :effect (req (wait-for
                              (expose state side (:ice context))
                              (continue-ability
                                state side
                                {:optional
                                 {:prompt "Jack out?"
                                  :yes-ability {:msg "jack out"
                                                :async true
                                                :effect (effect (jack-out eid))}
                                  :no-ability {:msg "continue the run"}}}
                                card nil)))}}}]})

(defcard "Snowball"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier"
                                           {:additional-ability {:msg "gain +1 strength for the remainder of the run"
                                                                 :effect (effect (pump card 1 :end-of-run))}})
                                (strength-pump 1 1)]}))

(defcard "Spike"
  (break-and-enter "Barrier"))

(defcard "Stargate"
  {:abilities [{:cost [:click 1]
                :once :per-turn
                :msg "make a run on R&D"
                :makes-run true
                :async true
                :effect (effect (make-run eid :rd card))}]
   :events [(successful-run-replace-access
              {:target-server :rd
               :this-card-run true
               :mandatory true
               :ability
               {:async true
                :msg (msg "reveal " (->> (:deck corp)
                                         (take 3)
                                         (map :title)
                                         (string/join ", ")))
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
                                  :effect (effect (trash :runner eid (assoc target :seen true) nil))}
                                 card nil)))}})]})

(defcard "Study Guide"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                {:cost [:credit 2]
                                 :msg "place 1 power counter"
                                 :effect (effect (add-counter card :power 1))}]
                    :strength-bonus (req (get-counters card :power))}))

(defcard "Sūnya"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")]
                    :strength-bonus (req (get-counters card :power))
                    :events [{:event :end-of-encounter
                              :req (req (all-subs-broken-by-card? (:ice context) card))
                              :msg "place 1 power counter on it"
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
                            (swap! state assoc-in [:run :position] (inc tgtndx))
                            (swap-ice state side cice target)))})]
    {:abilities [{:cost [:credit 2]
                  :msg "swap a piece of Barrier ice"
                  :req (req (and run
                                 (= :encounter-ice (:phase run))
                                 (rezzed? current-ice)
                                 (has-subtype? current-ice "Barrier")))
                  :label "Swap the piece of Barrier ice currently being encountered with a piece of ice directly before or after it"
                  :async true
                  :effect (effect (continue-ability (surf state current-ice) card nil))}]}))

(defcard "Surveillance Network Key"
  {:implementation "Only implemented for click to draw"
   :events [{:event :corp-click-draw
             :msg (msg "reveal the card just drawn: " (:title target))}]})

(defcard "Switchblade"
  (auto-icebreaker {:implementation "Stealth credit restriction not enforced"
                    :abilities [(break-sub 1 0 "Sentry" {:label "break any number of Sentry subroutines (using 1 stealth [Credits])"})
                                (strength-pump 1 7 :end-of-encounter {:label "add 7 strength (using 1 stealth [Credits])" :cost-req (min-stealth 1)})]}))

(defcard "Takobi"
  {:events [{:event :subroutines-broken
             :optional {:req (req (all-subs-broken? target))
                        :prompt "Place 1 power counter on Takobi?"
                        :yes-ability
                        {:msg "add 1 power counter to Takobi"
                         :effect (effect (add-counter card :power 1))}}}]
   :abilities [{:req (req (and run
                               (rezzed? current-ice)
                               (= :encounter-ice (:phase run))))
                :cost [:power 2]
                :label "Give non-AI icebreaker +3 strength"
                :prompt "Choose an installed non-AI icebreaker"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                :msg (msg "add +3 strength to " (:title target))
                :effect (effect (pump target 3))}]})

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
               :effect (effect (trash eid card {:cause :purge}))}]}))

(defcard "Torch"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "Tracker"
  (let [ability {:prompt "Choose a server for Tracker"
                 :choices (req servers)
                 :msg (msg "target " target)
                 :req (req (not (:server-target card)))
                 :effect (effect (update! (assoc card :server-target target)))}]
    {:implemention "Doesn't preveent subroutine from resolving"
     :abilities [{:label "Make a run on targeted server"
                  :cost [:click 1 :credit 2]
                  :req (req (some #(= (:server-target card) %) runnable-servers))
                  :msg (msg "make a run on " (:server-target card) ". Prevent the first subroutine that would resolve from resolving")
                  :async true
                  :effect (effect (make-run eid (:server-target card) card))}]
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-turn-ends
               :effect (effect (update! (dissoc card :server-target)))}]}))

(defcard "Tranquilizer"
  (let [action (req (add-counter state side card :virus 1)
                    (if (and (rezzed? (get-card state (:host card)))
                             (<= 3 (get-virus-counters state (get-card state card))))
                      (derez state side (get-card state (:host card)))))]
    {:hosting {:card #(and (ice? %)
                           (can-host? %))}
     :on-install {:effect action}
     :events [{:event :runner-turn-begins
               :effect action}]}))

(defcard "Trope"
  {:events [{:event :runner-turn-begins
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:req (req (not (zone-locked? state :runner :discard)))
                :label "shuffle cards from heap into stack"
                :async true
                :effect
                (effect
                  (continue-ability
                    {:cost [:click 1 :remove-from-game]
                     :label "Reshuffle cards from Heap back into Stack"
                     :show-discard true
                     :choices {:max (min (get-counters card :power) (count (:discard runner)))
                               :all true
                               :card #(and (runner? %)
                                           (in-discard? %))}
                     :msg (msg "shuffle " (string/join ", " (map :title targets))
                               " into their Stack")
                     :effect (req (doseq [c targets] (move state side c :deck))
                                  (shuffle! state side :deck))}
                    card nil))}]})

(defcard "Trypano"
  (let [trash-if-5 (req (let [h (get-card state (:host card))]
                          (if (and h
                                   (>= (get-virus-counters state card) 5)
                                   (not (and (card-flag? h :untrashable-while-rezzed true)
                                             (rezzed? h))))
                            (do (system-msg state :runner (str "uses Trypano to trash " (card-str state h)))
                                (unregister-events state side card)
                                (trash state :runner eid h nil))
                            (effect-completed state side eid))))]
    {:hosting {:card #(and (ice? %)
                           (can-host? %))}
     :on-install {:async true
                  :effect trash-if-5}
     :abilities [(set-autoresolve :auto-accept "add virus counter to Trypano")]
     :events [{:event :runner-turn-begins
               :optional
               {:prompt "Place a virus counter on Trypano?"
                :autoresolve (get-autoresolve :auto-accept)
                :yes-ability {:effect (req (system-msg state :runner "places a virus counter on Trypano")
                                           (add-counter state side card :virus 1))}}}
              {:event :counter-added
               :async true
               :effect trash-if-5}
              {:event :card-moved
               :async true
               :effect trash-if-5}
              {:event :runner-install
               :async true
               :effect trash-if-5}]}))

(defcard "Tycoon"
  (auto-icebreaker {:abilities [(break-sub 1 2 "Barrier")
                                (strength-pump 2 3)]
                    :events [{:event :end-of-encounter
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :msg "give the Corp 2 [Credits]"
                              :async true
                              :effect (effect (gain-credits :corp eid 2))}]}))

(defcard "Unity"
  {:abilities [(break-sub 1 1 "Code Gate")
               {:label "1 [Credits]: Add 1 strength for each installed icebreaker"
                :async true
                :effect (effect (continue-ability
                                  (strength-pump
                                    1
                                    (count (filter #(and (program? %)
                                                         (has-subtype? % "Icebreaker"))
                                                   (all-active-installed state :runner))))
                                  card nil))}]})

(defcard "Upya"
  {:implementation "Power counters added automatically"
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :rd (target-server context)))
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:cost [:click 1 :power 3]
                :once :per-turn
                :msg "gain [Click][Click]"
                :effect (effect (gain :click 2))}]})

(defcard "Utae"
  (let [break-req (:break-req (break-sub 1 1 "Code Gate"))]
    (auto-icebreaker {:abilities [{:label "Break X Code Gate subroutines"
                                   :cost [:x-credits]
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
            {:prompt "Choose a subtype"
             :choices ["Barrier" "Code Gate" "Sentry"]
             :async true
             :effect (req (wait-for (trash state side card {:unpreventable true})
                                    (continue-ability state side
                                                      (expose-and-maybe-bounce target)
                                                      card nil)))})
          (expose-and-maybe-bounce [chosen-subtype]
            {:choices {:card #(and (ice? %)
                                   (not (rezzed? %)))}
             :async true
             :msg (str "name " chosen-subtype)
             :effect (req (wait-for (expose state side target)
                                    (if (has-subtype? async-result chosen-subtype)
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

(defcard "Wyrm"
  (auto-icebreaker {:abilities [(break-sub 3 1 "All" {:label "break 1 subroutine on a piece of ice with 0 or less strength"
                                                      :req (req (not (pos? (get-strength current-ice))))})
                                {:cost [:credit 1]
                                 :label "Give -1 strength to current piece of ice"
                                 :req (req (rezzed? current-ice))
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
