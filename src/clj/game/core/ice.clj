(ns game.core.ice
  (:require
    [game.core.board :refer [all-active-installed all-installed]]
    [game.core.card :refer [get-card ice? installed? rezzed? has-subtype?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [break-sub-ability-cost]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid make-result]]
    [game.core.effects :refer [any-effects get-effects register-lingering-effect sum-effects]]
    [game.core.engine :refer [ability-as-handler pay resolve-ability trigger-event trigger-event-simult queue-event checkpoint]]
    [game.core.flags :refer [card-flag?]]
    [game.core.payment :refer [build-cost-label can-pay? merge-costs]]
    [game.core.say :refer [system-msg]]
    [game.core.update :refer [update!]]
    [game.macros :refer [req effect msg continue-ability wait-for]]
    [game.utils :refer [same-card? pluralize quantify remove-once]]
    [jinteki.utils :refer [make-label]]
    [clojure.string :as string]
    [clojure.set :as set]))

;; These should be in runs.clj, but `req` needs get-current-ice and
;; moving.clj needs set-current-ice
(defn get-run-ices
  [state]
  (when-let [run (:run @state)]
    (get-in @state (concat [:corp :servers] (:server run) [:ices]))))

(defn get-current-ice
  [state]
  (let [ice (get-in @state [:run :current-ice])]
    (or (get-card state (-> @state :encounters peek :ice))
        (get-card state ice)
        ice)))

(defn get-current-encounter
  [state]
  (peek (:encounters @state)))

(defn update-current-encounter
  [state key value]
  (when-let [encounter (get-current-encounter state)]
    (let [updated-encounter (assoc encounter key value)]
      (swap! state update :encounters #(conj (pop %) updated-encounter)))))

(defn set-current-ice
  ([state]
   (when (:run @state)
     (let [run-ice (get-run-ices state)
           pos (get-in @state [:run :position])]
       (when (and pos
                  (pos? pos)
                  (<= pos (count run-ice)))
         (set-current-ice state (nth run-ice (dec pos)))))))
  ([state card]
   (when (:run @state)
     (swap! state assoc-in [:run :current-ice] (get-card state card)))))

(defn active-ice?
  "Ice is active when installed and rezzed or is the current encounter"
  ([state] (active-ice? state (get-current-ice state)))
  ([state ice]
   (let [encounter (peek (:encounters @state))
         encounter-ice (get-card state (:ice encounter))]
     (if (installed? ice)
       (rezzed? ice)
       (same-card? ice encounter-ice)))))

;;; Ice subroutine functions
(defn add-sub
  ([ice sub] (add-sub ice sub (:cid ice) nil))
  ([ice sub cid] (add-sub ice sub cid nil))
  ([ice sub cid {:keys [front back printed variable]}]
   (let [curr-subs (:subroutines ice [])
         position (cond
                    back 1
                    front -1
                    :else 0)
         new-sub {:label (make-label sub)
                  :from-cid cid
                  :sub-effect (if (:sub-effect sub)
                                (:sub-effect sub)
                                (dissoc sub :breakable))
                  :position position
                  :variable (or variable false)
                  :printed (or printed false)
                  :breakable (:breakable sub true)}
         new-subs (->> (conj curr-subs new-sub)
                       (sort-by :position)
                       (map-indexed (fn [idx sub] (assoc sub :index idx)))
                       (into []))]
     (assoc ice :subroutines new-subs))))

(defn add-sub!
  ([state side ice sub] (add-sub! state side ice sub (:cid ice) nil))
  ([state side ice sub cid] (add-sub! state side ice sub cid nil))
  ([state side ice sub cid args]
   (update! state :corp (add-sub ice sub cid args))
   (trigger-event state side :subroutines-changed (get-card state ice))))

(defn remove-sub
  "Removes a single sub from a piece of ice for pred. By default removes the first subroutine
  with the same cid as the given ice."
  ([ice] (remove-sub ice #(= (:cid ice) (:from-cid %))))
  ([ice pred]
   (let [new-subs (->> (:subroutines ice)
                       (remove-once pred)
                       (map-indexed (fn [idx sub] (assoc sub :index idx)))
                       (into []))]
     (assoc ice :subroutines new-subs))))

(defn remove-sub!
  ([state side ice] (remove-sub! state side ice #(= (:cid ice) (:from-cid %))))
  ([state side ice pred]
   (update! state :corp (remove-sub ice pred))
   (trigger-event state side :subroutines-changed (get-card state ice))))

(defn remove-subs
  "Removes all subs from a piece of ice for pred. By default removes the subroutines
  with the same cid as the given ice."
  ([ice] (remove-sub ice #(= (:cid ice) (:from-cid %))))
  ([ice pred]
   (let [new-subs (->> (:subroutines ice)
                       (remove pred)
                       (map-indexed (fn [idx sub] (assoc sub :index idx)))
                       (into []))]
     (assoc ice :subroutines new-subs))))

(defn remove-subs!
  ([state side ice] (remove-subs! state side ice #(= (:cid ice) (:from-cid %))))
  ([state side ice pred]
   (update! state :corp (remove-subs ice pred))
   (trigger-event state side :subroutines-changed (get-card state ice))))

(defn add-extra-sub!
  "Add a run time subroutine to a piece of ice (Warden, Sub Boost, etc)"
  ([state side ice sub] (add-extra-sub! state side ice sub (:cid ice) {:back true}))
  ([state side ice sub cid] (add-extra-sub! state side ice sub cid {:back true}))
  ([state side ice sub cid args]
   (add-sub! state side (assoc-in ice [:special :extra-subs] true) sub cid args)))

(defn remove-extra-subs!
  "Remove runtime subroutines assigned from the given cid from a piece of ice."
  [state side ice cid]
  (let [curr-subs (:subroutines ice)
        new-subs (remove #(= cid (:from-cid %)) curr-subs)
        extra-subs (some #(= (:cid ice) (:from-cid %)) new-subs)]
    (update! state :corp
             (-> ice
                 (assoc :subroutines (vec new-subs))
                 (assoc-in [:special :extra-subs] extra-subs)))
    (trigger-event state side :subroutines-changed (get-card state ice))))

(defn break-subroutine
  "Marks a given subroutine as broken"
  ([ice sub] (break-subroutine ice sub nil))
  ([ice sub breaker]
   (assoc ice :subroutines (assoc (:subroutines ice) (:index sub) (if breaker
                                                                    (assoc sub :broken true :breaker (:cid breaker))
                                                                    (assoc sub :broken true))))))

(defn break-subroutine!
  "Marks a given subroutine as broken, update!s state"
  ([state ice sub] (break-subroutine! state ice sub nil))
  ([state ice sub breaker]
   (update! state :corp (break-subroutine ice sub breaker))))

(defn break-all-subroutines
  ([ice]
   (break-all-subroutines ice nil))
  ([ice breaker]
   (if breaker
     (reduce #(break-subroutine %1 %2 breaker) ice (:subroutines ice))
     (reduce break-subroutine ice (:subroutines ice)))))

(defn break-all-subroutines!
  ([state ice] (break-all-subroutines! state ice nil))
  ([state ice breaker]
   (update! state :corp (break-all-subroutines ice breaker))))

(defn any-subs-broken?
  [ice]
  (some :broken (:subroutines ice)))

(defn all-subs-broken?
  [ice]
  (let [subroutines (:subroutines ice)]
    (and (seq subroutines)
         (every? :broken subroutines))))

(defn any-subs-broken-by-card?
  [ice card]
  (some #(and (:broken %)
              (= (:cid card) (:breaker %)))
        (:subroutines ice)))

(defn all-subs-broken-by-card?
  [ice card]
  (let [subroutines (:subroutines ice)]
    (and (seq subroutines)
         (every? #(and (:broken %)
                       (= (:cid card) (:breaker %)))
                 subroutines))))

(defn dont-resolve-subroutine
  "Marks a given subroutine as not resolving (e.g. Mass-Driver)"
  [ice sub]
  (assoc ice :subroutines (assoc (:subroutines ice) (:index sub) (assoc sub :resolve false))))

(defn dont-resolve-subroutine!
  "Marks a given subroutine as not resolving (e.g. Mass-Driver), update!s state"
  [state ice sub]
  (update! state :corp (dont-resolve-subroutine ice sub)))

(defn dont-resolve-all-subroutines
  [ice]
  (reduce dont-resolve-subroutine ice (:subroutines ice)))

(defn dont-resolve-all-subroutines!
  [state ice]
  (update! state :corp (dont-resolve-all-subroutines ice)))

(defn reset-all-subs
  "Mark all broken/fired subroutines as unbroken/unfired"
  [ice]
  (letfn [(reset-sub [sub] (dissoc sub :broken :fired :resolve))]
    (update ice :subroutines #(into [] (map reset-sub %)))))

(defn reset-all-subs!
  "Marks all broken subroutines as unbroken, update!s state"
  [state ice]
  (update! state :corp (reset-all-subs ice)))

(defn reset-all-ice
  [state _]
  (doseq [ice (filter ice? (all-installed state :corp))]
    (reset-all-subs! state ice)))

(defn unbroken-subroutines-choice
  "Takes an ice, returns the unbroken subroutines for a choices prompt"
  [ice]
  (for [sub (remove #(or (:broken %) (not (:resolve % true))) (:subroutines ice))]
    (make-label (:sub-effect sub))))

(defn breakable-subroutines-choice
  "Takes an ice, returns the breakable subroutines for a choices prompt"
  [state side eid card ice]
  (when-not (any-effects state side :cannot-break-subs-on-ice true? {:ice ice
                                                                     :icebreaker card})
    (for [sub (remove #(or (:broken %)
                           (not (if (fn? (:breakable %))
                                  ((:breakable %) state side eid ice [card])
                                  (:breakable % true))))
                      (:subroutines ice))]
      (make-label (:sub-effect sub)))))

(defn resolve-subroutine
  [ice sub]
  (assoc ice :subroutines (assoc (:subroutines ice) (:index sub) (assoc sub :fired true))))

(defn resolve-subroutine!
  ([state side ice sub]
   (let [eid (make-eid state {:source ice
                              :source-type :subroutine})]
     (resolve-subroutine! state side eid ice sub)))
  ([state side eid ice sub]
   (wait-for (trigger-event-simult state side (make-eid state eid) :pre-resolve-subroutine nil sub ice)
             ;; this is for cards like marcus batty
             (when-not (:exernal-trigger sub)
               (update! state :corp (resolve-subroutine ice sub)))
             ;; TODO - need a way to interact with multiple replacement effects.
             (let [replacement (:replace-subroutine (get-current-encounter state))
                   sub (or (when replacement (assoc replacement :index (:index sub))) sub)
                   prevent (:prevent-subroutine (get-current-encounter state))]
               (update-current-encounter state :replace-subroutine nil)
               (update-current-encounter state :prevent-subroutine nil)
               (if prevent
                 (checkpoint state nil eid)
                 ;; see if there are any effects which can prevent this subroutine
                 (wait-for (resolve-ability state side (make-eid state eid) (:sub-effect sub) (get-card state ice) nil)
                           (queue-event state :subroutine-fired {:sub sub :ice ice})
                           (checkpoint state nil eid)))))))

(defn- resolve-next-unbroken-sub
  ([state side ice subroutines]
   (let [eid (make-eid state {:source ice
                              :source-type :subroutine})]
     (resolve-next-unbroken-sub state side eid ice subroutines nil)))
  ([state side eid ice subroutines] (resolve-next-unbroken-sub state side eid ice subroutines nil))
  ([state side eid ice subroutines msgs]
   (if (and (seq subroutines)
            (or (:run @state)
                (peek (:encounters @state)))
            (active-ice? state ice)
            (not (get-in @state [:end-run :ended])))
     (let [sub (first subroutines)]
       (wait-for (resolve-subroutine! state side (make-eid state eid) ice sub)
                 (resolve-next-unbroken-sub state side eid
                                            (get-card state ice)
                                            (rest subroutines)
                                            (cons sub msgs))))
     (effect-completed state side (make-result eid (reverse msgs))))))

(defn resolve-unbroken-subs!
  ([state side ice]
   (let [eid (make-eid state {:source ice
                              :source-type :subroutine})]
     (resolve-unbroken-subs! state side eid ice)))
  ([state side eid ice]
   (if-let [subroutines (seq (remove #(or (:broken %) (= false (:resolve %))) (:subroutines ice)))]
     (wait-for (resolve-next-unbroken-sub state side (make-eid state eid) ice subroutines)
               (system-msg state :corp (str "resolves " (quantify (count async-result) "unbroken subroutine")
                                            " on " (:title ice)
                                            " (\"[subroutine] "
                                            (string/join "\" and \"[subroutine] "
                                                  (map :label (sort-by :index async-result)))
                                            "\")"))
               (effect-completed state side eid))
     (effect-completed state side eid))))

;;; Ice strength functions
(defn get-strength
  [card]
  (when (or (ice? card)
            (has-subtype? card "Icebreaker"))
    (or (:current-strength card)
        (:strength card)
        0)))

(defn get-pump-strength
  ([state side ability card] (get-pump-strength state side ability card nil))
  ([state side ability card targets]
   ((fnil + 0 0)
    (:pump ability)
    (when-let [pump-fn (:pump-bonus ability)]
      (pump-fn state side (make-eid state) card targets)))))

(defn ice-strength-bonus
  "Use in :static-abilities vectors to give the current ice or program a conditional strength bonus"
  ([bonus]
   {:type :ice-strength
    :req (req (same-card? card target))
    :value bonus})
  ([req-fn bonus]
   {:type :ice-strength
    :req (req (and (same-card? card target)
                   (req-fn state side eid card targets)))
    :value bonus}))

(defn sum-ice-strength-effects
  "Sums the results from get-effects."
  [state side ice]
  (let [can-lower? (not (any-effects state side :cannot-lower-strength true? {:ice ice}))]
    (->> (get-effects state side :ice-strength ice)
         (filter #(and (number? %) (or can-lower? (pos? %))))
         (reduce +))))

(defn ice-strength
  "Gets the modified strength of the given ice."
  [state side {:keys [strength] :as ice}]
  (when (ice? ice)
    (->> [strength
          (when-let [strfun (:strength-bonus (card-def ice))]
            (strfun state side nil ice nil))
          (sum-ice-strength-effects state side ice)]
         (reduce (fnil + 0 0)))))

(defn update-ice-strength
  "Updates the given ice's strength by triggering strength events and updating the card."
  [state side ice]
  (let [ice (get-card state ice)
        old-strength (get-strength ice)
        new-strength (ice-strength state side ice)
        changed? (not= old-strength new-strength)]
    (when (active-ice? state ice)
      (update! state side (assoc ice :current-strength new-strength))
      (trigger-event state side :ice-strength-changed (get-card state ice) old-strength)
      changed?)))

(defn update-ice-in-server
  "Updates all ice in the given server's :ices field."
  [state side server]
  (reduce (fn [changed? ice]
            (or (update-ice-strength state side ice)
                changed?))
          false
          (:ices server)))

(defn update-all-ice
  "Updates all installed ice."
  [state side]
  (reduce (fn [changed? server]
            (or (update-ice-in-server state side (second server))
                changed?))
          false
          (get-in @state [:corp :servers])))

(defn pump-ice
  "Change a piece of ice's strength by n for the given duration of :end-of-encounter, :end-of-run or :end-of-turn"
  ([state side card n] (pump-ice state side card n :end-of-encounter))
  ([state side card n duration]
   (register-lingering-effect
     state side card
     {:type :ice-strength
      :duration duration
      :req (req (same-card? card target))
      :value n})
   (update-ice-strength state side (get-card state card))))

(defn pump-all-ice
  ([state side n] (pump-all-ice state side n :end-of-encounter))
  ([state side n duration]
   (doseq [ice (filter ice? (all-active-installed state :corp))]
     (pump-ice state side ice n duration))))

;;; Icebreaker functions
(defn breaker-strength
  "Gets the modified strength of the given breaker."
  [state side {:keys [strength] :as card}]
  (when-not (nil? strength)
    (->> [strength
          (when-let [strfun (:strength-bonus (card-def card))]
            (strfun state side (make-eid state) card nil))
          (sum-effects state side :breaker-strength card)]
         (reduce (fnil + 0 0)))))

(defn breaker-strength-bonus
  "Use in :static-abilities vectors to give the current ice or program a conditional strength bonus"
  ([bonus]
   {:type :breaker-strength
    :req (req (same-card? card target))
    :value bonus})
  ([req-fn bonus]
   {:type :breaker-strength
    :req (req (and (same-card? card target)
                   (req-fn state side eid card targets)))
    :value bonus}))

(defn update-breaker-strength
  "Updates a breaker's current strength by triggering updates and applying their effects."
  [state side breaker]
  (let [breaker (get-card state breaker)
        old-strength (get-strength breaker)
        new-strength (breaker-strength state side breaker)
        changed? (not= old-strength new-strength)]
    (update! state side (assoc (get-card state breaker) :current-strength new-strength))
    (trigger-event state side :breaker-strength-changed (get-card state breaker) old-strength)
    changed?))

(defn update-all-icebreakers
  [state side]
  (reduce (fn [changed? icebreaker]
            (or (update-breaker-strength state side icebreaker)
                changed?))
          false
          (filter #(has-subtype? % "Icebreaker")
                  (all-active-installed state :runner))))

(defn pump
  "Change a breaker's strength by n for the given duration of :end-of-encounter, :end-of-run or :end-of-turn"
  ([state side card n] (pump state side card n :end-of-encounter))
  ([state side card n duration]
   (let [floating-effect (register-lingering-effect
                           state side (get-card state card)
                           {:type :breaker-strength
                            :duration duration
                            :req (req (same-card? card target))
                            :value n})]
     (update-breaker-strength state side (get-card state card))
     (trigger-event state side :pump-breaker (get-card state card) floating-effect))))

(defn pump-all-icebreakers
  ([state side n] (pump-all-icebreakers state side n :end-of-encounter))
  ([state side n duration]
   (doseq [icebreaker (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))]
     (pump state side icebreaker n duration))))

;; Break abilities
(defn- break-subroutines-impl
  ([ice target-count] (break-subroutines-impl ice target-count '() nil))
  ([ice target-count broken-subs] (break-subroutines-impl ice target-count broken-subs nil))
  ([ice target-count broken-subs args]
   {:async true
    :prompt (str "Break a subroutine"
                 (when (and target-count (< 1 target-count))
                   (str " (" (count broken-subs)
                        " of " target-count ")")))
    :choices (req (concat (breakable-subroutines-choice state side eid card ice)
                          (when-not (and (:all args)
                                         (pos? (count (breakable-subroutines-choice state side eid card ice)))
                                         (< 1 target-count))
                            '("Done"))))
    :effect (req (if (= "Done" target)
                   (complete-with-result state side eid {:broken-subs broken-subs
                                                         :early-exit true})
                   (let [subroutines (filter #(and (not (:broken %))
                                                   (if (fn? (:breakable %))
                                                     ((:breakable %) state side eid ice [card])
                                                     (:breakable % true)))
                                             (:subroutines ice))
                         idx (:idx (first targets))
                         sub (if (number? idx)
                               (nth subroutines idx)
                               (first (filter #(= target (make-label (:sub-effect %))) subroutines)))
                         ice (break-subroutine ice sub)
                         broken-subs (cons sub broken-subs)
                         breakable-subs (breakable-subroutines-choice state side eid card ice)]
                     (if (and (pos? (count breakable-subs))
                              (< (count broken-subs) (if (pos? target-count) target-count (count (:subroutines ice)))))
                       (continue-ability state side (break-subroutines-impl ice target-count broken-subs args) card nil)
                       (complete-with-result state side eid {:broken-subs broken-subs
                                                             :early-exit (zero? (count breakable-subs))})))))}))

(defn break-subroutines-msg
  ([ice broken-subs breaker] (break-subroutines-msg ice broken-subs breaker nil))
  ([ice broken-subs breaker args]
   (str "use " (:title breaker)
        " to break " (quantify (count broken-subs)
                               (str (when-let [subtypes (:subtype args)]
                                      (when-not (= #{"All"} subtypes)
                                        (-> subtypes
                                            (set/intersection (set (:subtypes ice)))
                                            (first)
                                            (str " "))))
                                    "subroutine"))
        " on " (:title ice)
        " (\"[subroutine] "
        (string/join "\" and \"[subroutine] "
              (map :label (sort-by :index broken-subs)))
        "\")")))

(defn break-subroutines
  ([ice breaker cost n] (break-subroutines ice breaker cost n nil))
  ([ice breaker cost n args]
   (let [args (merge {:repeatable true
                      :all false}
                     args)]
     {:async true
      :effect (req (wait-for
                     (resolve-ability state side (make-eid state eid) (break-subroutines-impl ice (if (zero? n) (count (:subroutines current-ice)) n) '() args) card nil)
                     (let [broken-subs (:broken-subs async-result)
                           early-exit (:early-exit async-result)
                           total-cost (when (seq broken-subs)
                                        (break-sub-ability-cost state side
                                                                (assoc args
                                                                       :break-cost cost
                                                                       :broken-subs broken-subs)
                                                                card ice))
                           message (when (seq broken-subs)
                                     (break-subroutines-msg ice broken-subs breaker args))]
                       (wait-for (pay state side (make-eid state {:source card :source-info {:ability-idx (:ability-idx args) }  :source-type :ability}) card total-cost)
                                 (if-let [payment-str (:msg async-result)]
                                   (do (when (not (string/blank? message))
                                         (system-msg state :runner (str payment-str " to " message)))
                                       (doseq [sub broken-subs]
                                         (break-subroutine! state (get-card state ice) sub breaker)
                                         (resolve-ability state side (make-eid state {:source card :source-type :ability})
                                                          (:additional-ability args)
                                                          card nil))
                                       (let [ice (get-card state ice)
                                             on-break-subs (when ice (:on-break-subs (card-def ice)))
                                             event-args (when on-break-subs {:card-abilities (ability-as-handler ice on-break-subs)})]
                                         (wait-for
                                           (trigger-event-simult state side :subroutines-broken event-args ice broken-subs)
                                           (let [ice (get-card state ice)
                                                 card (get-card state card)]
                                             (if (and ice
                                                      card
                                                      (not early-exit)
                                                      (:repeatable args)
                                                      (seq broken-subs)
                                                      (pos? (count (unbroken-subroutines-choice ice)))
                                                      (can-pay? state side eid (get-card state card) nil cost))
                                               (continue-ability state side (break-subroutines ice breaker cost n args) card nil)
                                               (effect-completed state side eid))))))
                                   (effect-completed state side eid))))))})))

(defn break-sub
  "Creates a break subroutine ability.

  cost: A number (for credits) or a cost vector.
  n: A number of subs to break or a 5-fn that returns a number.
    If n = 0 then any number of subs are broken.
    If n is an fn, it will be called at action execution, not at definition.
  subtypes: A string, a set of strings, or nil.
    A string for a single subtype, a set for multiple, or nil for any (AI).
  args: A map or nil.
    * `:label` can be used to add a non-standard label to the ability.
    * `:additional-ability` is a non-async ability that is called after using the break ability.
    * `:req` will be added to the standard checks for encountering a piece of ice and strengths of the ice and breaker."
  ([cost n] (break-sub cost n nil nil))
  ([cost n subtypes] (break-sub cost n subtypes nil))
  ([cost n subtypes args]
   (let [cost (if (number? cost) [:credit cost] cost)
         subtypes (cond (string? subtypes) #{subtypes}
                        (set? subtypes) subtypes
                        :else #{"All"})
         args (assoc args :subtype subtypes :break n)
         break-req (req (and current-ice
                             (peek (:encounters @state))
                             (active-ice? state current-ice)
                             (or (contains? subtypes "All")
                                 (some #(has-subtype? current-ice %) subtypes))
                             (pos? (count (breakable-subroutines-choice state side eid card current-ice)))
                             (if (:req args)
                               ((:req args) state side eid card targets)
                               true)))
         strength-req (req (if (has-subtype? card "Icebreaker")
                             (<= (get-strength current-ice) (get-strength card))
                             true))]
     (merge
       (when (some #(= :trash-can (first %)) (merge-costs cost))
         {:trash-icon true})
       {:async true
        :req (req (and (break-req state side eid card targets)
                       (strength-req state side eid card targets)))
        :break-req break-req
        :break n
        :breaks subtypes
        :break-cost cost
        :auto-break-sort (:auto-break-sort args)
        :cost-req (:cost-req args)
        :break-cost-bonus (:break-cost-bonus args)
        :additional-ability (:additional-ability args)
        :label (str (or (:label args)
                        (str "break "
                             (when (< 1 n) "up to ")
                             (if (pos? n) n "any number of")
                             (when-not (= #{"All"} subtypes)
                               (str " " (string/join " or " (sort subtypes))))
                             (pluralize " subroutine" n))))
        :effect (effect (continue-ability
                          (let [n (if (fn? n)
                                    (n state side (assoc eid :source-type :ability) card nil)
                                    n)]
                            (when (can-pay? state side
                                            (assoc eid :source-type :ability)
                                            card nil
                                            (break-sub-ability-cost
                                              state side
                                              (assoc args :break-cost cost :broken-subs (take n (:subroutines current-ice)))
                                              card current-ice))
                              (break-subroutines current-ice card cost n (assoc args :ability-idx (:ability-idx (:source-info eid))))))
                          card nil))}))))

(defn strength-pump
  "Creates a strength pump ability.
  Cost can be a credit amount or a list of costs e.g. [:credit 2]."
  ([cost strength] (strength-pump cost strength :end-of-encounter nil))
  ([cost strength duration] (strength-pump cost strength duration nil))
  ([cost strength duration args]
   (let [cost (if (number? cost) [:credit cost] cost)
         duration-string (cond
                           (= duration :end-of-run)
                           " for the remainder of the run"
                           (= duration :end-of-turn)
                           " for the remainder of the turn")]
     {:label (str (or (:label args)
                      (str "add " strength " strength"
                           duration-string)))
      :req (req (if-let [str-req (:req args)]
                  (str-req state side eid card targets)
                  true))
      :cost cost
      :cost-req (:cost-req args)
      :pump strength
      :pump-bonus (:pump-bonus args)
      :auto-pump-sort (:auto-break-sort args)
      :msg (msg "increase its strength from " (get-strength card)
                " to " (+ (get-pump-strength
                            state side
                            (assoc args :pump strength)
                            card)
                          (get-strength card))
                duration-string)
      :effect (effect (pump card
                            (get-pump-strength
                              state side
                              (assoc args :pump strength)
                              card)
                            duration))})))


(def breaker-auto-pump
  "Updates an icebreaker's abilities with a pseudo-ability to trigger the
  auto-pump routine in core, IF we are encountering a rezzed ice with a subtype
  we can break."
  {:silent (req true)
   :effect
   (req (let [abs (remove #(or (= (:dynamic %) :auto-pump)
                               (= (:dynamic %) :auto-pump-and-break))
                          (:abilities card))
              current-ice (get-card state current-ice)
              ;; match strength
              can-pump (fn [ability]
                         (when (:pump ability)
                           ((:req ability) state side eid card nil)))
              pump-ability (some #(when (can-pump %) %) (:abilities (card-def card)))
              pump-strength (get-pump-strength state side pump-ability card)
              strength-diff (when (and current-ice
                                       (get-strength current-ice)
                                       (get-strength card))
                              (max 0 (- (get-strength current-ice)
                                        (get-strength card))))
              times-pump (if (and strength-diff
                                  (pos? pump-strength))
                           (int (Math/ceil (/ strength-diff pump-strength)))
                           0)
              total-pump-cost (when (and pump-ability
                                         times-pump)
                                (repeat times-pump (:cost pump-ability)))
              ;; break all subs
              can-break (fn [ability]
                          (when (:break-req ability)
                            ((:break-req ability) state side eid card nil)))
              break-ability (some #(when (can-break %) %) (:abilities (card-def card)))
              break-cost (break-sub-ability-cost state side break-ability card current-ice)
              subs-broken-at-once (when break-ability
                                    (:break break-ability 1))
              unbroken-subs (count (remove :broken (:subroutines current-ice)))
              no-unbreakable-subs (empty? (filter #(if (fn? (:breakable %)) ; filter for possibly unbreakable subs
                                                     (not= :unrestricted ((:breakable %) state side eid current-ice [card]))
                                                     (not (:breakable % true))) ; breakable is a bool
                                                  (:subroutines current-ice)))
              times-break (when (and (pos? unbroken-subs)
                                     subs-broken-at-once)
                            (if (pos? subs-broken-at-once)
                              (int (Math/ceil (/ unbroken-subs subs-broken-at-once)))
                              1))
              total-break-cost (when (and break-cost
                                          times-break)
                                 (repeat times-break break-cost))
              total-cost (merge-costs (conj total-pump-cost total-break-cost))]
          (update! state side
                   (assoc card :abilities
                          (if (and (seq total-cost)
                                   (peek (:encounters @state))
                                   (active-ice? state current-ice)
                                   (or break-ability
                                       pump-ability))
                            (vec (concat abs
                                         (when (and break-ability
                                                    (or pump-ability (zero? strength-diff))
                                                    no-unbreakable-subs
                                                    (pos? unbroken-subs)
                                                    (can-pay? state side eid card total-cost))
                                           [{:dynamic :auto-pump-and-break
                                             :cost total-cost
                                             :cost-label (build-cost-label total-cost)
                                             :label (str (if (and pump-ability (pos? times-pump))
                                                           "Match strength and fully break "
                                                           "Fully break ")
                                                         (:title current-ice))}])
                                         (when (and pump-ability
                                                    (pos? times-pump)
                                                    (can-pay? state side eid card total-pump-cost))
                                           [{:dynamic :auto-pump
                                             :cost total-pump-cost
                                             :cost-label (build-cost-label total-pump-cost)
                                             :label (str "Match strength of " (:title current-ice))}])))
                            abs)))))})

;; Takes a a card definition, and returns a new card definition that
;; hooks up breaker-auto-pump to the necessary events.
;; IMPORTANT: Events on cdef take precedence, and should call
;; (:effect breaker-auto-pump) themselves.
(let [events (for [event [:run :approach-ice :encounter-ice :pass-ice :run-ends
                          :ice-strength-changed :ice-subtype-changed :breaker-strength-changed
                          :subroutines-changed]]
               (assoc breaker-auto-pump :event event))]
  (defn auto-icebreaker [cdef]
    (assoc cdef :events (apply conj events (:events cdef)))))
