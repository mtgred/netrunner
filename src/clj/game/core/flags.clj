(in-ns 'game.core)

;;;; Various functions for checking small "flag" values of cards, runs, players, etc.

(defn card-flag?
  "Checks the card to see if it has a :flags entry of the given flag-key, and with the given value if provided"
  ;; TODO: add a register for mutable state card flags, separate from this
  ([card flag-key]
   (let [cdef (card-def card)]
     (some? (get-in cdef [:flags flag-key]))))
  ([card flag-key value]
   (let [cdef (card-def card)]
     (= value (get-in cdef [:flags flag-key])))))

(defn card-flag-fn?
  "Checks the card to see if it has a :flags entry of the given flag-key, whose value is a four-argument
  function that returns the given value"
  ([state side card flag-key]
   (let [cdef (card-def card)
         func (get-in cdef [:flags flag-key])]
     (func state side (make-eid state) card nil)))
  ([state side card flag-key value]
   (let [cdef (card-def card)
         func (get-in cdef [:flags flag-key])]
     (and func (= (func state side (make-eid state) card nil) value)))))

(defn any-flag-fn?
  "Checks `card-flag-fn? on all installed cards on specified side for the value with the flag-key
  Default value of `cards` is `(all-active state side)`"
  ([state side flag-key value]
    (any-flag-fn? state side flag-key value (all-active state side)))
  ([state side flag-key value cards]
   (some #(card-flag-fn? state side % flag-key value) cards)))

;;; Generic flag functions
(defn- register-flag!
  "Register a flag of the specific type."
  [state side card flag-type flag condition]
  (swap! state update-in [:stack flag-type flag] #(conj % {:card card :condition condition})))

(defn- check-flag?
  "Flag condition will ask for permission to do something, e.g. :can-rez, :can-advance
  If allowed, return true, if not allowed, return false. Therefore check for any false conditions.
  Returns true if no flags are present."
  [state side card flag-type flag]
  (let [conditions (get-in @state [:stack flag-type flag])]
    ;; check that every condition returns true
    (every? #((:condition %) state side card) conditions)))

(defn check-flag-types?
  "Checks flag that the specified flag types are permitting the flag"
  [state side card flag flag-types]
  (every? #(check-flag? state side card % flag) flag-types))

(defn get-preventing-cards
  "Returns all cards that are preventing specified flag, checking in the specified flag-types"
  [state side card flag flag-types]
  (let [conditions (mapcat #(get-in @state [:stack % flag]) flag-types)
        predicate (complement #((:condition %) state side card))]
    (map :card (filter predicate conditions))))

(defn has-flag?
  "Checks if the specified flag exists - used for Gene Conditioning Shoppe"
  [state side flag-type flag]
  (not-empty (get-in @state [:stack flag-type flag])))

(defn- clear-all-flags!
  "Clears all flags of specified type"
  [state flag-type]
  (swap! state assoc-in [:stack flag-type] nil))

(defn- clear-flag-for-card!
  "Remove all entries for specified card for flag-type and flag"
  [state side card flag-type flag]
  (swap! state update-in [:stack flag-type flag]
         (fn [flag-map] (remove #(= (get-cid %) (:cid card)) flag-map))))

;; Currently unused
(defn clear-all-flags-for-card!
  "Removes all flags set by the card - of any flag type"
  [state side card]
  (letfn [(clear-flag-type! [flag-type]
            (map #(clear-flag-for-card! state side card flag-type %)
                 (keys (get-in @state [:stack flag-type]))))]
    ;; Only care about the side-effects of this
    (map clear-flag-type! #{:current-run :current-turn :persistent})
    ;; Return the card again
    card))

;;; Run flag - cleared at end of run
(defn register-run-flag!
  "Registers a flag for the current run only. The flag gets cleared in end-run.
  Example: Blackmail flags the inability to rez ice."
  [state side card flag condition]
  (register-flag! state side card :current-run flag condition))

(defn run-flag?
  "Checks if any cards explicitly forbids the flag this run"
  [state side card flag]
  (check-flag? state side card :current-run flag))

(defn clear-run-register!
  "Clears the run-flag register."
  [state]
  (clear-all-flags! state :current-run))

(defn clear-run-flag!
  "Remove any entry associated with card for the given flag"
  [state side card flag]
  (clear-flag-for-card! state side card :current-run flag))

;;; Turn flag - cleared at end of turn
(defn register-turn-flag!
  "As register-run-flag, but for the entire turn."
  [state side card flag condition]
  (register-flag! state side card :current-turn flag condition))

(defn turn-flag?
  "Checks if any cards explicitly forbids the flag this turn"
  [state side card flag]
  (check-flag? state side card :current-turn flag))

(defn clear-turn-register! [state]
  (clear-all-flags! state :current-turn))

(defn clear-turn-flag!
  "Remove any entry associated with card for the given flag"
  [state side card flag]
  (clear-flag-for-card! state side card :current-turn flag))

;;; Persistent flag - has to be cleared manually
(defn register-persistent-flag!
  "A flag that persists until cleared."
  [state side card flag condition]
  (register-flag! state side card :persistent flag condition))

;; Currently unused after Efficiency Committee and Genetics refactor
(defn persistent-flag?
  "Checks if any cards explicitly forbids the flag"
  [state side card flag]
  (check-flag? state side card :persistent flag))

(defn clear-persistent-flag!
  "Remove any entry associated with card for the given flag"
  [state side card flag]
  (clear-flag-for-card! state side card :persistent flag))

;;; Functions related to servers that can be run
(defn prevent-run-on-server
  "Adds specified server to list of servers that cannot be run on.
  The causing card is also specified"
  [state card & servers]
  (doseq [server servers]
    (swap! state assoc-in [:runner :register :cannot-run-on-server server (:cid card)] true)))

(defn enable-run-on-server
  "Removes specified server from list of server for the associated card.
  If other cards are associated with the same server that server will still be unable to be run
  on."
  [state card & servers]
  (doseq [server servers]
    (let [card-map (get-in @state [:runner :register :cannot-run-on-server server])
          reduced-card-map (dissoc card-map (:cid card))]
      (if (empty? reduced-card-map)
        ;; removes server if no cards block it, otherwise updates the map
        (swap! state update-in [:runner :register :cannot-run-on-server] dissoc server)
        (swap! state assoc-in [:runner :register :cannot-run-on-server server]
               reduced-card-map)))))

(defn can-run-server?
  "Returns true if the specified server can be run on. Specified server must be string form."
  [state server]
  (not-any? #{server}
            (map zone->name (keys (get-in @state [:runner :register :cannot-run-on-server])))))


;;; Functions for preventing specific game actions.
;;; TODO: look into migrating these to turn-flags and run-flags.
(defn prevent-draw [state side]
  (swap! state assoc-in [:runner :register :cannot-draw] true))

(defn prevent-jack-out [state side]
  (swap! state assoc-in [:run :cannot-jack-out] true))

(defn prevent-current [state side]
  (swap! state assoc-in [:runner :register :cannot-play-current] true))

(defn lock-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(conj % cid)))

(defn release-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(remove #{cid} %)))

(defn zone-locked?
  [state side zone]
  (seq (get-in @state [side :locked zone])))

(defn untrashable-while-rezzed? [card]
  (and (card-flag? card :untrashable-while-rezzed true) (rezzed? card)))

(defn untrashable-while-resources? [card]
  (and (card-flag? card :untrashable-while-resources true) (installed? card)))

(defn- can-rez-reason
  "Checks if the corp can rez the card.
  Returns true if so, otherwise the reason:
  :side card is not on :corp side
  :run-flag run flag prevents rez
  :turn-flag turn flag prevents rez
  :unique fails unique check
  :req does not meet rez requirement"
  [state side card]
  (let [uniqueness (:uniqueness card)
        rez-req (:rez-req (card-def card))]
    (cond
      ;; Card on same side?
      (not (same-side? side (:side card))) :side
      ;; No flag restrictions?
      (not (run-flag? state side card :can-rez)) :run-flag
      (not (turn-flag? state side card :can-rez)) :turn-flag
      ;; Uniqueness check
      (and uniqueness (some #(and (rezzed? %) (= (:code card) (:code %))) (all-installed state :corp))) :unique
      ;; Rez req check
      (and rez-req (not (rez-req state side (make-eid state) card nil))) :req
      ;; No problems - return true
      :default true)))

(defn can-rez?
  "Checks if the card can be rezzed. Toasts the reason if not."
  ([state side card] (can-rez? state side card nil))
  ([state side card {:keys [ignore-unique] :as args}]
   (let [reason (can-rez-reason state side card)
         reason-toast #(do (toast state side %) false)
         title (:title card)]
     (case reason
       ;; Do nothing special if true
       true true
       ;; No need to toast if on different side
       :side false
       ;; Flag restrictions - toast handled by flag
       :run-flag false
       :turn-flag false
       ;; Uniqueness
       :unique (or ignore-unique
                   (reason-toast (str "Cannot rez a second copy of " title " since it is unique. Please trash the other"
                                      " copy first")))
       ;; Rez requirement
       :req (reason-toast (str "Rez requirements for " title " are not fulfilled"))))))

(defn can-steal?
  "Checks if the runner can steal agendas"
  [state side card]
  (and (check-flag-types? state side card :can-steal [:current-turn :current-run])
       (check-flag-types? state side card :can-steal [:current-turn :persistent])))

(defn can-trash?
  "Checks if the runner can trash cards"
  [state side card]
  (and (check-flag-types? state side card :can-trash [:current-turn :current-run])
       (check-flag-types? state side card :can-trash [:current-turn :persistent])))

(defn can-run?
  "Checks if the runner is allowed to run"
  [state side]
  (let [cards (->> @state :stack :current-turn :can-run (map :card))]
    (if (empty? cards)
      true
      (do (toast state side (str "Cannot run due to " (join ", " (map :title cards))))
        false))))

(defn can-access?
  "Checks if the runner can access the specified card"
  [state side card]
  (check-flag-types? state side card :can-access [:current-run :current-turn :persistent]))

(defn can-access-loud
  "Checks if the runner can access the card, toasts card that is preventing it"
  [state side card]
  (let [cards (get-preventing-cards state side card :can-access [:current-run :current-turn :persistent])]
    (if (empty? cards)
      true
      (do (toast state side (str "Cannot access " (card-str state card) " because of " (join ", " (map :title cards))) "info")
          false))))

(defn can-advance?
  "Checks if the corp can advance cards"
  [state side card]
  (check-flag-types? state side card :can-advance [:current-turn :persistent]))

(defn can-score?
  "Checks if the corp can score cards"
  [state side card]
  (and
    (some? card)
    ;; The agenda has enough agenda counters to legally score
    (let [cost (or (:current-cost card)
                   (:advancementcost card))]
      (and cost
           (<= cost (get-counters card :advancement))))
    ;; An effect hasn't be flagged as unable to be scored (Dedication Ceremony)
    (check-flag-types? state side card :can-score [:current-turn :persistent])
    ;; An effect hasn't set a card as unable to be scored (Clot)
    (empty? (filter #(same-card? card %) (get-in @state [:corp :register :cannot-score])))
    ;; A terminal operation hasn't been played
    (not (get-in @state [:corp :register :terminal]))))

(defn is-scored?
  "Checks if the specified card is in the scored area of the specified player."
  [state side card]
  (some #(same-card? % card) (get-in @state [side :scored])))

(defn in-corp-scored?
  "Checks if the specified card is in the Corp score area."
  [state side card]
  (is-scored? state :corp card))

(defn in-runner-scored?
  "Checks if the specified card is in the Runner score area."
  [state side card]
  (is-scored? state :runner card))

(defn card-is-public?
  [state side {:keys [zone] :as card}]
  (if (= side :runner)
    ;; public runner cards: in hand and :openhand is true;
    ;; or installed/hosted and not facedown;
    ;; or scored or current or in heap
    (or (corp? card)
        (and (:openhand (:runner @state))
             (in-hand? card))
        (and (or (installed? card)
                 (:host card))
             (not (facedown? card)))
        (#{:scored :discard :current} (last zone)))
    ;; public corp cards: in hand and :openhand;
    ;; or installed and rezzed;
    ;; or in :discard and :seen
    ;; or scored or current
    (or (runner? card)
        (and (:openhand (:corp @state))
             (in-hand? card))
        (and (or (installed? card)
                 (:host card))
             (or (operation? card)
                 (rezzed? card)))
        (and (in-discard? card) (:seen card))
        (#{:scored :current} (last zone)))))

(defn can-host?
  "Checks if the specified card is able to host other cards"
  [card]
  (or (not (rezzed? card))
      (not (:cannot-host (card-def card)))))

(defn when-scored?
  "Checks if the specified card is able to be used for a when-scored text ability"
  [card]
  (not (:not-when-scored (card-def card))))

(defn ab-can-prevent?
  "Checks if the specified ability definition should prevent.
  Checks for a :req in the :prevent map of the card-def.
  Defaults to false if req check not met"
  ([state side card req-fn target args]
   (ab-can-prevent? state side (make-eid state) card req-fn target args))
  ([state side eid card req-fn target args]
   (cond
     req-fn (req-fn state side eid card (list (assoc args :prevent-target target)))
     :else false)))

(defn get-card-prevention
  "Returns card prevent abilities for a given type"
  [card type]
  (filter #(contains? (:type %) type)
          (-> card card-def :interactions :prevent)))

(defn card-can-prevent?
  "Checks if a cards req (truthy test) can be met for this type"
  [state side card type target args]
  (->> (get-card-prevention card type)
       (map #(ab-can-prevent? state side card (:req %) target args))
       (some #(-> % false? not))))

(defn cards-can-prevent?
  "Checks if any cards in a list can prevent this type"
  ([state side cards type] (cards-can-prevent? state side cards type nil nil))
  ([state side cards type target args]
   (->> cards
        (map #(card-can-prevent? state side % type target args))
        (some true?))))

(defn get-prevent-list
  "Get list of cards that have prevent for a given type"
  [state side type]
  (filter #(seq (get-card-prevention % type))
          (all-active state side)))
