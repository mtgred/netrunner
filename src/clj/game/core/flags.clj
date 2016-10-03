(in-ns 'game.core)

;;;; Various functions for checking small "flag" values of cards, runs, players, etc.

(defn card-flag?
  "Checks the card to see if it has a :flags entry of the given flag-key with the given value"
  ;; TODO: add a register for mutable state card flags, separate from this
  [card flag-key value]
  (let [cdef (card-def card)]
    (= value (get-in cdef [:flags flag-key]))))

(defn card-flag-fn?
  "Checks the card to see if it has a :flags entry of the given flag-key, whose value is a four-argument
  function that returns the given value"
  [state side card flag-key value]
  (let [cdef (card-def card)
        func (get-in cdef [:flags flag-key])]
    (and func (= (func state side (make-eid state) card nil) value))))

(defn any-flag-fn?
  "Checks `card-flag-fn? on all installed cards on specified side for the value with the flag-key
  Default value of `cards` is `(all-active state side)`"
  ([state side flag-key value]
    (any-flag-fn? state side flag-key value (all-active state side)))
  ([state side flag-key value cards]
   (some #(card-flag-fn? state side % flag-key value) cards)))

(defn is-tagged?
  "Returns true if the runner is tagged."
  [state]
  (or (pos? (get-in state [:runner :tag]))
      (pos? (get-in state [:runner :tagged]))))

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
  (not (empty? (get-in @state [:stack flag-type flag]))))

(defn- clear-all-flags!
  "Clears all flags of specified type"
  [state flag-type]
  (swap! state assoc-in [:stack flag-type] nil))

(defn- clear-flag-for-card!
  "Remove all entries for specified card for flag-type and flag"
  [state side card flag-type flag]
  (swap! state update-in [:stack flag-type flag]
         (fn [flag-map] (remove #(= (:cid (:card %)) (:cid card)) flag-map))))

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
(defn prevent-run [state side]
  (swap! state assoc-in [:runner :register :cannot-run] true))

(defn prevent-draw [state side]
  (swap! state assoc-in [:runner :register :cannot-draw] true))

(defn prevent-jack-out [state side]
  (swap! state assoc-in [:run :cannot-jack-out] true))

;; This function appears unused as well
(defn prevent-steal [state side]
  (swap! state assoc-in [:runner :register :cannot-steal] true))

(defn prevent-current [state side]
  (swap! state assoc-in [:runner :register :cannot-play-current] true))

(defn lock-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(conj % cid)))

(defn release-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(remove #{cid} %)))


;;; Small utilities for card properties.
(defn in-server?
  "Checks if the specified card is installed in -- and not PROTECTING -- a server"
  [card]
  (= (last (:zone card)) :content))

(defn in-hand?
  "Checks if the specified card is in the hand."
  [card]
  (= (:zone card) [:hand]))

(defn in-discard?
  "Checks if the specified card is in the discard pile."
  [card]
  (= (:zone card) [:discard]))

(defn is-scored?
  "Checks if the specified card is in the scored area of the specified player."
  [state side card]
  (some #(= (:cid %) (:cid card)) (get-in @state [side :scored])))

(defn in-deck?
  "Checks if the specified card is in the draw deck."
  [card]
  (= (:zone card) [:deck]))

(defn facedown?
  "Checks if the specified card is facedown."
  [card]
  (or (= (:zone card) [:rig :facedown]) (:facedown card)))

(defn in-corp-scored?
  "Checks if the specified card is in the Corp score area."
  [state side card]
  (is-scored? state :corp card))

(defn in-runner-scored?
  "Checks if the specified card is in the Runner score area."
  [state side card]
  (is-scored? state :runner card))

(defn is-type?
  "Checks if the card is of the specified type, where the type is a string."
  [card type]
  (card-is? card :type type))

(defn has-subtype?
  "Checks if the specified subtype is present in the card.
  Mostly sugar for the has? function."
  [card subtype]
  (has? card :subtype subtype))

(defn ice? [card]
  (is-type? card "ICE"))

(defn rezzed? [card]
  (:rezzed card))

(defn installed? [card]
  (or (:installed card) (= :servers (first (:zone card)))))

(defn active? [{:keys [zone] :as card}]
  "Checks if the card is active and should receive game events/triggers."
  (or (is-type? card "Identity")
      (= zone [:current])
      (and (card-is? card :side :corp)
           (installed? card)
           (rezzed? card))
      (and (card-is? card :side :runner)
           (installed? card)
           (not (facedown? card)))))

;; This appears unused, can it be removed?
(defn untrashable-while-rezzed? [card]
  (and (card-flag? card :untrashable-while-rezzed true) (rezzed? card)))

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
        req (:rez-req (card-def card))]
    (cond
      ;; Card on same side?
      (not (same-side? side (:side card))) :side
      ;; No flag restrictions?
      (not (run-flag? state side card :can-rez)) :run-flag
      (not (turn-flag? state side card :can-rez)) :turn-flag
      ;; Uniqueness check
      (and uniqueness (some #(and (:rezzed %) (= (:code card) (:code %))) (all-installed state :corp))) :unique
      ;; Rez req check
      (and req (not (req state side (make-eid state) card nil))) :req
      ;; No problems - return true
      :default true)))

(defn can-rez?
  "Checks if the card can be rezzed. Toasts the reason if not."
  ([state side card] (can-rez? state side card nil))
  ([state side card _]
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
       :unique (reason-toast (str "Cannot rez a second copy of " title " since it is unique. Please trash the other"
                                  " copy first"))
       ;; Rez requirement
       :req (reason-toast (str "Rez requirements for " title " are not fulfilled"))))))

(defn can-steal?
  "Checks if the runner can steal agendas"
  [state side card]
  (check-flag-types? state side card :can-steal [:current-turn :current-run]))

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
  (check-flag-types? state side card :can-score [:current-turn :persistent]))

(defn can-be-advanced?
  "Returns true if the card can be advanced"
  [card]
  (or (card-is? card :advanceable :always)
      ;; e.g. Tyrant, Woodcutter
      (and (card-is? card :advanceable :while-rezzed)
           (rezzed? card))
      ;; e.g. Haas Arcology AI
      (and (card-is? card :advanceable :while-unrezzed)
           (not (rezzed? card)))
      (and (is-type? card "Agenda")
           (installed? card))))

(defn card-is-public? [state side {:keys [zone] :as card}]
  (if (= side :runner)
    ;; public runner cards: in hand and :openhand is true;
    ;; or installed/hosted and not facedown;
    ;; or scored or current or in heap
    (or (card-is? card :side :corp)
        (and (:openhand (:runner @state)) (in-hand? card))
        (and (or (installed? card) (:host card)) (not (facedown? card)))
        (#{:scored :discard :current} (last zone)))
    ;; public corp cards: in hand and :openhand;
    ;; or installed and rezzed;
    ;; or in :discard and :seen
    ;; or scored or current
    (or (card-is? card :side :runner)
        (and (:openhand (:corp @state)) (in-hand? card))
        (and (or (installed? card) (:host card))
             (or (is-type? card "Operation") (rezzed? card)))
        (and (in-discard? card) (:seen card))
        (#{:scored :current} (last zone)))))
