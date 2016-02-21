(in-ns 'game.core)

;; These functions are called by main.clj in response to commands sent by users.

(declare card-str can-rez? corp-install enforce-msg gain-agenda-point get-remote-names
         jack-out move name-zone play-instant purge resolve-select run has-subtype?
         runner-install trash update-breaker-strength update-ice-in-server update-run-ice win)

;;; Neutral actions
(defn play
  "Called when the player clicks a card from hand."
  [state side {:keys [card server]}]
  (case (:type card)
    ("Event" "Operation") (play-instant state side card {:extra-cost [:click 1]})
    ("Hardware" "Resource" "Program") (runner-install state side card {:extra-cost [:click 1]})
    ("ICE" "Upgrade" "Asset" "Agenda") (corp-install state side card server {:extra-cost [:click 1]}))
  (trigger-event state side :play card))

(defn shuffle-deck
  "Shuffle R&D/Stack."
  [state side {:keys [close] :as args}]
  (swap! state update-in [side :deck] shuffle)
  (if close
    (system-msg state side "stops looking at their deck and shuffles it")
    (system-msg state side "shuffles their deck")))

(defn click-draw
  "Click to draw."
  [state side args]
  (when (and (not (get-in @state [side :register :cannot-draw])) (pay state side nil :click 1))
    (system-msg state side "spends [Click] to draw a card")
    (draw state side)
    (trigger-event state side (if (= side :corp) :corp-click-draw :runner-click-draw))))

(defn click-credit
  "Click to gain 1 credit."
  [state side args]
  (when (pay state side nil :click 1)
    (system-msg state side "spends [Click] to gain 1 [Credits]")
    (gain state side :credit 1)
    (trigger-event state side (if (= side :corp) :corp-click-credit :runner-click-credit))))

(defn change
  "Increase/decrease a player's property (clicks, credits, MU, etc.) by delta."
  [state side {:keys [key delta]}]
  (let [kw (to-keyword key)]
    (if (neg? delta)
      (deduce state side [kw (- delta)])
      (swap! state update-in [side kw] (partial + delta)))
    (system-msg state side
                (str "sets " (.replace key "-" " ") " to " (get-in @state [side kw])
                     " (" (if (pos? delta) (str "+" delta) delta) ")"))))

(defn move-card
  "Called when the user drags a card from one zone to another."
  [state side {:keys [card server]}]
  (let [c (update-in card [:zone] #(map to-keyword %))
        last-zone (last (:zone c))
        src (name-zone (:side c) (:zone c))
        from-str (when-not (nil? src) (str " from their " src))
        label (if (and (not= last-zone :play-area)
                       (not (and (= (:side c)  "Runner")
                                 (= last-zone :hand)
                                 (= server "Grip")))
                       (or (and (= (:side c)  "Runner")
                                (not (:facedown c)))
                           (rezzed? c)
                           (:seen c)
                           (= last-zone :deck)))
                (:title c)
                "a card")
        s (if (#{"HQ" "R&D" "Archives"} server) :corp :runner)]
    (when-not (= src server)
      (case server
        ("Heap" "Archives")
        (do (trash state s c {:unpreventable true})
            (system-msg state side (str "trashes " label from-str)))
        ("HQ" "Grip")
        (do (move state s (dissoc c :seen :rezzed) :hand)
            (system-msg state side (str "moves " label from-str " to " server)))
        ("Stack" "R&D")
        (do (move state s (dissoc c :seen :rezzed) :deck {:front true})
            (system-msg state side (str "moves " label from-str " to the top of " server)))
        nil))))

(defn concede [state side args]
  (system-msg state side "concedes")
  (win state (if (= side :corp) :runner :corp) "Concede"))

(defn resolve-prompt
  "Resolves a prompt by invoking its effect funtion with the selected target of the prompt.
  Triggered by a selection of a prompt choice button in the UI."
  [state side {:keys [choice card] :as args}]
  (let [prompt (first (get-in @state [side :prompt]))
        choice (if (= (:choices prompt) :credit)
                 (min choice (get-in @state [side :credit]))
                 choice)]
    (if (not= choice "Cancel")
      ;; The user did not choose "cancel"
      (do (when (= (:choices prompt) :credit) ; :credit prompts require a pay
            (pay state side card :credit choice))
          (when (= (:choices prompt) :counter) ; :counter prompts deduct counters from the card
            (add-prop state side (:card prompt) :counter (- choice)))
          ;; trigger the prompt's effect function
          ((:effect prompt) (or choice card)))
      (when-let [cancel-effect (:cancel-effect prompt)]
        ;; the user chose "cancel" -- trigger the cancel effect.
        (cancel-effect choice)))
    ;; trigger end-effect if present
    (when-let [end-effect (:end-effect prompt)]
      (end-effect state side card nil))

    ;; remove the prompt from the queue
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % prompt) pr)))
    ;; This is a dirty hack to end the run when the last access prompt is resolved.
    (when (empty? (get-in @state [:runner :prompt]))
      (when-let [run (:run @state)]
        (when (:ended run)
          (handle-end-run state :runner)))
      (swap! state dissoc :access))))

(defn select
  "Attempt to select the given card to satisfy the current select prompt. Calls resolve-select
  if the max number of cards has been selected."
  [state side {:keys [card] :as args}]
  (let [r (get-in @state [side :selected 0 :req])]
    (when (or (not r) (r card))
      (let [c (update-in card [:selected] not)]
        (update! state side c)
        (if (:selected c)
          (swap! state update-in [side :selected 0 :cards] #(conj % c))
          (swap! state update-in [side :selected 0 :cards]
                 (fn [coll] (remove-once #(not= (:cid %) (:cid card)) coll))))
        (let [selected (get-in @state [side :selected 0])]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side)))))))


(defn play-ability
  "Triggers a card's ability using its zero-based index into the card's card-def :abilities vector."
  [state side {:keys [card ability targets] :as args}]
  (let [cdef (card-def card)
        abilities (:abilities cdef)
        ab (if (= ability (count abilities))
             ;; recurring credit abilities are not in the :abilities map and are implicit
             {:msg "take 1 [Recurring Credits]" :req (req (> (:rec-counter card) 0))
              :effect (effect (add-prop card :rec-counter -1) (gain :credit 1))}
             (get-in cdef [:abilities ability]))
        cost (:cost ab)]
    (when (or (nil? cost)
              (apply can-pay? state side (:title card) cost))
      (when-let [activatemsg (:activatemsg ab)] (system-msg state side activatemsg))
      (resolve-ability state side ab card targets))))


;;; Corp actions
(defn trash-resource
  "Click to trash a resource."
  [state side args]
  (let [trash-cost (max 0 (- 2 (or (get-in @state [:corp :trash-cost-bonus]) 0)))]
    (when-let [cost-str (pay state side nil :click 1 :credit trash-cost)]
      (resolve-ability state side
                       {:prompt "Choose a resource to trash"
                        :choices {:req #(is-type? % "Resource")}
                        :effect (effect (trash target)
                                        (system-msg (str (build-spend-msg cost-str "trash")
                                                         (:title target))))} nil nil))))

(defn do-purge
  "Purge viruses."
  [state side args]
  (when (pay state side nil :click 3)
    (system-msg state side "purges viruses")
    (purge state side)))

(defn rez
  "Rez a corp card."
  ([state side card] (rez state side card nil))
  ([state side card {:keys [ignore-cost no-warning] :as args}]
   (if (can-rez? state side card)
     (do
       (trigger-event state side :pre-rez card)
       (when (or (#{"Asset" "ICE" "Upgrade"} (:type card))
                 (:install-rezzed (card-def card)))
         (trigger-event state side :pre-rez-cost card)
         (let [cdef (card-def card)
               cost (rez-cost state side card)
               costs (concat (when-not ignore-cost [:credit cost])
                             (when (not= ignore-cost :all-costs) (:additional-cost cdef)))]
           (when-let [cost-str (apply pay state side card costs)]
             ;; Deregister the derezzed-events before rezzing card
             (when (:derezzed-events cdef)
               (unregister-events state side card))
             (card-init state side (assoc card :rezzed true))
             (doseq [h (:hosted card)]
               (update! state side (-> h
                                       (update-in [:zone] #(map to-keyword %))
                                       (update-in [:host :zone] #(map to-keyword %)))))
             (system-msg state side (str (build-spend-msg cost-str "rez" "rezzes")
                                         (:title card) (when ignore-cost " at no cost")))
             (when (and (not no-warning) (:corp-phase-12 @state))
               (toast state :corp "You are not allowed to rez cards between Start of Turn and Mandatory Draw.
                      Please rez prior to clicking Start Turn in the future." "warning"
                      {:time-out 0 :close-button true}))
             (when (ice? card)
               (update-ice-strength state side card)
               (update-run-ice state side))
             (trigger-event state side :rez card))))
       (swap! state update-in [:bonus] dissoc :cost)))))

(defn derez
  "Derez a corp card."
  [state side card]
  (system-msg state side (str "derezzes " (:title card)))
  (update! state :corp (deactivate state :corp card true))
  (let [cdef (card-def card)]
    (when-let [derez-effect (:derez-effect cdef)]
      (resolve-ability state side derez-effect (get-card state card) nil))
    (when-let [dre (:derezzed-events cdef)]
      (register-events state side dre card)))
  (trigger-event state side :derez card))

(defn advance
  "Advance a corp card that can be advanced."
  [state side {:keys [card]}]
  (when (pay state side card :click 1 :credit 1)
    (system-msg state side (str "advances " (card-str state card)))
    (update-advancement-cost state side card)
    (add-prop state side (get-card state card) :advance-counter 1)))

(defn score
  "Score an agenda."
  [state side args]
  (let [card (or (:card args) args)]
    (when (and (empty? (filter #(= (:cid card) (:cid %)) (get-in @state [:corp :register :cannot-score])))
               (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card))))
      (let [moved-card (move state :corp card :scored)
            c (card-init state :corp moved-card)
            points (get-agenda-points state :corp c)]
        (system-msg state :corp (str "scores " (:title c) " and gains " points
                                     " agenda point" (when (> points 1) "s")))
        (swap! state update-in [:corp :register :scored-agenda] #(+ (or % 0) points))
        (gain-agenda-point state :corp points)
        (set-prop state :corp c :advance-counter 0)
        (trigger-event state :corp :agenda-scored (assoc c :advance-counter 0))
        (when-let [current (first (get-in @state [:runner :current]))]
          (say state side {:user "__system__" :text (str (:title current) " is trashed.")})
          (trash state side current))))))

(defn no-action
  "The corp indicates they have no more actions for the encounter."
  [state side args]
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action")
  (trigger-event state side :no-action)
  (when-let [pos (get-in @state [:run :position])]
    (when-let [ice (when (and pos (> pos 0)) (get-card state (nth (get-in @state [:run :ices]) (dec pos))))]
      (when (:rezzed ice)
        (trigger-event state side :encounter-ice ice)
        (update-ice-strength state side ice)))))

;;; Runner actions
(defn click-run
  "Click to start a run."
  [state side {:keys [server] :as args}]
  (when (and (not (get-in @state [:runner :register :cannot-run])) (can-pay? state :runner "a run" :click 1))
    (system-msg state :runner (str "makes a run on " server))
    (run state side server)
    (pay state :runner nil :click 1)))

(defn remove-tag
  "Click to remove a tag."
  [state side args]
  (let [remove-cost (max 0 (- 2 (or (get-in @state [:runner :tag-remove-bonus]) 0)))]
    (when-let [cost-str (pay state side nil :click 1 :credit remove-cost :tag 1)]
      (system-msg state side (build-spend-msg cost-str "remove 1 Tag")))))

(defn auto-pump
  "Use the 'match strength with ice' function of icebreakers."
  [state side args]
  (let [run (:run @state) card (get-card state (:card args))
        current-ice (when (and run (> (or (:position run) 0) 0)) (get-card state ((:ices run) (dec (:position run)))))
        pumpabi (some #(when (:pump %) %) (:abilities (card-def card)))
        pumpcst (when pumpabi (second (drop-while #(and (not= % :credit) (not= % "credit")) (:cost pumpabi))))
        strdif (when current-ice (max 0 (- (or (:current-strength current-ice) (:strength current-ice))
                                           (or (:current-strength card) (:strength card)))))
        pumpnum (when strdif (int (Math/ceil (/ strdif (:pump pumpabi)))))]
    (when (and pumpnum pumpcst (>= (get-in @state [:runner :credit]) (* pumpnum pumpcst)))
      (dotimes [n pumpnum] (resolve-ability state side (dissoc pumpabi :msg) (get-card state card) nil))
      (system-msg state side (str "spends " (* pumpnum pumpcst) " [Credits] to increase the strength of "
                                  (:title card) " to " (:current-strength (get-card state card)))))))

(defn continue
  "The runner decides to approach the next ice, or the server itself."
  [state side args]
  (when (get-in @state [:run :no-action])
    (when-let [pos (get-in @state [:run :position])]
      (do (if-let [ice (when (and pos (> pos 0)) (get-card state (nth (get-in @state [:run :ices]) (dec pos))))]
            (trigger-event state side :pass-ice ice)
            (trigger-event state side :pass-ice nil))
          (update-ice-in-server state side (get-in @state (concat [:corp :servers] (get-in @state [:run :server]))))))
    (swap! state update-in [:run :position] dec)
    (swap! state assoc-in [:run :no-action] false)
    (system-msg state side "continues the run")
    (let [pos (get-in @state [:run :position])]
      (when (> (count (get-in @state [:run :ices])) 0)
        (update-ice-strength state side (nth (get-in @state [:run :ices]) pos)))
      (when (> pos 0)
        (let [ice (get-card state (nth (get-in @state [:run :ices]) (dec pos)))]
          (trigger-event state side :approach-ice ice))))
    (doseq [p (filter #(has-subtype? % "Icebreaker") (all-installed state :runner))]
      (update! state side (update-in (get-card state p) [:pump] dissoc :encounter))
      (update-breaker-strength state side p))))
