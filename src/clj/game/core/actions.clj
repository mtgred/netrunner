(in-ns 'game.core)

;; These functions are called by main.clj in response to commands sent by users.

(declare available-mu card-str can-rez? can-advance? corp-install effect-as-handler
         enforce-msg gain-agenda-point get-remote-names get-run-ices jack-out move
         name-zone play-instant purge make-run runner-install trash get-strength
         update-breaker-strength update-ice-in-server update-run-ice win can-run?
         can-run-server? can-score? say play-sfx base-mod-size free-mu total-run-cost
         reset-all-subs! resolve-subroutine! resolve-unbroken-subs! break-subroutine!
         update-all-ice update-all-icebreakers)

;;; Neutral actions
(defn play
  "Called when the player clicks a card from hand."
  [state side {:keys [card server]}]
  (when-let [card (get-card state card)]
    (case (:type card)
      ("Event" "Operation") (play-instant state side (make-eid state {:source :action
                                                                      :source-type :play}) card {:base-cost [:click 1]})
      ("Hardware" "Resource" "Program") (runner-install state side (make-eid state {:source :action
                                                                                    :source-type :runner-install}) card {:base-cost [:click 1]})
      ("ICE" "Upgrade" "Asset" "Agenda") (corp-install state side (make-eid state {:source server
                                                                                   :source-type :corp-install}) card server {:base-cost [:click 1] :action :corp-click-install}))))

(defn shuffle-deck
  "Shuffle R&D/Stack."
  [state side {:keys [close] :as args}]
  (swap! state update-in [side :deck] shuffle)
  (if close
    (do
      (swap! state update-in [side] dissoc :view-deck)
      (system-msg state side "stops looking at their deck and shuffles it"))
    (system-msg state side "shuffles their deck")))

(defn click-draw
  "Click to draw."
  [state side args]
  (when (and (not (get-in @state [side :register :cannot-draw]))
             (pay state side nil :click 1 {:action :corp-click-draw}))
    (system-msg state side "spends [Click] to draw a card")
    (wait-for (trigger-event-simult state side (if (= side :corp) :pre-corp-click-draw :pre-runner-click-draw) nil nil)
              (trigger-event state side (if (= side :corp) :corp-click-draw :runner-click-draw) (->> @state side :deck (take 1)))
              (draw state side)
              (swap! state update-in [:stats side :click :draw] (fnil inc 0))
              (play-sfx state side "click-card"))))

(defn click-credit
  "Click to gain 1 credit."
  [state side args]
  (when (pay state side nil :click 1 {:action :corp-click-credit})
    (system-msg state side "spends [Click] to gain 1 [Credits]")
    (gain-credits state side 1 (if (= :corp side) :corp-click-credit :runner-click-credit))
    (swap! state update-in [:stats side :click :credit] (fnil inc 0))
    (trigger-event state side (if (= side :corp) :corp-click-credit :runner-click-credit))
    (play-sfx state side "click-credit")))

(defn- change-msg
  "Send a system message indicating the property change"
  [state side kw new-val delta]
  (let [key (name kw)]
    (system-msg state side
                (str "sets " (.replace key "-" " ") " to " new-val
                     " (" (if (pos? delta) (str "+" delta) delta) ")"))))

(defn- change-map
  "Change a player's property using the :mod system"
  [state side key delta]
  (gain state side key {:mod delta})
  (change-msg state side key (base-mod-size state side key) delta))

(defn- change-mu
  "Send a system message indicating how mu was changed"
  [state side delta]
  (free-mu state delta)
  (system-msg state side
              (str "sets unused MU to " (available-mu state)
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-tags
  "Change a player's base tag count"
  [state delta]
  (if (pos? delta)
    (do (gain state :runner :tag delta)
        (trigger-event state :runner :manual-gain-tag delta))
    (do (deduct state :runner [:tag (Math/abs delta)])
        (trigger-event state :runner :manual-lose-tag delta)))
  (system-msg state :runner
              (str "sets Tags to " (get-in @state [:runner :tag :base])
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-bad-pub
  "Change a player's base bad pub count"
  [state delta]
  (if (neg? delta)
    (deduct state :corp [:bad-publicity (Math/abs delta)])
    (gain state :corp :bad-publicity delta))
  (system-msg state :corp
              (str "sets Bad Publicity to " (get-in @state [:corp :bad-publicity :base])
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-generic
  "Change a player's base generic property."
  [state side key delta]
  (if (neg? delta)
    (deduct state side [key (- delta)])
    (swap! state update-in [side key] (partial + delta)))
  (change-msg state side key (get-in @state [side key]) delta))

(defn change
  "Increase/decrease a player's property (clicks, credits, MU, etc.) by delta."
  [state side {:keys [key delta]}]
  (case key
    :memory (change-mu state side delta)
    :hand-size (change-map state side key delta)
    :tag (change-tags state delta)
    :bad-publicity (change-bad-pub state delta)
    ; else
    (change-generic state side key delta)))

(defn move-card
  "Called when the user drags a card from one zone to another."
  [state side {:keys [card server]}]
  (let [c (get-card state card)
        ;; hack: if dragging opponent's card from play-area (Indexing), the previous line will fail
        ;; to find the card. the next line will search in the other player's play-area.
        c (or c (get-card state (assoc card :side (other-side (to-keyword (:side card))))))
        last-zone (last (:zone c))
        src (name-zone (:side c) (:zone c))
        from-str (when-not (nil? src)
                   (if (= :content last-zone)
                     (str " in " src) ; this string matches the message when a card is trashed via (trash)
                     (str " from their " src)))
        label (if (and (not= last-zone :play-area)
                       (not (and (runner? c)
                                 (= last-zone :hand)
                                 (or (= server "Stack")
                                     (= server "Grip"))))
                       (or (and (runner? c)
                                (or (not (facedown? c))
                                    (not= last-zone :hand)))
                           (rezzed? c)
                           (:seen c)
                           (= last-zone :deck)))
                (:title c)
                "a card")
        s (if (#{"HQ" "R&D" "Archives"} server) :corp :runner)]
    ;; allow moving from play-area always, otherwise only when same side, and to valid zone
    (when (and (not= src server)
               (same-side? s (:side card))
               (or (= last-zone :play-area)
                   (same-side? side (:side card))))
      (let [move-card-to (partial move state s c)
            log-move (fn [verb & text]
                       (system-msg state side (str verb " " label from-str
                                                   (when (seq text)
                                                     (apply str " " text)))))]
        (case server
          ("Heap" "Archives")
          (if (= :hand (first (:zone c)))
            ;; Discard from hand, do not trigger trash
            (do (move-card-to :discard {:force true})
                (log-move "discards"))
            (do (trash state s c {:unpreventable true})
                (log-move "trashes")))
          ("Grip" "HQ")
          (do (move-card-to :hand {:force true})
              (log-move "moves" "to " server))
          ("Stack" "R&D")
          (do (move-card-to :deck {:front true :force true})
              (log-move "moves" "to the top of " server))
          ;; default
          nil)))))

(defn concede
  "Trigger game concede by specified side. Takes a third argument for use with user commands."
  ([state side _] (concede state side))
  ([state side]
   (system-msg state side "concedes")
   (win state (if (= side :corp) :runner :corp) "Concede")))

(defn- finish-prompt [state side prompt card]
  (when-let [end-effect (:end-effect prompt)]
    (end-effect state side (make-eid state) card nil))
  ;; remove the prompt from the queue
  (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % prompt) pr)))
  ;; This is a dirty hack to end the run when the last access prompt is resolved.
  (when (empty? (get-in @state [:runner :prompt]))
    (when-let [run (:run @state)]
      (when (:ended run)
        (handle-end-run state :runner)))
    (swap! state dissoc :access))
  true)

(defn resolve-prompt
  "Resolves a prompt by invoking its effect function with the selected target of the prompt.
  Triggered by a selection of a prompt choice button in the UI."
  [state side {:keys [choice card] :as args}]
  (let [servercard (get-card state card)
        card (if (not= (:title card) (:title servercard))
               (server-card (:title card))
               servercard)
        prompt (first (get-in @state [side :prompt]))
        choices (:choices prompt)]
    (cond
      ;; Shortcut
      (= choice "Cancel")
      (do (if-let [cancel-effect (:cancel-effect prompt)]
            ;; trigger the cancel effect
            (cancel-effect choice)
            (effect-completed state side (:eid prompt)))
          (finish-prompt state side prompt card))

      ;; Integer prompts
      (or (= choices :credit)
          (= :trace (:prompt-type prompt))
          (:counter choices)
          (:number choices))
      (if (number? choice)
        (do (when (= choices :credit) ; :credit prompts require payment
              (pay state side card :credit (min choice (get-in @state [side :credit]))))
            (when (:counter choices)
              ;; :Counter prompts deduct counters from the card
              (add-counter state side (:card prompt) (:counter choices) (- choice)))
            ;; trigger the prompt's effect function
            (when-let [effect-prompt (:effect prompt)]
              (effect-prompt (or choice card)))
            (finish-prompt state side prompt card))
        (do
          (.println *err* (with-out-str
                            (clojure.stacktrace/print-stack-trace
                              (Exception. "Error in an integer prompt") 25)))
          (.println *err* (str "Current prompt: " prompt))))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (if (string? choice)
        (let [title-fn (:card-title choices)
              found (some #(when (= (lower-case choice) (lower-case (:title % ""))) %) (server-cards))]
          (if found
            (if (title-fn state side (make-eid state) (:card prompt) [found])
              (do (when-let [effect-prompt (:effect prompt)]
                    (effect-prompt (or choice card)))
                  (finish-prompt state side prompt card))
              (toast state side (str "You cannot choose " choice " for this effect.") "warning"))
            (toast state side (str "Could not find a card named " choice ".") "warning")))
        (do
          (.println *err* (with-out-str
                            (clojure.stacktrace/print-stack-trace
                              (Exception. "Error in a card-title prompt") 25)))
          (.println *err* (str "Current prompt: " prompt))))

      ;; Default text prompt
      :else
      (let [buttons (filter #(or (= choice %)
                                 (= card %)
                                 (let [choice-str (if (string? choice)
                                                    (lower-case choice)
                                                    (lower-case (:title choice "do-not-match")))]
                                   (or (= choice-str (lower-case %))
                                       (= choice-str (lower-case (:title % ""))))))
                            choices)
            button (first buttons)]
        (if button
          (do (when-let [effect-prompt (:effect prompt)]
                (effect-prompt button))
              (finish-prompt state side prompt card))
          (do
            (.println *err* (with-out-str
                              (clojure.stacktrace/print-stack-trace
                                (Exception. "Error in a text prompt") 25)))
            (.println *err* (str "Current prompt: " prompt))
            (.println *err* (str "Current args: " args))))))))

(defn select
  "Attempt to select the given card to satisfy the current select prompt. Calls resolve-select
  if the max number of cards has been selected."
  [state side {:keys [card] :as args}]
  (let [card (get-card state card)
        prompt (first (get-in @state [side :selected]))
        ability (:ability prompt)
        five (:five prompt)
        r (:req prompt)
        cid (:not-self prompt)]
    (when (and (not= (:cid card) cid)
               (cond
                 r (r card)
                 five (five state side (:eid ability) (:card ability) [card])
                 :else true))
      (let [c (update-in card [:selected] not)]
        (update! state side c)
        (if (:selected c)
          (swap! state update-in [side :selected 0 :cards] #(conj % c))
          (swap! state update-in [side :selected 0 :cards]
                 (fn [coll] (remove-once #(same-card? % card) coll))))
        (let [selected (get-in @state [side :selected 0])]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side update! resolve-ability)))))))

(defn- do-play-ability [state side card ability targets]
  (let [cost (:cost ability)
        cost-to-run (when (has-subtype? card "Run")
                      (total-run-cost state side card))]
    (when (or (and (nil? cost)
                   (nil? cost-to-run))
              (can-pay? state side (make-eid state {:source card :source-type :ability}) card (:title card) cost cost-to-run))
      (when-let [activatemsg (:activatemsg ability)]
        (system-msg state side activatemsg))
      (resolve-ability state side ability card targets))))

(defn play-ability
  "Triggers a card's ability using its zero-based index into the card's card-def :abilities vector."
  [state side {:keys [card ability targets] :as args}]
  (let [card (get-card state card)
        cdef (card-def card)
        abilities (:abilities cdef)
        ab (if (= ability (count abilities))
             ;; recurring credit abilities are not in the :abilities map and are implicit
             {:msg "take 1 [Recurring Credits]"
              :req (req (pos? (get-counters card :recurring)))
              :async true
              :effect (req (add-prop state side card :rec-counter -1)
                           (gain state side :credit 1)
                           (let [event (when (has-subtype? card "Stealth")
                                         :spent-stealth-credit)]
                             (trigger-event-sync state side eid event card)))}
             (get-in cdef [:abilities ability]))]
    (when-not (:disabled card)
      (do-play-ability state side card ab targets))))

(defn play-auto-pump
  "Use the 'match strength with ice' function of icebreakers."
  [state side args]
  (let [run (:run @state)
        card (get-card state (:card args))
        eid (make-eid state {:source card :source-type :ability})
        run-ice (get-run-ices state)
        ice-cnt (count run-ice)
        ice-idx (dec (:position run 0))
        in-range (and (pos? ice-cnt) (< -1 ice-idx ice-cnt))
        current-ice (when (and run in-range) (get-card state (run-ice ice-idx)))
        pump-ability (some #(when (:pump %) %) (:abilities (card-def card)))
        strength-diff (when (and current-ice
                                 (get-strength current-ice)
                                 (get-strength card))
                        (max 0 (- (get-strength current-ice)
                                  (get-strength card))))
        times-pump (when strength-diff
                     (int (Math/ceil (/ strength-diff (:pump pump-ability 1)))))
        total-pump-cost (when (and pump-ability
                                   times-pump)
                          (repeat times-pump (:cost pump-ability)))]
    (when (can-pay? state side eid card total-pump-cost)
      (wait-for (pay-sync state side (make-eid state eid) card total-pump-cost)
                (dotimes [n times-pump]
                  (resolve-ability state side (dissoc pump-ability :cost :msg) (get-card state card) nil))
                (system-msg state side (str (build-spend-msg async-result "increase")
                                            "the strength of " (:title card) " to "
                                            (:current-strength (get-card state card))))
                (effect-completed state side eid)))))

(defn play-auto-pump-and-break
  "Use play-auto-pump and then break all available subroutines"
  [state side args]
  (let [run (:run @state)
        card (get-card state (:card args))
        eid (make-eid state {:source card :source-type :ability})
        run-ice (get-run-ices state)
        ice-cnt (count run-ice)
        ice-idx (dec (:position run 0))
        in-range (and (pos? ice-cnt) (< -1 ice-idx ice-cnt))
        current-ice (when (and run in-range) (get-card state (run-ice ice-idx)))
        ;; match strength
        pump-ability (some #(when (:pump %) %) (:abilities (card-def card)))
        strength-diff (when (and current-ice
                                 (get-strength current-ice)
                                 (get-strength card))
                        (max 0 (- (get-strength current-ice)
                                  (get-strength card))))
        times-pump (when strength-diff
                     (int (Math/ceil (/ strength-diff (:pump pump-ability 1)))))
        total-pump-cost (when (and pump-ability
                                   times-pump)
                          (repeat times-pump (:cost pump-ability)))
        ;; break all subs
        can-break (fn [ability]
                    (if-let [subtype (:breaks ability)]
                      (or (= subtype "All")
                          (has-subtype? current-ice subtype))
                      false))
        break-ability (some #(when (can-break %) %) (:abilities (card-def card)))
        subs-broken-at-once (when break-ability
                              (:break break-ability 1))
        unbroken-subs (when (:subroutines current-ice)
                        (count (remove :broken (:subroutines current-ice))))
        some-already-broken (not= unbroken-subs (count (:subroutines current-ice)))
        times-break (when (and unbroken-subs
                               subs-broken-at-once)
                      (if (pos? subs-broken-at-once)
                        (int (Math/ceil (/ unbroken-subs subs-broken-at-once)))
                        1))
        total-break-cost (when (and break-ability
                                    times-break)
                           (repeat times-break (:break-cost break-ability)))
        total-cost (merge-costs (conj total-pump-cost total-break-cost))]
    (when (can-pay? state side eid card (:title card) total-cost)
      (wait-for (pay-sync state side (make-eid state eid) card total-cost)
                (dotimes [n times-pump]
                  (resolve-ability state side (dissoc pump-ability :cost :msg) (get-card state card) nil))
                (doseq [sub (remove :broken (:subroutines current-ice))]
                  (break-subroutine! state (get-card state current-ice) sub)
                  (resolve-ability state side (make-eid state {:source card :source-type :ability})
                                   (:additional-ability break-ability) (get-card state card) nil))
                (system-msg state side (if (pos? times-pump)
                                         (str (build-spend-msg async-result "increase")
                                              "the strength of " (:title card)
                                              " to " (get-strength (get-card state card))
                                              " and break all " unbroken-subs
                                              " subroutines on " (:title current-ice))
                                         (str (build-spend-msg async-result "use")
                                              (:title card)
                                              " to break "
                                              (if some-already-broken
                                                "the remaining "
                                                "all ")
                                              unbroken-subs " subroutines on "
                                              (:title current-ice))))))))

(defn play-copy-ability
  "Play an ability from another card's definition."
  [state side {:keys [card source index] :as args}]
  (let [card (get-card state card)
        source-abis (:abilities (card-def source))
        abi (when (< -1 index (count source-abis))
              (nth source-abis index))]
    (when abi
      (do-play-ability state side card abi nil))))

(def dynamic-abilities
  {"auto-pump" play-auto-pump
   "auto-pump-and-break" play-auto-pump-and-break
   "copy" play-copy-ability})

(defn play-dynamic-ability
  "Triggers an ability that was dynamically added to a card's data but is not necessarily present in its
  :abilities vector."
  [state side args]
  ((dynamic-abilities (:dynamic args)) state (keyword side) args))

(defn play-corp-ability
  "Triggers a runner card's corp-ability using its zero-based index into the card's card-def :corp-abilities vector."
  [state side {:keys [card ability targets] :as args}]
  (let [card (get-card state card)
        cdef (card-def card)
        ab (get-in cdef [:corp-abilities ability])]
    (do-play-ability state side card ab targets)))

(defn play-runner-ability
  "Triggers a corp card's runner-ability using its zero-based index into the card's card-def :runner-abilities vector."
  [state side {:keys [card ability targets] :as args}]
  (let [card (get-card state card)
        cdef (card-def card)
        ab (get-in cdef [:runner-abilities ability])]
    (do-play-ability state side card ab targets)))

(defn play-subroutine
  "Triggers a card's subroutine using its zero-based index into the card's :subroutines vector."
  [state side {:keys [card subroutine] :as args}]
  (let [card (get-card state card)
        sub (nth (:subroutines card) subroutine nil)]
    (when card
      (resolve-subroutine! state side card sub))))

(defn play-unbroken-subroutines
  "Triggers each unbroken subroutine on a card in order, waiting for each to complete"
  [state side {:keys [card] :as args}]
  (let [card (get-card state card)]
    (when card
      (resolve-unbroken-subs! state side card))))

;;; Corp actions
(defn trash-resource
  "Click to trash a resource."
  [state side args]
  (let [trash-cost (max 0 (- 2 (get-in @state [:corp :trash-cost-bonus] 0)))]
    (when-let [cost-str (pay state side nil :click 1 :credit trash-cost {:action :corp-trash-resource})]
      (resolve-ability state side
                       {:prompt  "Choose a resource to trash"
                        :choices {:req (fn [card]
                                         (if (and (seq (filter (fn [c] (untrashable-while-resources? c)) (all-active-installed state :runner)))
                                                  (> (count (filter resource? (all-active-installed state :runner))) 1))
                                           (and (resource? card) (not (untrashable-while-resources? card)))
                                           (resource? card)))}
                        :cancel-effect (effect (gain :credit trash-cost :click 1))
                        :effect  (effect (trash target)
                                         (system-msg (str (build-spend-msg cost-str "trash")
                                                          (:title target))))} nil nil))))

(defn do-purge
  "Purge viruses."
  [state side args]
  (when-let [cost (pay state side nil :click 3 {:action :corp-click-purge})]
    (purge state side)
    (let [spent (build-spend-msg cost "purge")
          message (str spent "all virus counters")]
      (system-msg state side message))
    (play-sfx state side "virus-purge")))

(defn get-rez-cost
  [state side card {:keys [ignore-cost cost-bonus] :as args}]
  (if (= :all-costs ignore-cost)
    [:credit 0]
    (let [cost (rez-cost state side card {:cost-bonus cost-bonus})
          additional-costs (rez-additional-cost-bonus state side card)]
      (concat
        (when-not ignore-cost
          [:credit cost])
        (when (not (:disabled card))
          additional-costs)))))

(defn rez
  "Rez a corp card."
  ([state side card] (rez state side (make-eid state) card nil))
  ([state side card args]
   (rez state side (make-eid state) card args))
  ([state side eid {:keys [disabled] :as card}
    {:keys [ignore-cost no-warning force declined-alternative-cost paid-alt no-msg
            cost-bonus] :as args}]
   (let [eid (eid-set-defaults eid :source nil :source-type :rez)
         card (get-card state card)
         alternative-cost (when (and card
                                     (not paid-alt)
                                     (not declined-alternative-cost))
                            (:alternative-cost (card-def card)))]
     (if (and card
              (or force
                  (can-rez? state side card))
              (or (asset? card)
                  (ice? card)
                  (upgrade? card)
                  (:install-rezzed (card-def card))))
       (if (and alternative-cost
                (not ignore-cost)
                (can-pay? state side eid card nil alternative-cost))
         (continue-ability
           state side
           {:optional
            {:prompt "Pay the alternative Rez cost?"
             :yes-ability {:cost alternative-cost
                           :async true
                           :effect (effect (rez eid card (merge args {:ignore-cost true
                                                                      :paid-alt true})))}
             :no-ability {:async true
                          :effect (effect (rez eid card (merge args {:declined-alternative-cost true})))}}}
           card nil)
         (let [cdef (card-def card)
               costs (get-rez-cost state side card args)]
           (wait-for (pay-sync state side (make-eid state eid) card costs)
                     (if-let [cost-str (and (string? async-result) async-result)]
                       (do (when (:derezzed-events cdef)
                             (unregister-events state side card))
                           (if-not disabled
                             (card-init state side (assoc card :rezzed :this-turn))
                             (update! state side (assoc card :rezzed :this-turn)))
                           (doseq [h (:hosted card)]
                             (update! state side (-> h
                                                     (update-in [:zone] #(map to-keyword %))
                                                     (update-in [:host :zone] #(map to-keyword %)))))
                           (when-not no-msg
                             (system-msg state side
                                         (str (build-spend-msg cost-str "rez" "rezzes")
                                              (:title card)
                                              (cond
                                                paid-alt " by paying its alternative cost"
                                                ignore-cost " at no cost"))))
                           (when (and (not no-warning) (:corp-phase-12 @state))
                             (toast state :corp "You are not allowed to rez cards between Start of Turn and Mandatory Draw.
                                                Please rez prior to clicking Start Turn in the future." "warning"
                                    {:time-out 0 :close-button true}))
                           (if (ice? card)
                             (do (update-ice-strength state side card)
                                 (play-sfx state side "rez-ice"))
                             (play-sfx state side "rez-other"))
                           (swap! state update-in [:stats :corp :cards :rezzed] (fnil inc 0))
                           (trigger-event-sync state side eid :rez card))
                       (effect-completed state side eid)))))
       (effect-completed state side eid)))))

(defn derez
  "Derez a corp card."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "derezzes " (:title card)))
    (update! state :corp (deactivate state :corp card true))
    (let [cdef (card-def card)]
      (when-let [derez-effect (:derez-effect cdef)]
        (resolve-ability state side derez-effect (get-card state card) nil))
      (register-events state side card (:derezzed-events cdef)))
    (unregister-constant-effects state side card)
    (trigger-event state side :derez card side)))

(defn advance
  "Advance a corp card that can be advanced.
   If you pass in a truthy value as the no-cost parameter, it will advance at no cost (for the card Success)."
  ([state side {:keys [card]}] (advance state side (make-eid state) card nil))
  ([state side card no-cost] (advance state side (make-eid state) card no-cost))
  ([state side eid card no-cost]
   (let [card (get-card state card)
         eid (eid-set-defaults eid :source nil :source-type :advance)]
     (when (can-advance? state side card)
       (wait-for (pay-sync state side (make-eid state eid) card :click (if-not no-cost 1 0) :credit (if-not no-cost 1 0) {:action :corp-advance})
                 (when-let [cost-str async-result]
                   (let [spent   (build-spend-msg cost-str "advance")
                         card    (card-str state card)
                         message (str spent card)]
                     (system-msg state side message))
                   (update-advancement-cost state side card)
                   (add-prop state side (get-card state card) :advance-counter 1)
                   (play-sfx state side "click-advance")))))))

(defn score
  "Score an agenda. It trusts the card data passed to it."
  ([state side args] (score state side (make-eid state) args))
  ([state side eid args]
   (let [card (or (:card args) args)]
     (wait-for (trigger-event-simult state :corp :pre-agenda-scored nil card)
               (when (can-score? state side card)
                 ;; do not card-init necessarily. if card-def has :effect, wrap a fake event
                 (let [moved-card (move state :corp card :scored)
                       c (card-init state :corp moved-card {:resolve-effect false
                                                            :init-data true})
                       points (get-agenda-points state :corp c)]
                   (system-msg state :corp (str "scores " (:title c) " and gains " (quantify points "agenda point")))
                   (trigger-event-simult state :corp eid :agenda-scored
                                         {:first-ability {:effect (req (when-let [current (first (get-in @state [:runner :current]))]
                                                                         ;; This is to handle Employee Strike with damage IDs #2688
                                                                         (when (:disable-id (card-def current))
                                                                           (swap! state assoc-in [:corp :disable-id] true)))
                                                                       (remove-old-current state side :runner))}
                                          :card-ability (card-as-handler c)
                                          :after-active-player {:effect (req (let [c (get-card state c)
                                                                                   points (or (get-agenda-points state :corp c) points)]
                                                                               (set-prop state :corp (get-card state moved-card) :advance-counter 0)
                                                                               (swap! state update-in [:corp :register :scored-agenda] #(+ (or % 0) points))
                                                                               (swap! state dissoc-in [:corp :disable-id])
                                                                               (gain-agenda-point state :corp points)
                                                                               (play-sfx state side "agenda-score")))}}
                    c)))))))

(defn no-action
  "The corp indicates they have no more actions for the encounter."
  [state side args]
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action")
  (trigger-event state side :no-action)
  (let [run-ice (get-run-ices state)
        pos (get-in @state [:run :position])
        ice (when (and pos (pos? pos) (<= pos (count run-ice)))
              (get-card state (nth run-ice (dec pos))))]
    (when (rezzed? ice)
      (trigger-event state side :encounter-ice ice)
      (update-ice-strength state side ice))))

;;; Runner actions
(defn click-run
  "Click to start a run."
  [state side {:keys [server] :as args}]
  (make-run state side (make-eid state) server nil nil {:click-run true}))

(defn remove-tag
  "Click to remove a tag."
  ([state side args] (remove-tag state side (make-eid state) args))
  ([state side eid args]
  (let [remove-cost (max 0 (- 2 (get-in @state [:runner :tag-remove-bonus] 0)))]
    (wait-for (pay-sync state side (make-eid state {:source :action :source-type :remove-tag}) nil :click 1 :credit remove-cost)
              (when-let [cost-str async-result]
                (lose-tags state :runner 1)
                (system-msg state side (string/trimr (build-spend-msg cost-str "remove 1 tag" "removes 1 tag")))
                (play-sfx state side "click-remove-tag"))
              (effect-completed state side eid)))))

(defn continue
  "The runner decides to approach the next ice, or the server itself."
  [state side args]
  (when (get-in @state [:run :no-action])
    (let [run-ice (get-run-ices state)
          pos (get-in @state [:run :position])
          cur-ice (when (and pos (pos? pos) (<= pos (count run-ice)))
                    (get-card state (nth run-ice (dec pos))))
          next-ice (when (and pos (< 1 pos) (<= (dec pos) (count run-ice)))
                     (get-card state (nth run-ice (- pos 2))))]
      (wait-for (trigger-event-sync state side :pass-ice cur-ice)
                (unregister-floating-effects state side :end-of-encounter)
                (unregister-floating-events state side :end-of-encounter)
                (swap! state update-in [:run :position] (fnil dec 1))
                (swap! state assoc-in [:run :no-action] false)
                (system-msg state side "continues the run")
                (when cur-ice
                  (reset-all-subs! state (get-card state cur-ice)))
                (update-all-ice state side)
                (update-all-icebreakers state side)
                (trigger-event-simult state side (make-eid state)
                                      (if next-ice :approach-ice :approach-server) nil (when next-ice next-ice))))))

(defn view-deck
  "Allows the player to view their deck by making the cards in the deck public."
  [state side args]
  (system-msg state side "looks at their deck")
  (swap! state assoc-in [side :view-deck] true))

(defn close-deck
  "Closes the deck view and makes cards in deck private again."
  [state side args]
  (system-msg state side "stops looking at their deck")
  (swap! state update-in [side] dissoc :view-deck))
