(in-ns 'game.core)

;; These functions are called by main.clj in response to commands sent by users.

(declare available-mu card-str can-rez? can-advance? corp-install effect-as-handler
         enforce-msg get-remote-names get-run-ices jack-out move
         name-zone play-instant purge make-run runner-install trash get-strength
         update-breaker-strength update-ice-in-server update-run-ice win can-run?
         can-run-server? can-score? say play-sfx base-mod-size free-mu total-run-cost
         reset-all-subs! resolve-subroutine! resolve-unbroken-subs! break-subroutine!
         update-all-ice update-all-icebreakers continue play-ability
         play-heap-breaker-auto-pump-and-break installable-servers get-runnable-zones
         pump get-current-ice)

;;; Neutral actions
(defn play
  "Called when the player clicks a card from hand."
  [state side {:keys [card server]}]
  (when-let [card (get-card state card)]
    (case (:type card)
      ("Event" "Operation") (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 3 :targets [card]})
      ("Hardware" "Resource" "Program" "ICE" "Upgrade" "Asset" "Agenda") (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 2 :targets [card server]}))))

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
  (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 1}))

(defn click-credit
  "Click to gain 1 credit."
  [state side args]
  (play-ability state side {:card (get-in @state [side :basic-action-card]) :ability 0}))

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

(defn- change-agenda-points
  "Change a player's total agenda points. This is done through registering an agenda
  point effect that's only used when tallying total agenda points. Instead of adding or
  removing these effects, we allow for creating as many as needed to properly adjust
  the total."
  [state side delta]
  (register-floating-effect
    state side nil
    ;; This is needed as `req` creates/shadows the existing `side` already in scope.
    (let [user-side side]
      {:type :user-agenda-points
       ;; `target` is either `:corp` or `:runner`
       :req (req (= user-side target))
       :value delta}))
  (update-all-agenda-points state side)
  (system-msg state side
              (str "sets their agenda points to " (get-in @state [side :agenda-point])
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
    :agenda-point (change-agenda-points state side delta)
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
            card-prompts (filter #(same-card? :title % c) (get-in @state [side :prompt]))
            log-move (fn [verb & text]
                       (system-msg state side (str verb " " label from-str
                                                   (when (seq text)
                                                     (apply str " " text)))))]
        (case server
          ("Heap" "Archives")
          (do (if (not (zero? (count card-prompts)))
                  ;remove all prompts associated with the trashed card
                  (do (swap! state update-in [side :prompt] #(filter (fn [p] (not= (get-in p [:card :title]) (:title c))) %))
                      (map #(effect-completed state side (:eid %)) card-prompts)))
              (if (= :hand (first (:zone c)))
                ;; Discard from hand, do not trigger trash
                (do (move-card-to :discard {:force true})
                    (log-move "discards"))
                (do (trash state s (make-eid state) c {:unpreventable true})
                    (log-move "trashes"))))
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
  true)

(defn- prompt-error
  [context prompt prompt-args]
  (.println *err* (with-out-str (clojure.stacktrace/print-stack-trace
                                  (Exception. (str "Error " context)))))
  (.println *err* (str "Prompt: " prompt))
  (.println *err* (str "Prompt args: " prompt-args)))

(defn resolve-prompt
  "Resolves a prompt by invoking its effect function with the selected target of the prompt.
  Triggered by a selection of a prompt choice button in the UI."
  [state side {:keys [choice] :as args}]
  (let [prompt (first (get-in @state [side :prompt]))
        effect (:effect prompt)
        card (get-card state (:card prompt))
        choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (= :trace (:prompt-type prompt))
          (:counter choices)
          (:number choices))
      (if (number? choice)
        (do (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % prompt) pr)))
            (when (= choices :credit) ; :credit prompts require payment
              (pay state side card :credit (min choice (get-in @state [side :credit]))))
            (when (:counter choices)
              ;; :Counter prompts deduct counters from the card
              (add-counter state side card (:counter choices) (- choice)))
            ;; trigger the prompt's effect function
            (when effect
              (effect (or choice card)))
            (finish-prompt state side prompt card))
        (prompt-error "in an integer prompt" prompt args))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (if (string? choice)
        (let [title-fn (:card-title choices)
              found (some #(when (= (lower-case choice) (lower-case (:title % ""))) %) (server-cards))]
          (if found
            (if (title-fn state side (make-eid state) card [found])
              (do (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % prompt) pr)))
                  (when effect
                    (effect (or choice card)))
                  (finish-prompt state side prompt card))
              (toast state side (str "You cannot choose " choice " for this effect.") "warning"))
            (toast state side (str "Could not find a card named " choice ".") "warning")))
        (prompt-error "in a card-title prompt" prompt args))

      ;; Otherwise, choices is a sequence of strings and/or cards
      ;; choice is a string and should match one of the strings, or the title of one of the cards
      (:uuid choice)
      (let [uuid (uuid/as-uuid (:uuid choice))
            match (first (filter #(= uuid (:uuid %)) choices))]
        (when match
          ;; remove the prompt from the queue
          (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % prompt) pr)))
          (if (= (:value match) "Cancel")
            (do (if-let [cancel-effect (:cancel-effect prompt)]
                  ;; trigger the cancel effect
                  (cancel-effect choice)
                  (effect-completed state side (:eid prompt)))
                (finish-prompt state side prompt card))
            (do (effect (:value match))
                (finish-prompt state side prompt card)))))

      :else
      (prompt-error "in an unknown prompt type" prompt args))))

(defn select
  "Attempt to select the given card to satisfy the current select prompt. Calls resolve-select
  if the max number of cards has been selected."
  [state side {:keys [card] :as args}]
  (let [target (get-card state card)
        prompt (first (get-in @state [side :selected]))
        ability (:ability prompt)
        card-req (:req prompt)
        card-condition (:card prompt)
        cid (:not-self prompt)]
    (when (and (not= (:cid target) cid)
               (cond
                 card-condition (card-condition target)
                 card-req (card-req state side (:eid ability) (get-card state (:card ability)) [target])
                 :else true))
      (let [c (update-in target [:selected] not)]
        (update! state side c)
        (if (:selected c)
          (swap! state update-in [side :selected 0 :cards] #(conj % c))
          (swap! state update-in [side :selected 0 :cards]
                 (fn [coll] (remove-once #(same-card? % target) coll))))
        (let [selected (get-in @state [side :selected 0])
              prompt (first (get-in @state [side :prompt]))
              card (:card prompt)]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side card (select-keys prompt [:cancel-effect]) update! resolve-ability)))))))

(declare check-for-empty-server handle-end-run)

(defn- do-play-ability [state side card ability ability-idx targets]
  (let [cost (card-ability-cost state side ability card targets)]
    (when (or (nil? cost)
              (can-pay? state side (make-eid state {:source card :source-type :ability :source-info {:ability-idx ability-idx}}) card (:title card) cost))
      (when-let [activatemsg (:activatemsg ability)]
        (system-msg state side activatemsg))
      (let [eid (make-eid state {:source card :source-type :ability :source-info {:ability-idx ability-idx}})]
        (wait-for (resolve-ability state side eid (assoc ability :cost cost) card targets)
                  (when (check-for-empty-server state)
                    (handle-end-run state side)))))))

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
                           (trigger-event-sync state side eid :spent-credits-from-card card))}
             (get-in cdef [:abilities ability]))
        cannot-play (or (:disabled card)
                        (any-effects state side :prevent-ability true? card [ab ability]))]
    (when-not cannot-play
      (do-play-ability state side card ab ability targets))))

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

(defn play-auto-pump-and-break-impl
  [state side sub-groups-to-break current-ice break-ability]
  {:async true
   :effect (req
             (let [subs-to-break (first sub-groups-to-break)
                   sub-groups-to-break (rest sub-groups-to-break)]
               (doseq [sub subs-to-break]
                 (break-subroutine! state (get-card state current-ice) sub card))
               (let [ice (get-card state current-ice)
                     on-break-subs (when ice (:on-break-subs (card-def current-ice)))
                     event-args (when on-break-subs
                                  {:card-abilities (ability-as-handler ice on-break-subs)})]
               (wait-for
                 (resolve-ability state side (make-eid state {:source card :source-type :ability})
                                  (:additional-ability break-ability) (get-card state card) nil)
                 (wait-for (trigger-event-simult state side :subroutines-broken event-args ice subs-to-break)
                           (if (empty? sub-groups-to-break)
                             (effect-completed state side eid)
                             (continue-ability state side (play-auto-pump-and-break-impl state side sub-groups-to-break current-ice break-ability) card nil)))))))})

(defn play-auto-pump-and-break
  "Use play-auto-pump and then break all available subroutines"
  [state side args]
  (if (some #(:heap-breaker-break %) (:abilities (card-def (get-card state (:card args)))))
    (play-heap-breaker-auto-pump-and-break state side args)
    (let [run (:run @state)
          card (get-card state (:card args))
          eid (make-eid state {:source card :source-type :ability})
          run-ice (get-run-ices state)
          ice-cnt (count run-ice)
          ice-idx (dec (:position run 0))
          in-range (and (pos? ice-cnt) (< -1 ice-idx ice-cnt))
          current-ice (when (and run in-range) (get-card state (run-ice ice-idx)))
          ;; match strength
          can-pump (fn [ability]
                     (when (:pump ability)
                       ((:req ability) state side eid card nil)))
          pump-ability (some #(when (can-pump %) %) (:abilities (card-def card)))
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
                      (when (:break-req ability)
                        ((:break-req ability) state side eid card nil)))
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
                  (let [cost-str async-result
                        sub-groups-to-break (if (pos? subs-broken-at-once)
                                              (partition subs-broken-at-once subs-broken-at-once nil (remove :broken (:subroutines current-ice)))
                                              [(remove :broken (:subroutines current-ice))])]
                    (wait-for (resolve-ability state side (play-auto-pump-and-break-impl state side sub-groups-to-break current-ice break-ability) card nil)
                              (system-msg state side
                                          (if (pos? times-pump)
                                            (str (build-spend-msg cost-str "increase")
                                                 "the strength of " (:title card)
                                                 " to " (get-strength (get-card state card))
                                                 " and break all " (when (< 1 unbroken-subs) unbroken-subs)
                                                 " subroutines on " (:title current-ice))
                                            (str (build-spend-msg cost-str "use")
                                                 (:title card)
                                                 " to break "
                                                 (if some-already-broken
                                                   "the remaining "
                                                   "all ")
                                                 unbroken-subs " subroutines on "
                                                 (:title current-ice))))
                              (continue state side nil))))))))

(defn play-heap-breaker-auto-pump-and-break-impl
  [state side sub-groups-to-break current-ice]
  {:async true
   :effect (req
             (let [subs-to-break (first sub-groups-to-break)
                   sub-groups-to-break (rest sub-groups-to-break)]
               (doseq [sub subs-to-break]
                 (break-subroutine! state (get-card state current-ice) sub card))
               (let [ice (get-card state current-ice)
                     on-break-subs (when ice (:on-break-subs (card-def current-ice)))
                     event-args (when on-break-subs
                                  {:card-abilities (ability-as-handler ice on-break-subs)})]
                 (wait-for (trigger-event-simult state side :subroutines-broken event-args ice subs-to-break)
                           (if (empty? sub-groups-to-break)
                             (effect-completed state side eid)
                             (continue-ability state side (play-heap-breaker-auto-pump-and-break-impl state side sub-groups-to-break current-ice) card nil))))))})

(defn play-heap-breaker-auto-pump-and-break
  "Play auto-pump-and-break for heap breakers"
  [state side args]
  (let [run (:run @state)
        card (get-card state (:card args))
        eid (make-eid state {:source card :source-type :ability})
        run-ice (get-run-ices state)
        ice-cnt (count run-ice)
        ice-idx (dec (:position run 0))
        in-range (and (pos? ice-cnt) (< -1 ice-idx ice-cnt))
        current-ice (when (and run in-range) (get-card state (run-ice ice-idx)))
        ; copied from heap-breaker-auto-pump-and-break
        abs (remove #(or (= (:dynamic %) :auto-pump)
                         (= (:dynamic %) :auto-pump-and-break))
                    (:abilities card))
        ;; match strength
        can-pump (fn [ability]
                   (when (:heap-breaker-pump ability)
                     ((:req ability (req true)) state side eid card nil)))
        breaker-ability (some #(when (can-pump %) %) (:abilities (card-def card)))
        pump-strength-at-once (when breaker-ability
                                (:heap-breaker-pump breaker-ability))
        subs-broken-at-once (when breaker-ability
                              (:heap-breaker-break breaker-ability))
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
        total-cost (when (and breaker-ability
                              ability-uses-needed)
                     (if x-breaker
                       [:credit x-number]
                       (repeat ability-uses-needed (:cost breaker-ability))))]
    (when (can-pay? state side eid card (:title card) total-cost)
      (wait-for (pay-sync state side (make-eid state eid) card total-cost)
                (if x-breaker
                  (pump state side (get-card state card) x-number)
                  (pump state side (get-card state card) (* pump-strength-at-once ability-uses-needed)))
                (let [cost-str async-result
                      sub-groups-to-break (if (and (number? subs-broken-at-once) (pos? subs-broken-at-once))
                                        (partition subs-broken-at-once subs-broken-at-once nil (remove :broken (:subroutines current-ice)))
                                        [(remove :broken (:subroutines current-ice))])]
                  (wait-for (resolve-ability state side (play-heap-breaker-auto-pump-and-break-impl state side sub-groups-to-break current-ice) card nil)
                            (system-msg state side
                                        (str (build-spend-msg cost-str "increase")
                                             "the strength of " (:title card)
                                             " to " (get-strength (get-card state card))
                                             " and break all subroutines on " (:title current-ice)))
                            (continue state side nil)))))))

(defn play-copy-ability
  "Play an ability from another card's definition."
  [state side {:keys [card source index] :as args}]
  (let [card (get-card state card)
        source-abis (:abilities (card-def source))
        abi (when (< -1 index (count source-abis))
              (nth source-abis index))]
    (when abi
      (do-play-ability state side card abi index nil))))

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
        ab (get-in cdef [:corp-abilities ability])
        cannot-play (or (:disabled card)
                        (any-effects state side :prevent-ability true? card [ab ability]))]
    (when-not cannot-play
      (do-play-ability state side card ab ability targets))))

(defn play-runner-ability
  "Triggers a corp card's runner-ability using its zero-based index into the card's card-def :runner-abilities vector."
  [state side {:keys [card ability targets] :as args}]
  (let [card (get-card state card)
        cdef (card-def card)
        ab (get-in cdef [:runner-abilities ability])
        cannot-play (or (:disabled card)
                        (any-effects state side :prevent-ability true? card [ab ability]))]
    (when-not cannot-play
      (do-play-ability state side card ab ability targets))))

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
  (play-ability state side {:card (get-in @state [:corp :basic-action-card]) :ability 5}))

(defn do-purge
  "Purge viruses."
  [state side args]
  (play-ability state side {:card (get-in @state [:corp :basic-action-card]) :ability 6}))

(defn get-rez-cost
  [state side card {:keys [ignore-cost alternative-cost cost-bonus] :as args}]
  (cond
    (= :all-costs ignore-cost)
    [:credit 0]
    alternative-cost
    alternative-cost
    :else
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
    {:keys [ignore-cost no-warning force declined-alternative-cost alternative-cost no-msg
            cost-bonus press-continue] :as args}]
   (let [eid (eid-set-defaults eid :source nil :source-type :rez)
         card (get-card state card)
         alternative-cost (when (and card
                                     (not alternative-cost)
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
             :yes-ability {:async true
                           :effect (effect (rez eid card (merge args {:ignore-cost true
                                                                      :alternative-cost alternative-cost})))}
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
                                                (:alternative-cost args) " by paying its alternative cost"
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
                           (trigger-event-sync state side eid :rez (get-card state card))
                           (when press-continue
                             (continue state side nil)))
                       (effect-completed state side eid)))))
       (effect-completed state side eid)))))

(defn derez
  "Derez a corp card."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "derezzes " (:title card)))
    (unregister-events state side card)
    (update! state :corp (deactivate state :corp card true))
    (let [cdef (card-def card)]
      (when-let [derez-effect (:derez-effect cdef)]
        (resolve-ability state side derez-effect (get-card state card) nil))
      (when-let [derezzed-events (:derezzed-events cdef)]
        (register-events state side card (map #(assoc % :condition :derezzed) derezzed-events))))
    (unregister-constant-effects state side card)
    (trigger-event state side :derez card side)))

(defn click-advance
  "Click to advance installed card."
  [state side {:keys [card]}]
  (when-let [card (get-card state card)]
    (play-ability state side {:card (get-in @state [:corp :basic-action-card]) :ability 4 :targets [card]})))

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
               (if (can-score? state side card)
                 ;; do not card-init necessarily. if card-def has :effect, wrap a fake event
                 (let [moved-card (move state :corp card :scored)
                       c (card-init state :corp moved-card {:resolve-effect false
                                                            :init-data true})
                       points (get-agenda-points state :corp c)]
                   (system-msg state :corp (str "scores " (:title c) " and gains " (quantify points "agenda point")))
                   (trigger-event-simult
                     state :corp eid :agenda-scored
                     {:first-ability {:async true
                                      :effect (req (when-let [current (first (get-in @state [:runner :current]))]
                                                     ;; This is to handle Employee Strike with damage IDs #2688
                                                     (when (:disable-id (card-def current))
                                                       (swap! state assoc-in [:corp :disable-id] true)))
                                                   (remove-old-current state side eid :runner))}
                      :card-abilities (card-as-handler c)
                      :after-active-player
                      {:effect (req (let [c (get-card state c)
                                          points (or (get-agenda-points state :corp c) points)]
                                      (set-prop state :corp (get-card state moved-card) :advance-counter 0)
                                      (swap! state update-in [:corp :register :scored-agenda] #(+ (or % 0) points))
                                      (swap! state dissoc-in [:corp :disable-id])
                                      (update-all-agenda-points state side)
                                      (check-winner state side)
                                      (play-sfx state side "agenda-score")))}}
                     c))
                 (effect-completed state side eid))))))

;;; Runner actions
(defn click-run
  "Click to start a run."
  [state side {:keys [server] :as args}]
  (play-ability state side {:card (get-in @state [:runner :basic-action-card]) :ability 4 :targets [server]}))

(defn remove-tag
  "Click to remove a tag."
  ([state side args] (remove-tag state side (make-eid state) args))
  ([state side eid args]
   (play-ability state side {:card (get-in @state [:runner :basic-action-card]) :ability 5})))

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

(defn generate-install-list
  [state side {:keys [card] :as args}]
  (let [card (get-card state card)]
    (if card
      (swap! state assoc-in [:corp :install-list] (installable-servers state card))
      (swap! state dissoc-in [:corp :install-list]))))

(defn generate-runnable-zones
  [state side args]
  (swap! state assoc-in [:runner :runnable-list] (zones->sorted-names (get-runnable-zones state))))
