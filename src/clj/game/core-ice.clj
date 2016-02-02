(in-ns 'game.core)

(declare card-flag?)

;;; Ice strength functions
(defn ice-strength-bonus
  "Increase the strength of the given ice by n. Negative values cause a decrease."
  [state side n ice]
  ;; apply the strength bonus if the bonus is positive, or if the ice doesn't have the "can't lower strength" flag
  (when (or (pos? n) (not (card-flag? ice :cannot-lower-strength true)))
    (swap! state update-in [:bonus :ice-strength] (fnil #(+ % n) 0))))

(defn ice-strength
  "Gets the modified strength of the given ice."
  [state side {:keys [strength] :as card}]
  (+ (if-let [strfun (:strength-bonus (card-def card))]
       (+ strength (strfun state side card nil))
       strength)
     (or (get-in @state [:bonus :ice-strength]) 0)))

(defn update-ice-strength
  "Updates the given ice's strength by triggering strength events and updating the card."
  [state side ice]
  (let [ice (get-card state ice) oldstren (or (:current-strength ice) (:strength ice))]
    (when (:rezzed ice)
      (swap! state update-in [:bonus] dissoc :ice-strength)
      (trigger-event state side :pre-ice-strength ice)
      (update! state side (assoc ice :current-strength (ice-strength state side ice)))
      (trigger-event state side :ice-strength-changed (get-card state ice) oldstren))))

(defn update-ice-in-server
  "Updates all ice in the given server's :ices field."
  [state side server]
  (doseq [ice (:ices server)] (update-ice-strength state side ice) ))

(defn update-all-ice
  "Updates all installed ice."
  [state side]
  (doseq [server (get-in @state [:corp :servers])]
    (update-ice-in-server state side (second server)))
  (update-run-ice state :corp))

(defn update-run-ice
  "Updates the :run :ices key with an updated copy of all ice in the run's server."
  [state side]
  (when (get-in @state [:run])
    (let [s (get-in @state [:run :server])
          ices (get-in @state (concat [:corp :servers] s [:ices]))]
      (swap! state assoc-in [:run :ices] ices))))

(defn trash-ice-in-run
  "Decreases the position of each ice in the run. For when an ice is trashed mid-run."
  [state]
  (when-let [run (:run @state)]
    (swap! state update-in [:run :position] dec)
    (trigger-event state :runner :pass-ice nil)))


;;; Icebreaker functions.
(defn breaker-strength-bonus
  "Increase the strength of the breaker by n. Negative values cause a decrease."
  [state side n]
  (swap! state update-in [:bonus :breaker-strength] (fnil #(+ % n) 0)))

(defn breaker-strength
  "Gets the modified strength of the given breaker."
  [state side {:keys [strength] :as card}]
  (when-not (nil? strength)
    ;; A breaker's current strength is the sum of its native strength,
    ;; the bonus reported by its :strength-bonus function,
    ;; the effects of per-encounter and per-run strength pumps,
    ;; and miscellaneous increases registered by third parties (Dinosaurus, others).
    (+ (if-let [strfun (:strength-bonus (card-def card))]
         (+ strength (strfun state side card nil))
         strength)
       (get-in card [:pump :encounter] 0)
       (get-in card [:pump :all-run] 0)
       (get-in @state [:bonus :breaker-strength] 0))))

(defn update-breaker-strength
  "Updates a breaker's current strength by triggering updates and applying their effects."
  [state side breaker]
  (let [breaker (get-card state breaker)
        oldstren (or (:current-strength breaker) (:strength breaker))]
    (swap! state update-in [:bonus] dissoc :breaker-strength)
    (trigger-event state side :pre-breaker-strength breaker)
    (update! state side (assoc breaker :current-strength (breaker-strength state side breaker)))
    (trigger-event state side :breaker-strength-changed (get-card state breaker) oldstren)))

(defn pump
  "Increase a breaker's strength by n for the given duration of :encounter or :all-run"
  ([state side card n] (pump state side card n :encounter))
  ([state side card n duration]
   (update! state side (update-in card [:pump duration] (fnil #(+ % n) 0)))
   (update-breaker-strength state side (get-card state card))))


;;; Others
(defn ice-index
  "Get the zero-based index of the given ice in its server's list of ice, where index 0
  is the innermost ice."
  [state ice]
  (first (keep-indexed #(when (= (:cid %2) (:cid ice)) %1) (get-in @state (cons :corp (:zone ice))))))
