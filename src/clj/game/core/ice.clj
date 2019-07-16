(in-ns 'game.core)

(declare card-flag?)

;;; Ice subroutine functions
(defn add-sub
  ([ice sub] (add-sub ice sub (:cid ice) nil))
  ([ice sub cid] (add-sub ice sub cid nil))
  ([ice sub cid {:keys [front back printed variable] :as args}]
   (let [curr-subs (:subroutines ice [])
         position (cond
                    back 1
                    front -1
                    :else 0)
         new-sub {:label (make-label sub)
                  :from-cid cid
                  :sub-effect sub
                  :position position
                  :variable (or variable false)
                  :printed (or printed false)}
         new-subs (->> (conj curr-subs new-sub)
                       (sort-by :position)
                       (map-indexed (fn [idx sub] (assoc sub :index idx)))
                       (into []))]
     (assoc ice :subroutines new-subs))))

(defn add-sub!
  ([state side ice sub] (update! state :corp (add-sub ice sub (:cid ice) nil)))
  ([state side ice sub cid] (update! state :corp (add-sub ice sub cid nil)))
  ([state side ice sub cid args] (update! state :corp (add-sub ice sub cid args))))

(defn remove-sub
  "Removes a single sub from a piece of ice. By default removes the first subroutine
  with the same cid as the given ice."
  ([ice] (remove-sub ice #(= (:cid ice) (:from-cid %))))
  ([ice pred]
   (let [curr-subs (:subroutines ice)
         new-subs (remove-once pred curr-subs)]
     (assoc ice :subroutines new-subs))))

(defn remove-sub!
  ([state side ice] (update! state :corp (remove-sub ice #(= (:cid ice) (:from-cid %)))))
  ([state side ice pred]
   (update! state :corp (remove-sub ice pred))))

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
               (assoc-in [:special :extra-subs] extra-subs)))))

(defn break-subroutine
  "Marks a given subroutine as broken"
  [ice sub]
  (assoc ice :subroutines (assoc (:subroutines ice) (:index sub) (assoc sub :broken true))))

(defn break-subroutine!
  "Marks a given subroutine as broken, update!s state"
  [state ice sub]
  (update! state :corp (break-subroutine ice sub)))

(defn reset-broken-subs
  "Mark all broken subroutines as unbroken"
  [ice]
  (assoc ice :subroutines (into [] (for [sub (:subroutines ice)] (dissoc sub :broken)))))

(defn reset-broken-subs!
  "Marks all broken subroutines as unbroken, update!s state"
  [state ice]
  (update! state :corp (reset-broken-subs ice)))

(defn unbroken-subroutines-choice
  "Takes an ice, returns the ubroken subroutines for a choices prompt"
  [ice]
  (for [sub (remove :broken (:subroutines ice))]
    (make-label (:sub-effect sub))))

;;; Ice strength functions
(defn ice-strength-bonus
  "Increase the strength of the given ice by n. Negative values cause a decrease."
  [state side n ice]
  ;; apply the strength bonus if the bonus is positive, or if the ice doesn't have the "can't lower strength" flag
  (when (or (pos? n) (not (card-flag? ice :cannot-lower-strength true)))
    (swap! state update-in [:bonus :ice-strength] (fnil + 0) n)))

(defn ice-strength
  "Gets the modified strength of the given ice."
  [state side card]
  (let [strength (:strength card 0)]
    (+ (if-let [strfun (:strength-bonus (card-def card))]
         (+ strength (strfun state side (make-eid state) card nil))
         strength)
       (get-in @state [:bonus :ice-strength] 0))))

(defn update-ice-strength
  "Updates the given ice's strength by triggering strength events and updating the card."
  [state side ice]
  (let [ice (get-card state ice)
        oldstren (or (:current-strength ice) (:strength ice))]
    (when (rezzed? ice)
      (swap! state update-in [:bonus] dissoc :ice-strength)
      (wait-for (trigger-event-simult state side :pre-ice-strength nil ice)
                (update! state side (assoc ice :current-strength (ice-strength state side ice)))
                (trigger-event state side :ice-strength-changed (get-card state ice) oldstren)
                (swap! state update-in [:bonus] dissoc :ice-strength)))))

(defn update-ice-in-server
  "Updates all ice in the given server's :ices field."
  [state side server]
  (doseq [ice (:ices server)] (update-ice-strength state side ice)))

(defn update-all-ice
  "Updates all installed ice."
  [state side]
  (doseq [server (get-in @state [:corp :servers])]
    (update-ice-in-server state side (second server))))


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
         (+ strength (strfun state side (make-eid state) card nil))
         strength)
       (get-in card [:pump :encounter] 0)
       (get-in card [:pump :all-run] 0)
       (get-in card [:pump :all-turn] 0)
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
  "Increase a breaker's strength by n for the given duration of :encounter, :all-run or :all-turn"
  ([state side card n] (pump state side card n :encounter))
  ([state side card n duration]
   (update! state side (update-in card [:pump duration] (fnil #(+ % n) 0)))
   (update-breaker-strength state side (get-card state card))
   (trigger-event state side :pump-breaker n card)))

;;; Others
(defn ice-index
  "Get the zero-based index of the given ice in its server's list of ice, where index 0
  is the innermost ice."
  [state ice]
  (first (keep-indexed #(when (same-card? %2 ice) %1) (get-in @state (cons :corp (:zone ice))))))
