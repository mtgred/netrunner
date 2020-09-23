(in-ns 'game.core)

(defn get-runnable-zones
  ([state] (get-runnable-zones state :runner (make-eid state) nil nil))
  ([state side] (get-runnable-zones state side (make-eid state) nil nil))
  ([state side card] (get-runnable-zones state side (make-eid state) card nil))
  ([state side card args] (get-runnable-zones state side (make-eid state) card args))
  ([state side eid card {:keys [zones ignore-costs] :as args}]
   (let [restricted-zones (keys (get-in @state [:runner :register :cannot-run-on-server]))
         permitted-zones (remove (set restricted-zones) (or zones (get-zones state)))]
     (if ignore-costs
       permitted-zones
       (filter #(can-pay? state :runner eid (total-run-cost state side card {:server (unknown->kw %)}))
               permitted-zones)))))

(defn same-server?
  "True if the two cards are IN or PROTECTING the same server."
  [card1 card2]
  (and card1
       card2
       (let [zone1 (get-zone card1)
             zone2 (get-zone card2)]
         (= (second zone1) (second zone2)))))

(defn protecting-same-server?
  "True if an ice is protecting the server that the card is in or protecting."
  [card ice]
  (and card
       ice
       (let [zone1 (get-zone card)
             zone2 (get-zone ice)]
         (and (= (second zone1) (second zone2))
              (= :ices (last zone2))))))

(defn in-same-server?
  "True if the two cards are installed IN the same server, or hosted on cards IN the same server."
  [card1 card2]
  (let [zone1 (get-zone card1)
        zone2 (get-zone card2)]
    (and card1
         card2
         (= zone1 zone2)
         (is-remote? (second zone1)) ; cards in centrals are in the server's root, not in the server.
         (= :content (last zone1)))))

(defn from-same-server?
  "True if the upgrade is in the root of the server that the target is in."
  [upgrade target]
  (and (:cid upgrade)
       (:cid target)
       (= (central->zone (:zone target))
          (butlast (get-zone upgrade)))))

;;; Other helpers

(defn swap-agendas
  "Swaps the two specified agendas, first one scored (on corp side), second one stolen (on runner side)"
  [state side scored stolen]
  ;; Update location information
  (let [scored (assoc scored :scored-side :runner)
        stolen (assoc stolen :scored-side :corp)]
    ;; Move agendas
    (swap! state update-in [:corp :scored]
           (fn [coll] (conj (remove-once #(same-card? % scored) coll) stolen)))
    (swap! state update-in [:runner :scored]
           (fn [coll] (conj (remove-once #(same-card? % stolen) coll)
                            (if-not (card-flag? scored :has-abilities-when-stolen true)
                              (dissoc scored :abilities :events) scored))))
    ;; Set up abilities and events for new scored agenda
    (let [new-scored (find-cid (:cid stolen) (get-in @state [:corp :scored]))
          abilities (:abilities (card-def new-scored))
          new-scored (merge new-scored {:abilities abilities})]
      (update! state :corp new-scored)
      (unregister-events state side new-scored)
      (register-events state side new-scored)
      (unregister-constant-effects state side new-scored)
      (register-constant-effects state side new-scored)
      (resolve-ability state side (:swapped (card-def new-scored)) new-scored nil))
    ;; Set up abilities and events for new stolen agenda
    (when-not (card-flag? scored :has-events-when-stolen true)
      (let [new-stolen (find-cid (:cid scored) (get-in @state [:runner :scored]))]
        (deactivate state :corp new-stolen)))
    ;; Update agenda points
    (update-all-agenda-points state side)
    (check-winner state side)))

(defn remove-old-current
  "Trashes or RFG the existing current when a new current is played, or an agenda is stolen / scored"
  [state side eid current-side]
  (if-let [current (first (get-in @state [current-side :current]))]
    (do (trigger-event state side :trash-current current)
        (unregister-constant-effects state side current)
        (let [current (get-card state current)]
          (if (get-in current [:special :rfg-when-trashed])
            (do (system-say state side (str (:title current) " is removed from the game."))
                (move state (other-side side) current :rfg)
                (effect-completed state side eid))
            (do (system-say state side (str (:title current) " is trashed."))
                (trash state (to-keyword (:side current)) eid current nil)))))
    (effect-completed state side eid)))

;;; Functions for icons associated with special cards - e.g. Femme Fatale
(defn add-icon
  "Adds an icon to a card. E.g. a Femme Fatale token.
  Card is the card adding the icon, target is card receiving the icon."
  [state side card target char color]
  ;; add icon
  (set-prop state side target :icon {:char char :color color :card card})
  ;; specify icon target on card
  (set-prop state side card :icon-target target))

(defn remove-icon
  "Remove the icon associated with the card and target."
  ([state side card] (remove-icon state side card (:icon-target card)))
  ([state side card target]
   (when target (set-prop state side (find-latest state target) :icon nil))
   (set-prop state side (find-latest state card) :icon-target nil)))
