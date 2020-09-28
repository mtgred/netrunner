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

