(in-ns 'game.core)

(declare set-prop all-active-installed)

(defn get-zones [state]
  (keys (get-in @state [:corp :servers])))

(defn get-remote-zones [state]
  (filter is-remote? (get-zones state)))

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

(defn get-remotes [state]
  (select-keys (get-in @state [:corp :servers]) (get-remote-zones state)))

(defn get-remote-names [state]
  (zones->sorted-names (get-remote-zones state)))

(defn server-list
  "Get a list of all servers (including centrals)"
  [state]
  (zones->sorted-names (get-zones state)))

(defn installable-servers
  "Get list of servers the specified card can be installed in"
  [state card]
  (let [base-list (concat (server-list state) ["New remote"])]
    (if-let [install-req (-> card card-def :install-req)]
      ;; Install req function overrides normal list of install locations
      (install-req state :corp card (make-eid state) base-list)
      ;; Standard list
      (if (or (agenda? card)
              (asset? card))
        (remove #{"HQ" "R&D" "Archives"} base-list)
        base-list))))


(defn server->zone [state server]
  (if (sequential? server)
    (vec (cons :servers server))
    (case server
      "HQ" [:servers :hq]
      "R&D" [:servers :rd]
      "Archives" [:servers :archives]
      "New remote" [:servers (keyword (str "remote" (make-rid state)))]
      [:servers (->> (split server #" ") last (str "remote") keyword)])))

(defn unknown->kw
  "Given a string ('Archives'), a keyword corresponding to a server (:archives)
  or a zone ([:servers :archives]), return the keyword.
  NOTE: return keyword even if server does not exist."
  [name-or-kw-or-zone]
  (cond
    (keyword? name-or-kw-or-zone)
    name-or-kw-or-zone

    (string? name-or-kw-or-zone)
    (case name-or-kw-or-zone
      "HQ" :hq
      "R&D" :rd
      "Archives" :archives
      ;; assume "Server N"
      (->> (split name-or-kw-or-zone #" ") last (str "remote") keyword))

    :else
    (second name-or-kw-or-zone)))

(defn same-server?
  "True if the two cards are IN or PROTECTING the same server."
  [card1 card2]
  (and card1
       card2
       (let [zone1 (get-nested-zone card1)
             zone2 (get-nested-zone card2)]
         (= (second zone1) (second zone2)))))

(defn protecting-same-server?
  "True if an ice is protecting the server that the card is in or protecting."
  [card ice]
  (and card
       ice
       (let [zone1 (get-nested-zone card)
             zone2 (get-nested-zone ice)]
         (and (= (second zone1) (second zone2))
              (= :ices (last zone2))))))

(defn in-same-server?
  "True if the two cards are installed IN the same server, or hosted on cards IN the same server."
  [card1 card2]
  (let [zone1 (get-nested-zone card1)
        zone2 (get-nested-zone card2)]
    (and card1
         card2
         (= zone1 zone2)
         (is-remote? (second zone1)) ; cards in centrals are in the server's root, not in the server.
         (= :content (last zone1)))))

(defn from-same-server?
  "True if the upgrade is in the root of the server that the target is in."
  [upgrade target]
  (and upgrade
       target
       (= (central->zone (:zone target))
          (butlast (get-nested-zone upgrade)))))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :runner)
    (let [top-level-cards (flatten (for [t [:program :hardware :resource :facedown]] (get-in @state [:runner :rig t])))
          hosted-on-ice (->> (:corp @state) :servers seq flatten (mapcat :ices) (mapcat :hosted))]
      (loop [unchecked (concat top-level-cards (filter runner? hosted-on-ice)) installed ()]
        (if (empty? unchecked)
          (filter installed? installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))
    (let [servers (->> (:corp @state) :servers seq flatten)
          content (mapcat :content servers)
          ice (mapcat :ices servers)
          top-level-cards (concat ice content)]
      (loop [unchecked top-level-cards installed ()]
        (if (empty? unchecked)
          (filter corp? installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))))

(defn get-all-installed
  "Returns a list of all installed cards"
  [state]
  (concat (all-installed state :corp) (all-installed state :runner)))

(defn all-installed-runner-type
  "Returns a list of all installed, non-facedown runner cards of the requested type."
  [state card-type]
  (filter (every-pred #(is-type? % card-type) (complement facedown?)) (all-installed state :runner)))

(defn number-of-virus-counters
  "Returns number of actual virus counters (excluding virtual counters from Hivemind)"
  [state]
  (reduce + (map #(get-counters % :virus) (get-all-installed state))))

(defn all-active
  "Returns a vector of all active cards for the given side. Active cards are either installed, the identity,
  currents, or the corp's scored area."
  [state side]
  (if (= side :runner)
    (cons (get-in @state [:runner :identity]) (concat (get-in @state [:runner :current])
                                                      (all-active-installed state side)
                                                      (get-in @state [:runner :play-area])))
    (cons (get-in @state [:corp :identity]) (remove :disabled
                                                    (concat (all-active-installed state side)
                                                            (get-in @state [:corp :current])
                                                            (get-in @state [:corp :scored])
                                                            (get-in @state [:corp :play-area]))))))

(defn all-active-installed
  "Returns a vector of active AND installed cards for the given side. This is all face-up installed cards."
  [state side]
  (let [installed (all-installed state side)]
   (if (= side :runner)
     (remove facedown? installed)
     (filter rezzed? installed))))

(defn installed-byname
  "Returns a truthy card map if a card matching title is installed"
  [state side title]
  (some #(when (= (:title %) title) %) (all-active-installed state side)))

(defn in-play?
  "Returns a truthy card map if the given card is in play (installed)."
  [state card]
  (installed-byname state (to-keyword (:side card)) (:title card)))

;;; Stuff for handling {:base x :mod y} data structures

(defn base-mod-size
  "Returns the value of properties using the `base` and `mod` system"
  [state side prop]
  (let [base (get-in @state [side prop :base] 0)
        mod (get-in @state [side prop :mod] 0)]
    (+ base mod)))

(defn hand-size
  "Returns the current maximum hand-size of the specified side."
  [state side]
  (base-mod-size state side :hand-size))

(defn available-mu
  "Returns the available MU the runner has"
  [state]
  (- (base-mod-size state :runner :memory)
     (get-in @state [:runner :memory :used] 0)))

(defn toast-check-mu
  "Check runner has not exceeded, toast if they have"
  [state]
  (when (neg? (available-mu state))
    (toast state :runner "You have exceeded your memory units!")))

(defn free-mu
  "Frees up specified amount of mu (reduces :used)"
  ([state _ n] (free-mu state n))
  ([state n]
   (deduct state :runner [:memory {:used n}])))

(defn use-mu
  "Increases amount of mu used (increased :used)"
  ([state _ n] (use-mu state n))
  ([state n]
   (gain state :runner :memory {:used n})))

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
  "Removes the old current when a new one is played, or an agenda is stolen / scored"
  [state side current-side]
  (when-let [current (first (get-in @state [current-side :current]))] ; trash old current
    (trigger-event state side :trash-current current)
    (unregister-constant-effects state side current)
    (let [current (get-card state current)]
      (if (get-in current [:special :rfg-when-trashed])
        (do (system-say state side (str (:title current) " is removed from the game."))
            (move state (other-side side) current :rfg))
        (do (system-say state side (str (:title current) " is trashed."))
            (trash state (to-keyword (:side current)) current))))))

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
