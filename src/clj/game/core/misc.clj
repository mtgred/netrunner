(in-ns 'game.core)

(declare set-prop)

(defn get-zones [state]
  (keys (get-in state [:corp :servers])))

(defn get-remote-zones [state]
  (filter is-remote? (get-zones state)))

(defn get-runnable-zones [state]
  (let [restricted-zones (keys (get-in state [:runner :register :cannot-run-on-server]))]
    (remove (set restricted-zones) (get-zones state))))

(defn get-remotes [state]
  (select-keys (get-in state [:corp :servers]) (get-remote-zones state)))

(defn get-remote-names [state]
  (zones->sorted-names (get-remote-zones state)))

(defn server-list [state card]
  (concat
    (if (#{"Asset" "Agenda"} (:type card))
      (get-remote-names @state)
      (zones->sorted-names (get-zones @state)))
    ["New remote"]))

(defn server->zone [state server]
  (if (sequential? server)
    (vec (cons :servers server))
    (case server
      "HQ" [:servers :hq]
      "R&D" [:servers :rd]
      "Archives" [:servers :archives]
      "New remote" [:servers (keyword (str "remote" (make-rid state)))]
      [:servers (->> (split server #" ") last (str "remote") keyword)])))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :runner)
    (let [top-level-cards (flatten (for [t [:program :hardware :resource]] (get-in @state [:runner :rig t])))
          hosted-on-ice (->> (:corp @state) :servers seq flatten (mapcat :ices) (mapcat :hosted))]
      (loop [unchecked (concat top-level-cards (filter #(= (:side %) "Runner") hosted-on-ice)) installed ()]
        (if (empty? unchecked)
          (filter :installed installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))
    (let [servers (->> (:corp @state) :servers seq flatten)
          content (mapcat :content servers)
          ice (mapcat :ices servers)
          top-level-cards (concat ice content)]
      (loop [unchecked top-level-cards installed ()]
        (if (empty? unchecked)
          (filter #(= (:side %) "Corp") installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))))

(defn all-active
  "Returns a vector of all active cards for the given side. Active cards are either installed, the identity,
  currents, or the corp's scored area."
  [state side]
  (if (= side :runner)
    (cons (get-in @state [:runner :identity]) (concat (get-in @state [:runner :current]) (all-installed state side)))
    (cons (get-in @state [:corp :identity]) (filter #(not (:disabled %))
                                                    (concat (all-installed state side)
                                                            (get-in @state [:corp :current])
                                                            (get-in @state [:corp :scored]))))))

(defn installed-byname
  "Returns a truthy card map if a card matching title is installed"
  [state side title]
  (some #(when (= (:title %) title) %) (all-installed state side)))

(defn in-play?
  "Returns a truthy card map if the given card is in play (installed)."
  [state card]
  (installed-byname state (to-keyword (:side card)) (:title card)))

(defn hand-size
  "Returns the current maximum handsize of the specified side."
  [state side]
  (let [side' (get @state side)
        base (get side' :hand-size-base 0)
        mod (get side' :hand-size-modification 0)]
    (+ base mod)))

(defn swap-agendas
  "Swaps the two specified agendas, first one scored (on corp side), second one stolen (on runner side)"
  [state side scored stolen]
  (let [corp-ap-stolen (get-agenda-points state :corp stolen)
        corp-ap-scored (get-agenda-points state :corp scored)
        runner-ap-stolen (get-agenda-points state :runner stolen)
        runner-ap-scored (get-agenda-points state :runner scored)
        corp-ap-change (- corp-ap-stolen corp-ap-scored)
        runner-ap-change (- runner-ap-scored runner-ap-stolen)]
    ;; Remove end of turn events for swapped out agenda
    (swap! state update-in [:corp :register :end-turn]
           (fn [events] (filter #(not (= (:cid scored) (get-in % [:card :cid]))) events)))
    ;; Move agendas
    (swap! state update-in [:corp :scored]
           (fn [coll] (conj (remove-once #(not= (:cid %) (:cid scored)) coll) stolen)))
    (swap! state update-in [:runner :scored]
           (fn [coll] (conj (remove-once #(not= (:cid %) (:cid stolen)) coll)
                            (if-not (card-flag? scored :has-abilities-when-stolen true)
                              (dissoc scored :abilities :events) scored))))
    ;; Update agenda points
    (gain-agenda-point state :runner runner-ap-change)
    (gain-agenda-point state :corp corp-ap-change)
    ;; Set up abilities and events
    (let [new-scored (find-cid (:cid stolen) (get-in @state [:corp :scored]))]
      (let [abilities (:abilities (card-def new-scored))
            new-scored (merge new-scored {:abilities abilities})]
        (update! state :corp new-scored)
        (when-let [events (:events (card-def new-scored))]
          (register-events state side events new-scored))
        (resolve-ability state side (:swapped (card-def new-scored)) new-scored nil)))
    (let [new-stolen (find-cid (:cid scored) (get-in @state [:runner :scored]))]
      (deactivate state :corp new-stolen))))

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
  ([state side card] (remove-icon state side card (get-card state (:icon-target card))))
  ([state side card target]
   (set-prop state side target :icon nil)
   (set-prop state side card :icon-target nil)))
