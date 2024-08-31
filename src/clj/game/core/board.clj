(ns game.core.board
  (:require
   [clojure.string :as string]
   [game.core.card :refer [agenda? asset? corp? event? facedown? installed?
                           is-type? operation? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.eid :refer [make-eid]]
   [game.core.servers :refer [is-remote? zones->sorted-names]]
   [game.utils :refer [dissoc-in to-keyword]]))

(defn corp-servers-cards [state]
  (for [server (vals (:servers (:corp @state)))
        installed-card (into (:content server) (:ices server))]
    installed-card))

(defn runner-rig-cards [state]
  (for [row (vals (:rig (:runner @state)))
        installed-card row]
    installed-card))

(defn get-all-cards
  "Every single card in the game. All cards in the hand, deck, discard, play-area, set-aside,
  score zone, currents, and removed from the game. And all cards that are installed and hosted"
  [state]
  (let [installed-corp (corp-servers-cards state)
        installed-runner (runner-rig-cards state)
        corp (:corp @state)
        runner (:runner @state)
        cards-in-zones (for [side [corp runner]
                             zone [:deck :hand :discard :current :scored :play-area :rfg :set-aside]
                             card (zone side)]
                         card)
        identities (list (:identity corp) (:identity runner))]
    (loop [checked (transient [])
           unchecked (concat installed-corp installed-runner cards-in-zones identities)]
      (if (empty? unchecked)
        (persistent! checked)
        (let [[card & remaining] unchecked]
          (if card
            (recur (conj! checked card) (into remaining (:hosted card)))
            (recur checked remaining)))))))

(defn all-installed-runner
  [state]
  (let [installed-cards (runner-rig-cards state)
        hosted-on-corp-cards (mapcat :hosted (corp-servers-cards state))]
    (loop [installed (transient [])
           unchecked (into installed-cards hosted-on-corp-cards)]
      (if (empty? unchecked)
        (persistent! installed)
        (let [[card & remaining] unchecked]
          (recur
            (if (and (runner? card)
                     (installed? card))
              (conj! installed card)
              installed)
            (into remaining (:hosted card))))))))

(defn all-installed-corp
  [state]
  (let [installed-cards (corp-servers-cards state)
        hosted-on-runner-cards (mapcat :hosted (runner-rig-cards state))]
    (loop [installed (transient [])
           unchecked (into installed-cards hosted-on-runner-cards)]
      (if (empty? unchecked)
        (persistent! installed)
        (let [[card & remaining] unchecked]
          (recur
            (if (and (corp? card)
                     (installed? card))
              (conj! installed card)
              installed)
            (into remaining (:hosted card))))))))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :runner)
    (all-installed-runner state)
    (all-installed-corp state)))

(defn all-installed-and-scored
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
   but not including 'inactive hosting' like Personal Workshop, and the cards in the given side's scored area."
  [state side]
  (concat (all-installed state side)
          (-> @state side :scored)))

(defn get-all-installed
  "Returns a list of all installed cards"
  [state]
  (let [installed-runner-cards (runner-rig-cards state)
        installed-corp-cards (corp-servers-cards state)]
    (loop [installed (transient [])
           unchecked (concat installed-runner-cards
                             installed-corp-cards)]
      (if (empty? unchecked)
        (persistent! installed)
        (let [[card & remaining] unchecked]
          (recur
            (if (installed? card)
              (conj! installed card)
              installed)
            (into (vec remaining) (:hosted card))))))))

(defn all-installed-runner-type
  "Returns a list of all installed, non-facedown runner cards of the requested type."
  [state card-type]
  (filter #(and (is-type? % card-type) (not (facedown? %))) (all-installed state :runner)))

(defn all-active-installed
  "Returns a vector of active AND installed cards for the given side. This is all face-up installed cards."
  [state side]
  (let [installed (all-installed state side)]
    (if (= side :runner)
      (remove facedown? installed)
      (filter rezzed? installed))))

(defn all-active
  "Returns a sequence of all active cards for the given side. Active cards are either installed, the identity,
  currents, or the corp's scored area."
  [state side]
  (->> (concat [(-> @state side :identity)]
               (all-active-installed state side)
               (-> @state side :current)
               (filter (if (= :corp side) operation? event?)
                       (-> @state side :play-area))
               (when (= side :corp)
                 (-> @state :corp :scored)))
       (filter identity)
       (remove :disabled)))

(defn installed-byname
  "Returns a truthy card map if a card matching title is installed"
  [state side title]
  (some #(when (= (:title %) title) %) (all-active-installed state side)))

(defn in-play?
  "Returns a truthy card map if the given card is in play (installed)."
  [state card]
  (installed-byname state (to-keyword (:side card)) (:title card)))

;; zone stuff
(defn get-zones [state]
  (keys (get-in @state [:corp :servers])))

(defn get-remote-zones [state]
  (filter is-remote? (get-zones state)))

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
  (let [max-servers (when-not (get (:disabled-card-reg @state) (get-in @state [:corp :identity :cid]))
                      (get-in (card-def (get-in @state [:corp :identity])) [:flags :server-limit]))
        at-remote-limit? (and max-servers (>= (count (get-remotes state)) max-servers))
        hosts (filter #(when-let [can-host (:can-host (card-def %))]
                         (and (rezzed? %)
                              (can-host state :corp (make-eid state) % [card])))
                      (all-installed state :corp))
        base-list (concat hosts (server-list state) (when-not at-remote-limit? ["New remote"]))]
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
    (if (:cid server)
      [:onhost]
      (case server
        "HQ" [:servers :hq]
        "R&D" [:servers :rd]
        "Archives" [:servers :archives]
        "New remote" [:servers (keyword (str "remote" (:rid @state)))]
        [:servers (->> (string/split server #" ") last (str "remote") keyword)]))))

(defn card->server
  "Returns the server map that this card is installed in or protecting."
  [state card]
  (let [z (:zone card)]
    (get-in @state [:corp :servers (second z)])))

(defn clear-empty-remotes
  [state]
  (doseq [remote (get-remotes state)]
    (let [zone [:corp :servers (first remote)]]
      (when (and (empty? (get-in @state (conj zone :content)))
                 (empty? (get-in @state (conj zone :ices))))
        (swap! state dissoc-in zone)))))
