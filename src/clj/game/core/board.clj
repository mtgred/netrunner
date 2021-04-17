(ns game.core.board
  (:require [clojure.string :as string]
            [game.core.card :refer [agenda? asset? condition-counter? corp? event? facedown? get-counters installed? is-type? operation? rezzed? runner?]]
            [game.core.card-defs :refer [card-def]]
            [game.core.eid :refer [make-eid]]
            [game.core.servers :refer [is-remote? zones->sorted-names]]
            [game.core.state :refer [make-rid]]
            [game.utils :refer [dissoc-in to-keyword]]))

(defn get-all-cards
  "Every single card in the game. All cards in the hand, deck, discard, play-area,
  score zone, currents, and removed from the game. And all cards that are installed and hosted"
  [state]
  (let [installed-corp (for [server-key (keys (get-in @state [:corp :servers]))
                             :let [server (get-in @state [:corp :servers server-key])]
                             installed-card (concat (:content server) (:ices server))]
                         installed-card)
        installed-runner (for [card-type [:program :hardware :resource :facedown]
                               installed-card (get-in @state [:runner :rig card-type])]
                           installed-card)
        cards-in-zones (for [side [:corp :runner]
                             zone [:deck :hand :discard :current :scored :play-area :rfg]
                             card (get-in @state [side zone])]
                         card)
        identities (for [side [:corp :runner]]
                     (get-in @state [side :identity]))]
    (loop [checked '()
           unchecked (concat installed-corp installed-runner cards-in-zones identities)]
      (if (empty? unchecked)
        checked
        (let [[card & remaining] unchecked]
          (recur (conj checked card) (concat remaining (:hosted card))))))))

(defn- all-installed-runner
  [state]
  (let [installed-cards (for [card-type [:program :hardware :resource :facedown]
                              installed-card (get-in @state [:runner :rig card-type])]
                          installed-card)
        hosted-on-corp-cards (for [server-key (keys (get-in @state [:corp :servers]))
                                   :let [server (get-in @state [:corp :servers server-key])]
                                   installed-card (concat (:content server) (:ices server))
                                   hosted-card (:hosted installed-card)]
                               hosted-card)]
    (loop [installed '()
           unchecked (concat installed-cards hosted-on-corp-cards)]
      (if (empty? unchecked)
        (filter #(and (runner? %)
                      (installed? %))
                installed)
        (let [[card & remaining] unchecked]
          (recur (conj installed card) (concat remaining (:hosted card))))))))

(defn- all-installed-corp
  [state]
  (let [installed-cards (for [server-key (keys (get-in @state [:corp :servers]))
                              :let [server (get-in @state [:corp :servers server-key])]
                              installed-card (concat (:content server) (:ices server))]
                          installed-card)
        hosted-on-runner-cards (for [card-type [:program :hardware :resource :facedown]
                                     installed-card (get-in @state [:runner :rig card-type])
                                     hosted-card (:hosted installed-card)]
                                 hosted-card)]
    (loop [installed '()
           unchecked (concat installed-cards hosted-on-runner-cards)]
      (if (empty? unchecked)
        (filter #(and (corp? %)
                      (installed? %))
                installed)
        (let [[card & remaining] unchecked]
          (recur (conj installed card) (concat remaining (:hosted card))))))))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :runner)
    (all-installed-runner state)
    (all-installed-corp state)))

(defn get-all-installed
  "Returns a list of all installed cards"
  [state]
  (concat (all-installed-corp state) (all-installed-runner state)))

(defn all-installed-runner-type
  "Returns a list of all installed, non-facedown runner cards of the requested type."
  [state card-type]
  (filter (every-pred #(is-type? % card-type) (complement facedown?)) (all-installed state :runner)))

(defn all-active-installed
  "Returns a vector of active AND installed cards for the given side. This is all face-up installed cards."
  [state side]
  (let [installed (all-installed state side)]
   (if (= side :runner)
     (remove facedown? installed)
     (filter rezzed? installed))))

(defn all-active
  "Returns a vector of all active cards for the given side. Active cards are either installed, the identity,
  currents, or the corp's scored area."
  [state side]
  (remove
    :disabled
    (concat [(get-in @state [side :identity])]
      (all-active-installed state side)
      (get-in @state [side :current])
      (filter #(or (event? %) (operation? %)) (get-in @state [side :play-area]))
      (when (= side :corp)
        (get-in @state [:corp :scored])))))

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
      "New remote" [:servers (keyword (str "remote" (:rid @state)))]
      [:servers (->> (string/split server #" ") last (str "remote") keyword)])))

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
