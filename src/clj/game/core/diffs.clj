(ns game.core.diffs
  (:require
    [differ.core :as differ]
    [cond-plus.core :refer [cond+]]
    [game.core.board :refer [installable-servers]]
    [game.core.card :refer :all]
    [game.core.cost-fns :refer [card-ability-cost]]
    [game.core.engine :refer [can-trigger?]]
    [game.core.installing :refer [corp-can-pay-and-install? runner-can-pay-and-install?]]
    [game.core.payment :refer [can-pay?]]
    [game.core.play-instants :refer [can-play-instant?]]
    [game.utils :refer [dissoc-in prune-null-fields]]))

(defn playable? [card state side]
  (if (and ((if (= :corp side) corp? runner?) card)
           (in-hand? card)
           (cond+
             [(or (agenda? card)
                  (asset? card)
                  (ice? card)
                  (upgrade? card))
              (some
                (fn [server]
                  (corp-can-pay-and-install?
                    state :corp {:source server :source-type :corp-install}
                    card server {:base-cost [:click 1]
                                 :action :corp-click-install
                                 :no-toast true}))
                (installable-servers state card))]
             [(or (hardware? card)
                  (program? card)
                  (resource? card))
              (runner-can-pay-and-install?
                state :runner {:source :action :source-type :runner-install}
                card {:base-cost [:click 1]
                      :no-toast true})]
             [(or (event? card)
                  (operation? card))
              (can-play-instant?
                state side {:source :action :source-type :play}
                card {:base-cost [:click 1]})])
           true)
    (assoc card :playable true)
    card))

(defn ability-playable? [state side card ability-idx ability]
  (let [cost (card-ability-cost state side ability card)
        eid {:source card
             :source-type :ability
             :source-info {:ability-idx ability-idx}}]
    (if (and (can-pay? state side eid card nil cost)
             (can-trigger? state side eid ability card nil))
      (assoc ability :playable true)
      ability)))

(defn abilities-playable? [state side card ability-kw]
  (->> (get card ability-kw)
       (map-indexed (partial ability-playable? state side card))
       (into [])))

(defn card-abilities-playable? [card state side]
  (if (or (active? card)
          (is-type? card "Basic Action"))
    (-> card
        (assoc :abilities (abilities-playable? state side card :abilities))
        (assoc :corp-abilities (abilities-playable? state side card :corp-abilities))
        (assoc :runner-abilities (abilities-playable? state side card :runner-abilities)))
    card))

(defn card-summary [card state side]
  (cond+
    [(not (is-public? card side))
     (prune-null-fields (private-card card))]
    [(:hosted card)
     (update card :hosted (partial mapv #(card-summary % state side)))]
    [:else
     (-> card
         (playable? state side)
         (card-abilities-playable? state side)
         (prune-null-fields))]))

(defn card-summary-vec [cards state side]
  (mapv #(card-summary % state side) cards))

(defn prune-vec [cards]
  (mapv prune-null-fields cards))

(defn player-keys []
  [:aid
   :user
   :identity
   :basic-action-card
   :deck
   :deck-id
   :hand
   :discard
   :scored
   :rfg
   :play-area
   :current
   :click
   :credit
   :toast
   :hand-size
   :keep
   :quote
   :register
   :prompt
   :prompt-state
   :agenda-point
   :agenda-point-req])

(defn player-summary
  [player state side]
  (-> (select-keys player (player-keys))
      (update :identity prune-null-fields)
      (update :basic-action-card card-abilities-playable? state side)
      (update :current card-summary-vec state side)
      (update :play-area card-summary-vec state side)
      (update :rfg card-summary-vec state side)
      (update :scored card-summary-vec state side)
      (update :register select-keys [:spent-click])))

(defn corp-keys []
  [:servers
   :bad-publicity])

(defn servers-summary
  [state side]
  (let [corp-player? (= side :corp)
        corp (:corp @state)]
    (if corp-player?
      (:servers corp)
      (let [server-keys (keys (:servers corp))
            zones (reduce
                    (fn [servers server]
                      (into servers [[:servers server :content]
                                     [:servers server :ices]]))
                    []
                    server-keys)]
        (loop [corp corp
               zones zones]
          (let [zone (first zones)]
            (if (nil? zone)
              (:servers corp)
              (recur (update-in corp zone card-summary-vec state :runner)
                     (next zones)))))))))

(defn corp-summary
  [state side]
  (let [corp-player? (= side :corp)
        corp (:corp @state)
        view-deck (:view-deck corp)
        deck (:deck corp)
        hand (:hand corp)
        open-hands? (:openhand corp)
        discard (:discard corp)
        install-list (:install-list corp)]
    (-> (player-summary corp state side)
        (merge (select-keys corp (corp-keys)))
        (assoc
          :deck (if (and corp-player? view-deck) (prune-vec deck) [])
          :deck-count (count deck)
          :hand (if (or corp-player? open-hands?) (card-summary-vec hand state :corp) [])
          :hand-count (count hand)
          :discard (card-summary-vec discard state :corp)
          :servers (servers-summary state side))
        (cond-> (and corp-player? install-list) (assoc :install-list install-list)))))

(defn runner-keys []
  [:rig
   :run-credit
   :link
   :tag
   :memory
   :brain-damage])

(defn rig-summary
  [state side]
  (let [runner (:runner @state)]
    (into {} (for [row [:hardware :facedown :program :resource]
                   :let [cards (get-in runner [:rig row])]]
               [row (card-summary-vec cards state :runner)]))))

(defn runner-summary
  [state side]
  (let [runner-player? (= side :runner)
        runner (:runner @state)
        view-deck (:view-deck runner)
        deck (:deck runner)
        hand (:hand runner)
        open-hands? (:openhand runner)
        discard (:discard runner)
        runnable-list (:runnable-list runner)]
    (-> (player-summary runner state side)
        (merge (select-keys runner (runner-keys)))
        (assoc
          :deck (if (and runner-player? view-deck) (prune-vec deck) [])
          :deck-count (count deck)
          :hand (if (or runner-player? open-hands?) (card-summary-vec hand state :runner) [])
          :hand-count (count hand)
          :discard (prune-vec discard)
          :rig (rig-summary state side))
        (cond-> (and runner-player? runnable-list) (assoc :runnable-list runnable-list)))))

(defn run-keys []
  [:server
   :position
   :corp-auto-no-action
   :jack-out
   :jack-out-after-pass
   :phase
   :next-phase
   :no-action
   :source-card])

(defn run-summary
  [state]
  (when-let [run (:run @state)]
    (select-keys run (run-keys))))

(defn state-keys []
  [:active-player
   :corp
   :corp-phase-12
   :end-turn
   :gameid
   :log
   :options
   :psi
   :reason
   :room
   :run
   :runner
   :runner-phase-12
   :sfx
   :sfx-current-id
   :start-date
   :stats
   :trace
   :turn
   :typing
   :winning-user
   :winner])

(defn strip-state
  [state]
  (-> (select-keys @state (state-keys))
      (assoc :run (run-summary state))))

(defn state-summary
  [stripped-state state side]
  (-> stripped-state
      (assoc :corp (corp-summary state side))
      (assoc :runner (runner-summary state side))))

(defn strip-for-spectators
  [stripped-state corp-player runner-player]
  (let [spectator? (get-in stripped-state [:options :spectatorhands])
        hidden-discard (-> corp-player
                           (get-in [:corp :discard])
                           (card-summary-vec stripped-state :spectator))]
    (-> stripped-state
        (assoc :corp (:corp corp-player)
               :runner (:runner runner-player))
        (update-in [:corp :discard] #(if spectator? % hidden-discard))
        (update-in [:corp :hand] #(if spectator? % []))
        (update-in [:runner :hand] #(if spectator? % [])))))

(defn strip-for-replay
  [stripped-state corp-player runner-player]
  (-> stripped-state
      (assoc :corp (:corp corp-player)
             :runner (:runner runner-player))
      (dissoc-in [:runner :user :isadmin])
      (dissoc-in [:runner :user :options :blocked-users])
      (dissoc-in [:runner :user :stats])
      (dissoc-in [:corp :user :isadmin])
      (dissoc-in [:corp :user :options :blocked-users])
      (dissoc-in [:corp :user :stats])))

(defn private-states
  "Generates privatized states for the Corp, Runner, any spectators, and the history from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well."
  [state]
  (let [stripped-state (strip-state state)
        corp-player (state-summary stripped-state state :corp)
        runner-player (state-summary stripped-state state :runner)]
    ;; corp, runner, spectator, history
    [corp-player
     runner-player
     (strip-for-spectators stripped-state corp-player runner-player)
     (strip-for-replay stripped-state corp-player runner-player)]))

(defn public-states [state]
  (let [[corp-state runner-state spectator-state history-state] (private-states state)]
    {:corp-state corp-state
     :runner-state runner-state
     :spect-state spectator-state
     :hist-state history-state}))

(defn public-diffs [old-state new-state]
  (let [[old-corp old-runner old-spect old-hist] (when old-state (private-states (atom old-state)))
        [new-corp new-runner new-spect new-hist] (private-states new-state)]
    {:runner-diff (differ/diff old-runner new-runner)
     :corp-diff (differ/diff old-corp new-corp)
     :spect-diff (differ/diff old-spect new-spect)
     :hist-diff (differ/diff old-hist new-hist)}))
