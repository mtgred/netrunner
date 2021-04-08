(ns game.core.diffs
  (:require
    [differ.core :as differ]
    [cond-plus.core :refer [cond+]]
    [game.core.card :refer [is-public? private-card]]
    [game.utils :refer [dissoc-in prune-null-fields]]))

(defn card-summary
  [card side]
  (cond+
    [(not (is-public? card side))
     (prune-null-fields (private-card card))]
    [(:hosted card)
     (update card :hosted (partial mapv #(card-summary % side)))]
    [:else
     (prune-null-fields card)]))

(defn card-summary-vec
  [cards side]
  (mapv #(card-summary % side) cards))

(def player-keys
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
  [player side]
  (-> (select-keys player player-keys)
      (update :identity prune-null-fields)
      (update :current card-summary-vec side)
      (update :play-area card-summary-vec side)
      (update :rfg card-summary-vec side)
      (update :scored card-summary-vec side)
      (update :register select-keys [:spent-click])))

(def corp-keys
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
              (recur (update-in corp zone card-summary-vec :runner)
                     (next zones)))))))))

(defn corp-summary
  [state side]
  (let [corp-player? (= side :corp)
        corp (:corp @state)
        view-deck (:view-deck corp)
        deck (:deck corp)
        hand (:hand corp)
        discard (:discard corp)
        install-list (:install-list corp)]
    (-> (player-summary corp side)
        (merge (select-keys corp corp-keys))
        (assoc
          :deck (if (and corp-player? view-deck) deck [])
          :deck-count (count deck)
          :hand (if corp-player? hand [])
          :hand-count (count hand)
          :discard (card-summary-vec discard :corp)
          :servers (servers-summary state side))
        (cond-> (and corp-player? install-list) (assoc :install-list install-list)))))

(def runner-keys
  [:rig
   :run-credit
   :link
   :tag
   :memory
   :brain-damage])

(defn rig-summary
  [state side]
  (let [runner-player? (= side :runner)
        runner (:runner @state)]
    (into {} (for [row [:hardware :facedown :program :resource]
                   :let [cards (get-in runner [:rig row])]]
               [row (card-summary-vec cards :runner)]))))

(defn runner-summary
  [state side]
  (let [runner-player? (= side :runner)
        runner (:runner @state)
        view-deck (:view-deck runner)
        deck (:deck runner)
        hand (:hand runner)
        discard (:discard runner)
        runnable-list (:runnable-list runner)]
    (-> (player-summary runner side)
        (merge (select-keys runner runner-keys))
        (assoc
          :deck (if (and runner-player? view-deck) deck [])
          :deck-count (count deck)
          :hand (if runner-player? hand [])
          :hand-count (count hand)
          :discard discard
          :rig (rig-summary state side))
        (cond-> (and runner-player? runnable-list) (assoc :runnable-list runnable-list)))))

(def run-keys
  [:server
   :position
   :corp-auto-no-action
   :jack-out
   :jack-out-after-pass
   :phase
   :next-phase
   :source-card])

(defn run-summary
  [state]
  (when-let [run (:run @state)]
    (select-keys run run-keys)))

(def state-keys
  [:active-player
   :corp
   :end-turn
   :gameid
   :log
   :options
   :psi
   :room
   :run
   :runner
   :sfc-current-id
   :sfx
   :sfx-current-id
   :start-date
   :stats
   :trace
   :turn
   :typing])

(defn state-summary
  [state stripped-state side]
  (-> stripped-state
      (assoc :corp (corp-summary state side))
      (assoc :runner (runner-summary state side))))

(defn strip-state
  [state]
  (-> (select-keys @state state-keys)
      (assoc :run (run-summary state))))

(defn strip-for-spectators
  [stripped-state corp-player runner-player]
  (let [spectator? (get-in stripped-state [:options :spectatorhands])]
    (-> stripped-state
        (assoc :corp (:corp corp-player)
               :runner (:runner runner-player))
        (update-in [:corp :hand] #(if spectator? % []))
        (update-in [:runner :hand] #(if spectator? % [])))))

(defn strip-for-replay
  [stripped-state]
  (-> stripped-state
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
        corp-player (state-summary state stripped-state :corp)
        runner-player (state-summary state stripped-state :runner)]
    ;; corp, runner, spectator, history
    [corp-player
     runner-player
     (strip-for-spectators stripped-state corp-player runner-player)
     (strip-for-replay stripped-state)]))

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
