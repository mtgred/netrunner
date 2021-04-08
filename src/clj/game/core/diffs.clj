(ns game.core.diffs
  (:require [game.core.flags :refer [card-is-public?]]
            [game.core.card :refer [private-card]]
            [game.utils :refer [dissoc-in]]
            [differ.core :as differ]))

(defn- private-card-vector [state side cards]
  (mapv (fn [card]
          (cond
            (not (card-is-public? state side card)) (private-card card)
            (:hosted card) (update-in card [:hosted] #(private-card-vector state side %))
            :else card))
        cards))

(def corp-keys
  [:aid
   :user
   :identity
   :options
   :basic-action-card
   :deck
   :deck-id
   :hand
   :discard
   :scored
   :rfg
   :play-area
   :servers
   :click
   :credit
   :bad-publicity
   :toast
   :hand-size
   :agenda-point
   :agenda-point-req
   :keep
   :quote
   :register
   :prompt
   :prompt-state])

(defn servers-summary
  [state side]
  (let [corp-player? (= side :corp)
        corp (:corp @state)
        server-keys (keys (:servers corp))
        zones (reduce
                (fn [servers server]
                  (into servers [[:servers server :content]
                                 [:servers server :ices]]))
                []
                server-keys)]
    (if corp-player?
      (:servers corp)
      (loop [corp corp
             zones zones]
        (let [zone (first zones)]
          (if (nil? zone)
            (:servers corp)
            (recur (update-in corp zone #(private-card-vector state :corp %))
                   (next zones))))))))

(defn corp-summary
  [state side]
  (let [corp-player? (= side :corp)
        corp (:corp @state)
        view-deck (:view-deck corp)
        deck (:deck corp)
        hand (:hand corp)
        discard (:discard corp)
        install-list (:install-list corp)]
    (-> (select-keys corp corp-keys)
        (assoc
          :deck (if (and corp-player? view-deck) deck [])
          :deck-count (count deck)
          :hand (if corp-player? hand [])
          :hand-count (count hand)
          :discard (private-card-vector state :corp discard)
          :servers (servers-summary state side))
        (update :register select-keys [:spent-click])
        (cond-> (and corp-player? install-list) (assoc :install-list install-list)))))

(def runner-keys
  [:aid
   :user
   :identity
   :options
   :basic-action-card
   :deck
   :deck-id
   :hand
   :discard
   :scored
   :rfg
   :play-area
   :rig
   :toast
   :click
   :credit
   :run-credit
   :link
   :tag
   :memory
   :hand-size
   :agenda-point
   :agenda-point-req
   :brain-damage
   :keep
   :quote
   :register
   :prompt
   :prompt-state])

(defn rig-summary
  [state side]
  (let [runner-player? (= side :runner)
        runner (:runner @state)]
    (into {} (for [row [:hardware :facedown :program :resource]
                   :let [cards (get-in runner [:rig row])]]
               [row (private-card-vector state :runner cards)]))))

(defn runner-summary
  [state side]
  (let [runner-player? (= side :runner)
        runner (:runner @state)
        view-deck (:view-deck runner)
        deck (:deck runner)
        hand (:hand runner)
        discard (:discard runner)]
    (-> (select-keys runner runner-keys)
        (assoc
          :deck (if (and runner-player? view-deck) deck [])
          :deck-count (count deck)
          :hand (if runner-player? hand [])
          :hand-count (count hand)
          :discard discard
          :rig (rig-summary state side))
        (update :register select-keys [:spent-click]))))

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
