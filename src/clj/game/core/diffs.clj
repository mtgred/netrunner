(ns game.core.diffs
  (:require
    [differ.core :as differ]
    [cond-plus.core :refer [cond+]]
    [game.core.card :refer [corp? runner? is-public? private-card]]
    [game.utils :refer [dissoc-in prune-null-fields]]))

; (defn playable? [{:keys [side zone cost type uniqueness] :as card}]
;   (let [my-side (:side @game-state)
;         me (my-side @game-state)]
;     (and (= (keyword (.toLowerCase side)) my-side)

;          (cond

;            (has-subtype? card "Double")
;            (if (>= (:click me) 2) true false)

;            (has-subtype? card "Triple")
;            (if (>= (:click me) 3) true false)

;            (= (:code card) "07036") ; Day Job
;            (if (>= (:click me) 4) true false)

;            (has-subtype? card "Priority")
;            (if (get-in @game-state [my-side :register :spent-click]) false true)

;            :else
;            true)

;          (and (= zone ["hand"])
;               (or (not uniqueness) (not (in-play? card)))
;               (or (#{"Agenda" "Asset" "Upgrade" "ICE"} type) (>= (:credit me) cost))
;               (pos? (:click me))))))

; (defn playable? [card state side]
;   (let [side? (if (= :corp side) corp? runner?)]
;     ()
;     )
;   )

(defn card-summary [card state side]
  (cond+
    [(not (is-public? card side))
     (prune-null-fields (private-card card))]
    [(:hosted card)
     (update card :hosted (partial mapv #(card-summary % side)))]
    [:else
     (-> card
         ; (playable? state side)
         (prune-null-fields))]))

(defn card-summary-vec [cards state side]
  (mapv #(card-summary % state side) cards))

(defn prune-vec [cards]
  (mapv prune-null-fields cards))

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
  [player state side]
  (-> (select-keys player player-keys)
      (update :identity prune-null-fields)
      (update :current card-summary-vec state side)
      (update :play-area card-summary-vec state side)
      (update :rfg card-summary-vec state side)
      (update :scored card-summary-vec state side)
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
        (merge (select-keys corp corp-keys))
        (assoc
          :deck (if (and corp-player? view-deck) (prune-vec deck) [])
          :deck-count (count deck)
          :hand (if (or corp-player? open-hands?) (prune-vec hand) [])
          :hand-count (count hand)
          :discard (card-summary-vec discard state :corp)
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
        (merge (select-keys runner runner-keys))
        (assoc
          :deck (if (and runner-player? view-deck) (prune-vec deck) [])
          :deck-count (count deck)
          :hand (if (or runner-player? open-hands?) (prune-vec hand) [])
          :hand-count (count hand)
          :discard (prune-vec discard)
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
