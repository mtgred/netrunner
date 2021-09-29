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
    [game.utils :refer [dissoc-in prune-null-fields select-non-nil-keys]]))

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
                card {:base-cost [:click 1] :silent true})])
           true)
    (assoc card :playable true)
    card))

(defn ability-playable? [state side card ability-idx ability]
  (let [cost (card-ability-cost state side ability card)
        eid {:source card
             :source-type :ability
             :source-info {:ability-idx ability-idx}}]
    (if (and (or (active? card)
                 (:autoresolve ability))
             (can-pay? state side eid card nil cost)
             (can-trigger? state side eid ability card nil))
      (assoc ability :playable true)
      ability)))

(defn abilities-playable? [state side card ability]
  (->> ability
       (map-indexed (fn [ab-idx ab] (ability-playable? state side card ab-idx ab)))
       (into [])))

(defn card-abilities-playable? [card state side]
  (cond-> card
    (:abilities card)
    (assoc :abilities (abilities-playable? state side card (:abilities card)))
    (:corp-abilities card)
    (assoc :corp-abilities (abilities-playable? state side card (:corp-abilities card)))
    (:runner-abilities card)
    (assoc :runner-abilities (abilities-playable? state side card (:runner-abilities card)))))

(def card-keys
  [:abilities
   :advancementcost
   :agendapoints
   :baselink
   :cid
   :code
   :corp-abilities
   :cost
   :faction
   :hosted
   :implementation
   :images
   :index
   :installed
   :memoryunits
   :new
   :normalizedtitle
   :previous-zone
   :runner-abilities
   :side
   :strength
   :subroutines
   :subtypes
   :title
   :trash
   :type
   :uniqueness
   :zone])

(declare card-summary-vec)

(defn card-summary [card state side]
  (if (not (is-public? card side))
    (prune-null-fields (private-card card))
    (-> (if (:hosted card)
          (update card :hosted (partial mapv #(card-summary % state side)))
          card)
        (playable? state side)
        (card-abilities-playable? state side)
        (prune-null-fields))))

(defn card-summary-2 [card state side]
  (if (not (is-public? card side))
    (prune-null-fields (private-card card))
    (-> (if (:hosted card) (update card :hosted card-summary-vec state side) card)
        (playable? state side)
        (card-abilities-playable? state side)
        (select-non-nil-keys card-keys))))

(defn card-summary-vec [cards state side]
  (mapv #(card-summary % state side) cards))

(defn prune-card-vec [cards]
  (mapv #(select-non-nil-keys % card-keys) cards))

(def prompt-keys
  [:msg
   :choices
   :card
   :prompt-type
   :show-discard
   ;; traces
   :player
   :base
   :bonus
   :strength
   :link
   :corp-credits
   :runner-credits])

(defn build-prompt-state
  [prompt]
  (not-empty (select-non-nil-keys prompt prompt-keys)))

(defn prompt-summary
  [player same-side?]
  (update player :prompt-state #(if same-side? (build-prompt-state %) nil)))

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
   :current
   :click
   :credit
   :toast
   :hand-size
   :keep
   :quote
   :prompt-state
   :agenda-point
   :agenda-point-req])

(defn player-summary
  [player state side same-side? additional-keys]
  (-> player
      (update :identity card-summary state side)
      (update :basic-action-card card-abilities-playable? state side)
      (update :current card-summary-vec state side)
      (update :play-area card-summary-vec state side)
      (update :rfg card-summary-vec state side)
      (update :scored card-summary-vec state side)
      (prompt-summary same-side?)
      (select-non-nil-keys (into player-keys additional-keys))))

(def corp-keys
  [:servers
   :bad-publicity])

(defn servers-summary
  [state side]
  (reduce-kv
    (fn [servers current-server-kw current-server]
      (assoc servers
             current-server-kw
             {:content (card-summary-vec (:content current-server) state side)
              :ices (card-summary-vec (:ices current-server) state side)}))
    {}
    (:servers (:corp @state))))

(defn corp-summary
  [state side]
  (let [corp-player? (= side :corp)
        corp (:corp @state)
        install-list (:install-list corp)]
    (-> (player-summary corp state side corp-player? corp-keys)
        (update :deck #(if (and corp-player? (:view-deck corp)) (prune-card-vec %) []))
        (update :hand #(if (or corp-player? (:openhand corp)) (card-summary-vec % state :corp) []))
        (update :discard card-summary-vec state :corp)
        (assoc
          :deck-count (count (:deck corp))
          :hand-count (count (:hand corp))
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
  (-> (:rig (:runner @state))
      (update :hardware card-summary-vec state side)
      (update :facedown card-summary-vec state side)
      (update :program card-summary-vec state side)
      (update :resource card-summary-vec state side)))

(defn runner-summary
  [state side]
  (let [runner-player? (= side :runner)
        runner (:runner @state)
        runnable-list (:runnable-list runner)]
    (-> (player-summary runner state side runner-player? runner-keys)
        (update :deck #(if (and runner-player? (:view-deck runner)) (prune-card-vec %) []))
        (update :hand #(if (or runner-player? (:openhand runner)) (card-summary-vec % state :runner) []))
        (update :discard prune-card-vec)
        (assoc
          :deck-count (count (:deck runner))
          :hand-count (count (:hand runner))
          :rig (rig-summary state side))
        (cond-> (and runner-player? runnable-list) (assoc :runnable-list runnable-list)))))

(def run-keys
  [:server
   :position
   :corp-auto-no-action
   :cannot-jack-out
   :phase
   :next-phase
   :no-action
   :source-card])

(defn run-summary
  [state]
  (when-let [run (:run @state)]
    (select-non-nil-keys run run-keys)))

(def encounter-ice-keys
  [:cid
   :current-strength
   :host
   :hosted
   :rezzed
   :side
   :strength
   :subroutines
   :subtypes
   :title
   :type
   :zone])

(defn encounter-ice-summary
  [ice state]
  (select-non-nil-keys (get-card state ice) encounter-ice-keys))

(def encounter-keys
  [:ice
   :no-action])

(defn encounters-summary
  [state]
  (let [encounters (:encounters @state)
        current-encounter (peek encounters)
        encounter-count (count encounters)]
    (when current-encounter
      (-> (select-non-nil-keys current-encounter encounter-keys)
          (update :ice encounter-ice-summary state)
          (assoc :encounter-count encounter-count)))))

(def state-keys
  [:active-player
   :angel-arena-info
   :corp
   :corp-phase-12
   :encounters
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
  (-> (select-non-nil-keys @state state-keys)
      (cond->
        (:run @state) (assoc :run (run-summary state))
        (:encounters @state) (assoc :encounters (encounters-summary state)))))

(defn state-summary
  [stripped-state state side]
  (-> stripped-state
      (assoc :corp (corp-summary state side))
      (assoc :runner (runner-summary state side))))

(defn strip-for-spectators
  [state stripped-state corp-player runner-player]
  (let [spectator? (-> stripped-state :options :spectatorhands)]
    (-> stripped-state
        (assoc :corp (:corp corp-player)
               :runner (:runner runner-player))
        (update-in [:corp :discard]
                   #(if spectator? % (-> corp-player
                                         :corp :discard
                                         (card-summary-vec state :spectator))))
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
     (strip-for-spectators state stripped-state corp-player runner-player)
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
