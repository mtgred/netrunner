(ns game.core.diffs
  (:require
   [cond-plus.core :refer [cond+]]
   [differ.core :as differ]
   [game.core.board :refer [installable-servers]]
   [game.core.card :refer :all]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.engine :refer [can-trigger?]]
   [game.core.effects :refer [is-disabled-reg?]]
   [game.core.installing :refer [corp-can-pay-and-install?
                                 runner-can-pay-and-install?]]
   [game.core.payment :refer [can-pay? ->c]]
   [game.core.play-instants :refer [can-play-instant?]]
   [game.utils :refer [dissoc-in]]
   [jinteki.utils :refer [select-non-nil-keys]]
   [medley.core :refer [update-existing]]))

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
                    state :corp {:source card :source-type :corp-install}
                    card server {:base-cost [(->c :click 1)]
                                 :action :corp-click-install
                                 :no-toast true}))
                (installable-servers state card))]
             [(or (hardware? card)
                  (program? card)
                  (resource? card))
              (runner-can-pay-and-install?
                state :runner {:source card :source-type :runner-install}
                card {:base-cost [(->c :click 1)]
                      :no-toast true})]
             [(or (event? card)
                  (operation? card))
              (can-play-instant?
                state side {:source card :source-type :play}
                card {:base-cost [(->c :click 1)]
                      :silent true})])
           true)
    (assoc card :playable true)
    card))

(defn ability-playable? [ability ability-idx state side card]
  (let [cost (card-ability-cost state side ability card)
        eid {:source card
             :source-type :ability
             :source-info {:ability-idx ability-idx}}]
    (if (and (or (active? card)
                 (:autoresolve ability))
             ;; using the reg instead of any-effect here because this function gets called
             ;; a LOT, and this is very close to linear time and should (I think)
             ;; always be correct when this fn is called.
             ;; If that's ever an issue, we can switch this to:
             ;; (is-disabled? state side card)
             ;; --n kelly, apr 2024
             (not (is-disabled-reg? state card))
             (can-pay? state side eid card nil cost)
             (can-trigger? state side eid ability card nil))
      (assoc ability :playable true)
      ability)))

(def ability-keys
  [:cost-label
   :dynamic
   :index
   :keep-menu-open
   :label
   :msg
   :playable
   :source])

(defn ability-summary [state side card ab-idx ability]
  (-> ability
      (ability-playable? ab-idx state side card)
      (select-non-nil-keys ability-keys)))

(defn abilities-summary [abilities card state side]
  (some->> (seq abilities)
           (map-indexed (fn [ab-idx ab] (ability-summary state side card ab-idx ab)))
           (into [])))

(def subroutine-keys
  [:broken
   :fired
   :label
   :msg
   :resolve])

(defn subroutines-summary [subroutines]
  (when (seq subroutines)
    (mapv #(select-non-nil-keys % subroutine-keys) subroutines)))

(defn card-abilities-summary [card state side]
  (cond-> card
    (:abilities card) (update :abilities abilities-summary card state side)
    (:corp-abilities card) (update :corp-abilities abilities-summary card state side)
    (:runner-abilities card) (update :runner-abilities abilities-summary card state side)
    (:subroutines card) (update :subroutines subroutines-summary)))

(def card-keys
  [:abilities
   :advance-counter
   :advanceable
   :advancementcost
   :agendapoints
   :card-target
   :cid
   :code
   :corp-abilities
   :cost
   :counter
   :current-advancement-requirement
   :current-points
   :current-strength
   :disabled
   :extra-advance-counter
   :face
   :faces
   :facedown
   :host
   :hosted
   :icon
   :images
   :implementation
   :installed
   :new
   :normalizedtitle
   :playable
   :printed-title
   :rezzed
   :runner-abilities
   :seen
   :selected
   :side
   :strength
   :subroutines
   :subtype-target
   :poison
   :highlight-in-discard
   :subtypes
   :title
   :type
   :zone])

(def private-card-keys
  [:advance-counter
   :cid
   :counter
   :extra-advance-counter
   :host
   :hosted
   :icon
   :new
   :side
   :zone])

(defn private-card
  "Returns only the public information of a given card when it's in a private state,
  for example, when it's facedown or in the hand"
  [card]
  (select-non-nil-keys card private-card-keys))

(declare cards-summary)

(defn card-summary [card state side]
  (if (is-public? card side)
    (-> (cond-> card
          (:host card) (-> (dissoc-in [:host :hosted])
                           (update :host card-summary state side))
          (:hosted card) (update :hosted cards-summary state side))
        (playable? state side)
        (card-abilities-summary state side)
        (select-non-nil-keys card-keys))
    (-> (cond-> card
          (:host card) (-> (dissoc-in [:host :hosted])
                           (update :host card-summary state side))
          (:hosted card) (update :hosted cards-summary state side))
        (private-card))))

(defn cards-summary [cards state side]
  (when (seq cards)
    (mapv #(card-summary % state side) cards)))

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

(defn prompt-summary
  [prompt same-side?]
  (if same-side?
    (not-empty (select-non-nil-keys prompt prompt-keys))
    nil))

(defn toast-summary
  [toast same-side?]
  (if same-side? toast nil))

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
   :set-aside
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
      (update :basic-action-card card-summary state side)
      (update :current cards-summary state side)
      (update :play-area cards-summary state side)
      (update :rfg cards-summary state side)
      (update :scored cards-summary state side)
      (update :set-aside cards-summary state side)
      (update :prompt-state prompt-summary same-side?)
      (update :toast toast-summary same-side?)
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
             {:content (cards-summary (:content current-server) state side)
              :ices (cards-summary (:ices current-server) state side)}))
    {}
    (:servers (:corp @state))))

(defn prune-cards [cards]
  (mapv #(select-non-nil-keys % card-keys) cards))

(defn deck-summary
  "Is the player's deck publicly visible?"
  [deck same-side? player]
  (if (and same-side? (:view-deck player))
    (prune-cards deck)
    []))

(defn hand-summary
  "Is the player's hand publicly visible?"
  [hand state same-side? side player]
  (if (or same-side? (:openhand player))
    (cards-summary hand state side)
    []))

(defn discard-summary
  [discard state same-side? side player]
  (if (or same-side? (:openhand player))
    (cards-summary discard state :corp)
    (cards-summary discard state side)))

(defn corp-summary
  [corp state side]
  (let [corp-player? (= side :corp)
        install-list (:install-list corp)]
    (-> (player-summary corp state side corp-player? corp-keys)
        (update :deck deck-summary corp-player? corp)
        (update :hand hand-summary state corp-player? :corp corp)
        (update :discard discard-summary state corp-player? side corp)
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
      (update :hardware cards-summary state side)
      (update :facedown cards-summary state side)
      (update :program cards-summary state side)
      (update :resource cards-summary state side)))

(defn runner-summary
  [runner state side]
  (let [runner-player? (= side :runner)
        runnable-list (:runnable-list runner)]
    (-> (player-summary runner state side runner-player? runner-keys)
        (update :deck deck-summary runner-player? runner)
        (update :hand hand-summary state runner-player? :runner runner)
        (update :discard prune-cards)
        (assoc
          :deck-count (count (:deck runner))
          :hand-count (count (:hand runner))
          :rig (rig-summary state side))
        (cond-> (and runner-player? runnable-list) (assoc :runnable-list runnable-list)))))

(def options-keys
  [:alt-arts
   :background
   :card-resolution
   :language
   :pronouns
   :show-alt-art])

(defn options-summary [options]
  (when (seq options)
    (select-non-nil-keys options options-keys)))

(def user-keys
  [:_id
   :username
   :emailhash
   :options
   :special])

(defn user-summary [user]
  (-> user
      (update-existing :options options-summary)
      (select-non-nil-keys user-keys)))

(def run-keys
  [:server
   :position
   :corp-auto-no-action
   :cannot-jack-out
   :phase
   :next-phase
   :no-action
   :source-card
   :approached-ice-in-position?])

(defn run-summary
  [state]
  (when-let [run (:run @state)]
    (-> run
        (assoc :approached-ice-in-position? (when (= :approach-ice (:phase run))
                                              (some? (get-card state (:current-ice run)))))
        (select-non-nil-keys run-keys))))

(defn encounter-ice-summary
  [ice state]
  (when-let [ice (get-card state ice)]
    (card-summary ice state :corp)))

(def encounter-keys
  [:encounter-count
   :ice
   :no-action])

(defn encounters-summary
  [state]
  (let [encounters (:encounters @state)
        current-encounter (peek encounters)
        encounter-count (count encounters)]
    (when current-encounter
      (-> current-encounter
          (update :ice encounter-ice-summary state)
          (assoc :encounter-count encounter-count)
          (select-non-nil-keys encounter-keys)))))

(def state-keys
  [:active-player
   :angel-arena-info
   :corp
   :corp-phase-12
   :encounters
   :end-turn
   :gameid
   :log
   :mark
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
  (-> @state
      (update-in [:corp :user] user-summary)
      (update-in [:runner :user] user-summary)
      (assoc :run (run-summary state))
      (assoc :encounters (encounters-summary state))
      (select-non-nil-keys state-keys)))

(defn state-summary
  [stripped-state state side]
  (-> stripped-state
      (update :corp corp-summary state side)
      (update :runner runner-summary state side)))

(defn strip-for-replay
  [stripped-state corp-player runner-player]
  (assoc stripped-state
         :corp (:corp corp-player)
         :runner (:runner runner-player)))

(defn strip-for-spectators
  [stripped-state corp-state runner-state]
  (let [spectator-hands? (-> stripped-state :options :spectatorhands)]
    (-> stripped-state
        (assoc :corp (if spectator-hands? (:corp corp-state) (:corp runner-state)))
        (assoc :runner (if spectator-hands? (:runner runner-state) (:runner corp-state))))))

(defn public-states
  "Generates privatized states for the Corp, Runner, any spectators, and the history from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well."
  [state]
  (let [stripped-state (strip-state state)
        corp-state (state-summary stripped-state state :corp)
        runner-state (state-summary stripped-state state :runner)
        replay-state (strip-for-replay stripped-state corp-state runner-state)]
    ;; corp, runner, spectator, history
    {:corp-state corp-state
     :runner-state runner-state
     :spect-state (strip-for-spectators replay-state corp-state runner-state)
     :hist-state replay-state}))

(defn public-diffs [old-state new-state]
  (let [{old-corp :corp-state old-runner :runner-state
         old-spect :spect-state old-hist :hist-state} (when old-state (public-states (atom old-state)))
        {new-corp :corp-state new-runner :runner-state
         new-spect :spect-state new-hist :hist-state} (public-states new-state)]
    {:runner-diff (differ/diff old-runner new-runner)
     :corp-diff (differ/diff old-corp new-corp)
     :spect-diff (differ/diff old-spect new-spect)
     :hist-diff (differ/diff old-hist new-hist)}))
