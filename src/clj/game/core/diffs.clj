(ns game.core.diffs
  (:require
   [cond-plus.core :refer [cond+]]
   [differ.core :as differ]
   [game.core.board :refer [installable-servers]]
   [game.core.card :refer :all]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.engine :refer [can-trigger?]]
   [game.core.effects :refer [any-effects is-disabled-reg? get-effects]]
   [game.core.installing :refer [corp-can-pay-and-install?
                                 runner-can-pay-and-install?]]
   [game.core.payment :refer [can-pay? ->c]]
   [game.core.play-instants :refer [can-play-instant?]]
   [game.core.winning :refer [agenda-points-required-to-win]]
   [game.utils :refer [dissoc-in]]
   [jinteki.utils :refer [select-non-nil-keys]]
   [medley.core :refer [update-existing]]))

(defn playable? [card state side]
  (if (and ((if (= :corp side) corp? runner?) card)
           (or (in-hand? card)
               (any-effects state side :can-play-as-if-in-hand true? card)
               (:as-flashback card))
           (not (:corp-phase-12 @state))
           (not (:runner-phase-12 @state))
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
              (and (not (:run @state))
                   (runner-can-pay-and-install?
                     state :runner {:source card :source-type :runner-install}
                     card {:base-cost [(->c :click 1)]
                           :no-toast true}))]
             [(or (event? card)
                  (operation? card))
              (and (not (:run @state))
                   (can-play-instant?
                     state side {:source card :source-type :play}
                     card {:base-cost (if-not (:as-flashback card)
                                        [(->c :click 1)]
                                        (:flashback (card-def card)))
                           :silent true}))])
           true)
    (assoc card :playable true)
    card))

(defn flashback-playable? [card state side]
  (if-let [flashback-cost (when (in-discard? card) (:flashback (card-def card)))]
    (let [adjusted-card (assoc card :as-flashback true)]
      (assoc card :flashback-playable (:playable (playable? adjusted-card state side))))
    card))

(defn playable-as-if-in-hand? [card state side]
  (if (any-effects state side :can-play-as-if-in-hand true? card)
    (assoc card :playable-as-if-in-hand true)
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
             ;; actions cannot be used during runs
             (not (and (:action ability)
                       (:run @state)))
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

(defn icon-summary [card state]
  (if-let [icons (seq (get-effects state nil :icon card))]
    (assoc card :icon (vec icons))
    card))

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
   :flashback-playable
   :host
   :hosted
   :icon
   :images
   :implementation
   :installed
   :new
   :normalizedtitle
   :playable
   :playable-as-if-in-hand
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
        (flashback-playable? state side)
        (playable-as-if-in-hand? state side)
        (card-abilities-summary state side)
        (icon-summary state)
        (select-non-nil-keys card-keys))
    (-> (cond-> card
          (:host card) (-> (dissoc-in [:host :hosted])
                           (update :host card-summary state side))
          (:hosted card) (update :hosted cards-summary state side))
        (icon-summary state)
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
   :selectable
   :eid
   ;; traces
   :player
   :base
   :bonus
   :strength
   :unbeatable
   :beat-trace
   :link
   :corp-credits
   :runner-credits])

(defn prompt-summary
  [prompt same-side?]
  (when same-side?
    (-> prompt
        (update :eid #(when (:eid %) (select-keys % [:eid])))
        (update :card #(not-empty (select-non-nil-keys % card-keys)))
        (update :choices (fn [choices]
                           (if (sequential? choices)
                             (->> choices
                                  (mapv
                                   (fn [choice]
                                     (if (-> choice :value :cid)
                                       (update choice :value select-non-nil-keys card-keys)
                                       choice)))
                                  (not-empty))
                             choices)))
        (select-non-nil-keys prompt-keys)
        (not-empty))))

(defn toast-summary
  [toast same-side?]
  (when same-side?
    toast))

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
   :destroyed
   :click
   :credit
   :toast
   :hand-size
   :keep
   :quote
   :properties
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
        (assoc :agenda-point-req (agenda-points-required-to-win state :corp))
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
        (assoc :agenda-point-req (agenda-points-required-to-win state :runner))
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
   :corp-card-sleeve
   :runner-card-sleeve
   :language
   :card-language
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
        (assoc :cannot-jack-out (any-effects state :corp :cannot-jack-out true?))
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
   ;; :angel-arena-info
   :corp
   :corp-phase-12
   :corp-post-discard
   :decklists
   :encounters
   :end-turn
   :forced-encounter
   :gameid
   :last-revealed
   :log
   :mark
   :options
   :psi
   :reason
   :room
   :run
   :runner
   :runner-phase-12
   :runner-post-discard
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
      (assoc :stats (when (:winner @state) (:stats @state)))
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

(defn strip-for-corp-spect
  [stripped-state corp-state runner-state]
  (assoc stripped-state :corp (:corp corp-state) :runner (:runner corp-state)))

(defn strip-for-runner-spect
  [stripped-state corp-state runner-state]
  (assoc stripped-state :corp (:corp runner-state) :runner (:runner runner-state)))

(defn public-states
  "Generates privatized states for the Corp, Runner, any spectators, and the history from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well.
  note that when joining or starting a game, all states are always generated.
  Otherwise when computing diffs, only the relevant states are needed, and we can skip computing the other ones."
  ([state] (public-states state true true true))
  ([state spectators? corp-spectators? runner-spectators?]
   (let [stripped-state (strip-state state)
         corp-state (state-summary stripped-state state :corp)
         runner-state (state-summary stripped-state state :runner)
         replay-state (strip-for-replay stripped-state corp-state runner-state)]
     ;; corp, runner, spectator, history
     {:corp-state corp-state
      :runner-state runner-state
      :spect-state (when spectators? (strip-for-spectators replay-state corp-state runner-state))
      :corp-spect-state (when corp-spectators? (strip-for-corp-spect replay-state corp-state runner-state))
      :runner-spect-state (when runner-spectators? (strip-for-runner-spect replay-state corp-state runner-state))
      :hist-state replay-state})))

(defn public-diffs [old-state new-state spectators? corp-spectators? runner-spectators?]
  (let [{old-corp :corp-state old-runner :runner-state
         old-spect :spect-state old-hist :hist-state
         old-corp-spect :corp-spect-state
         old-runner-spect :runner-spect-state} (when old-state (public-states (atom old-state) spectators? corp-spectators? runner-spectators?))
        {new-corp :corp-state new-runner :runner-state
         new-spect :spect-state new-hist :hist-state
         new-corp-spect :corp-spect-state
         new-runner-spect :runner-spect-state} (public-states new-state spectators? corp-spectators? runner-spectators?)]
    {:runner-diff (differ/diff old-runner new-runner)
     :corp-diff (differ/diff old-corp new-corp)
     :spect-diff (when spectators? (differ/diff old-spect new-spect))
     :runner-spect-diff (when runner-spectators? (differ/diff old-runner-spect new-runner-spect))
     :corp-spect-diff (when corp-spectators? (differ/diff old-corp-spect new-corp-spect))
     :hist-diff (differ/diff old-hist new-hist)}))

(defn message-diffs [old-state new-state]
  (let [old-messages (select-keys old-state [:log])
        new-messages (select-keys @new-state [:log])
        message-diff (differ/diff old-messages new-messages)]
    {:runner-diff message-diff
     :corp-diff message-diff
     :spect-diff message-diff
     :runner-spect-diff message-diff
     :corp-spect-diff message-diff
     :hist-diff message-diff}))
