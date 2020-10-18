(ns game.diffs
  (:require [game.core :refer [card-is-public?]]
            [game.core.card :refer [private-card]]
            [game.utils :refer [dissoc-in]]
            [differ.core :as differ]))

(defn strip [state]
  (-> state
    (dissoc :eid :events :turn-events :per-turn :prevent :damage :effect-completed :click-state :turn-state)
    (update-in [:corp :register] select-keys [:spent-click])
    (update-in [:runner :register] select-keys [:spent-click])
    (dissoc-in [:corp :register-last-turn])
    (dissoc-in [:runner :register-last-turn])
    (dissoc-in [:run :current-ice])
    (dissoc-in [:run :events])))

(defn- private-card-vector [state side cards]
  (mapv (fn [card]
          (cond
            (not (card-is-public? state side card)) (private-card card)
            (:hosted card) (update-in card [:hosted] #(private-card-vector state side %))
            :else card))
        cards))

(defn- make-private-runner [state]
  (-> (:runner @state)
      (dissoc :runnable-list)
      (update-in [:hand] #(private-card-vector state :runner %))
      (update-in [:discard] #(private-card-vector state :runner %))
      (update-in [:deck] #(private-card-vector state :runner %))
      (update-in [:rig :facedown] #(private-card-vector state :runner %))
      (update-in [:rig :resource] #(private-card-vector state :runner %))))

(defn- make-private-corp [state]
  (let [zones (concat [[:hand]] [[:discard]] [[:deck]]
                      (for [server (keys (:servers (:corp @state)))] [:servers server :ices])
                      (for [server (keys (:servers (:corp @state)))] [:servers server :content]))
        corp (-> (:corp @state)
                 (dissoc :install-list))]
    (loop [s corp
           z zones]
      (if (empty? z)
        s
        (recur (update-in s (first z) #(private-card-vector state :corp %)) (rest z))))))

(defn- make-private-deck [state side deck]
  (if (:view-deck (side @state))
    deck
    (private-card-vector state side deck)))

(defn- private-states
  "Generates privatized states for the Corp, Runner and any spectators from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well."
  [state]
  ;; corp, runner, spectator
  (let [corp-private (make-private-corp state)
        runner-private (make-private-runner state)
        corp-deck (update-in (:corp @state) [:deck] #(make-private-deck state :corp %))
        runner-deck (update-in (:runner @state) [:deck] #(make-private-deck state :runner %))]
    [(assoc @state :runner runner-private
                   :corp corp-deck)
     (assoc @state :corp corp-private
                   :runner runner-deck)
     (if (get-in @state [:options :spectatorhands])
       (assoc @state :corp corp-deck :runner runner-deck)
       (assoc @state :corp corp-private :runner runner-private))]))

(defn public-states [state]
  (let [[new-corp new-runner new-spect] (private-states state)]
    {:runner-state (strip new-runner)
     :corp-state   (strip new-corp)
     :spect-state  (strip new-spect)}))

(defn public-diffs [old-state new-state]
  (let [[old-corp old-runner old-spect] (when old-state (private-states (atom old-state)))
        [new-corp new-runner new-spect] (private-states new-state)

        runner-diff (differ/diff (strip old-runner) (strip new-runner))
        corp-diff (differ/diff (strip old-corp) (strip new-corp))
        spect-diff (differ/diff (strip old-spect) (strip new-spect))]
    {:runner-diff runner-diff
     :corp-diff   corp-diff
     :spect-diff  spect-diff}))
