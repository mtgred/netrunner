(ns game.core.change-vals
  (:require
    [game.core.agendas :refer [update-all-agenda-points]]
    [game.core.effects :refer [register-lingering-effect]]
    [game.core.engine :refer [trigger-event]]
    [game.core.gaining :refer [base-mod-size deduct gain]]
    [game.core.hand-size :refer [hand-size update-hand-size]]
    [game.core.link :refer [get-link update-link]]
    [game.core.memory :refer [available-mu update-mu]]
    [game.core.say :refer [system-msg]]
    [game.core.tags :refer [update-tag-status]]
    [game.macros :refer [req]]))

(defn- change-msg
  "Send a system message indicating the property change"
  [state side kw new-val delta]
  (let [key (name kw)]
    (system-msg state side
                (str "sets " (.replace key "-" " ") " to " new-val
                     " (" (if (pos? delta) (str "+" delta) delta) ")"))))

(defn- change-map
  "Change a player's property using the :mod system"
  [state side key delta]
  (gain state side key {:mod delta})
  (change-msg state side key (base-mod-size state side key) delta))

(defn- change-mu
  "Send a system message indicating how mu was changed"
  [state side delta]
  (register-lingering-effect
    state side nil
    {:type :user-available-mu
     :value [:regular delta]})
  (update-mu state)
  (system-msg state side
              (str "sets unused [mu] to " (available-mu state)
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-tags
  "Change a player's tag count"
  [state delta]
  (gain state :runner :tag delta)
  (update-tag-status state)
  (system-msg state :runner
              (str "sets Tags to " (get-in @state [:runner :tag :total])
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-bad-pub
  "Change a player's base bad pub count"
  [state delta]
  (if (neg? delta)
    (deduct state :corp [:bad-publicity (Math/abs delta)])
    (gain state :corp :bad-publicity delta))
  (system-msg state :corp
              (str "sets Bad Publicity to " (get-in @state [:corp :bad-publicity :base])
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-agenda-points
  "Change a player's total agenda points, using floating effects."
  [state side delta]
  (register-lingering-effect
    state side nil
    ;; This is needed as `req` creates/shadows the existing `side` already in scope.
    (let [user-side side]
      {:type :user-agenda-points
       ;; `target` is either `:corp` or `:runner`
       :req (req (= user-side target))
       :value delta}))
  (update-all-agenda-points state side)
  (system-msg state side
              (str "sets [their] agenda points to " (get-in @state [side :agenda-point])
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-link
  "Change the runner's link, using floating effects."
  [state side delta]
  (register-lingering-effect
    state side nil
    {:type :user-link
     :value delta})
  (update-link state)
  (system-msg state side
              (str "sets [their] [link] to " (get-link state)
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-hand-size
  "Change the player's hand-size, using floating effects."
  [state side delta]
  (register-lingering-effect
    state side nil
    (let [user-side side]
      {:type :user-hand-size
       :req (req (= side user-side))
       :value delta}))
  (update-hand-size state side)
  (system-msg state side
              (str "sets [their] hand size to " (hand-size state side)
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-generic
  "Change a player's base generic property."
  [state side key delta]
  (if (neg? delta)
    (deduct state side [key (- delta)])
    (swap! state update-in [side key] (partial + delta)))
  (change-msg state side key (get-in @state [side key]) delta))

(defn change
  "Increase/decrease a player's property (clicks, credits, MU, etc.) by delta."
  [state side {:keys [key delta]}]
  (case key
    :memory (change-mu state side delta)
    :hand-size (change-hand-size state side delta)
    :tag (change-tags state delta)
    :bad-publicity (change-bad-pub state delta)
    :agenda-point (change-agenda-points state side delta)
    :link (change-link state side delta)
    ; else
    (change-generic state side key delta)))
