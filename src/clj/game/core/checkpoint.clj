(ns game.core.checkpoint
  (:require
   [game.core.agendas :refer [update-all-advancement-requirements update-all-agenda-points]]
   [game.core.actions :refer [generate-runnable-zones]]
   [game.core.board :refer [get-remotes clear-empty-remotes]]
   [game.core.effects :refer [update-disabled-cards]]
   [game.core.ice :refer [update-all-ice update-all-icebreakers]]
   [game.core.hand-size :refer [update-hand-size]]
   [game.core.initializing :refer [update-all-card-labels]]
   [game.core.link :refer [update-link]]
   [game.core.memory :refer [update-mu]]
   [game.core.subtypes :refer [update-all-subtypes]]
   [game.core.tags :refer [update-tag-status]]))

(defn fake-checkpoint
  [state]
  (loop [i 0]
    (let [changed [(update-all-ice state :corp)
                   (update-all-icebreakers state :runner)
                   (update-all-card-labels state)
                   (update-all-advancement-requirements state)
                   (update-all-agenda-points state)
                   (update-link state)
                   (update-mu state)
                   (update-hand-size state :corp)
                   (update-hand-size state :runner)
                   (update-all-subtypes state)
                   (update-tag-status state)
                   (update-disabled-cards state)]]
      (when (and (some true? changed)
                 (< i 10))
        (recur (inc i)))))
  (clear-empty-remotes state)
  (generate-runnable-zones state nil nil))
