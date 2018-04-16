(in-ns 'game.core)

(def card-operations-fast-track
  {"Fast Track"
   {:prompt "Choose an Agenda"
    :choices (req (cancellable (filter #(is-type? % "Agenda") (:deck corp)) :sorted))
    :effect (effect (system-msg (str "adds " (:title target) " to HQ and shuffle R&D"))
                    (shuffle! :deck)
                    (move target :hand) )}})
