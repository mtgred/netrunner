(in-ns 'game.core)

(def card-definitions-agendas-mandatory-upgrades
  {"Mandatory Upgrades"
   {:msg "gain an additional [Click] per turn"
    :silent (req true)
    :effect (req (gain state :corp
                       :click 1
                       :click-per-turn 1))
    :swapped {:msg "gain an additional [Click] per turn"
              :effect (req (when (= (:active-player @state) :corp)
                             (gain state :corp :click 1))
                           (gain state :corp :click-per-turn 1))}
    :leave-play (req (lose state :corp
                           :click 1
                           :click-per-turn 1))}})
