(in-ns 'game.cards.assets)

(def card-definition-victoria-jenkins
  {"Victoria Jenkins"
   {:effect (req (lose state :runner :click-per-turn 1)
                 (when (= (:active-player @state) :runner)
                   (lose state :runner :click 1)))
    :leave-play (req (gain state :runner :click-per-turn 1)
                     (when (= (:active-player @state) :runner)
                       (gain state :runner :click 1)))
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :async true
                   :effect (req (as-agenda state :runner eid card 2))}}})
