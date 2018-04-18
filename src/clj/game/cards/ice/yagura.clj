(in-ns 'game.core)

(def card-definitions-ice-yagura
  {"Yagura"
   {:subroutines [(do-net-damage 1)
                  {:msg "look at the top card of R&D"
                   :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                              :yes-ability {:effect (effect (move (first (:deck corp)) :deck)
                                                            (do (system-msg state side "uses Yagura to move the top card of R&D to the bottom")))}
                              :no-ability {:effect (req (system-msg state :corp (str "does not use Yagura to move the top card of R&D to the bottom")))}}}]}})
