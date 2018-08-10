(in-ns 'game.cards.ice)

(def card-definition-yagura
  {"Yagura"
   {:subroutines [(do-net-damage 1)
                  {:msg "look at the top card of R&D"
                   :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                              :yes-ability {:msg "move the top card of R&D to the bottom"
                                            :effect (effect (move (first (:deck corp)) :deck))}
                              :no-ability {:effect (effect (system-msg :corp (str "does not use Yagura to move the top card of R&D to the bottom")))}}}]}})
