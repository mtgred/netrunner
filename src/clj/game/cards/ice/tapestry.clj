(in-ns 'game.core)

(def card-definitions-ice-tapestry
  {"Tapestry"
   {:subroutines [{:label "force the Runner to lose 1 [Click], if able"
                   :msg "force the Runner to lose 1 [Click]"
                   :effect runner-loses-click}
                  {:msg "draw 1 card"
                   :effect (effect (draw))}
                  {:req (req (pos? (count (:hand corp))))
                   :prompt "Choose a card in HQ to move to the top of R&D"
                   :choices {:req #(and (in-hand? %) (= (:side %) "Corp"))}
                   :msg "add 1 card in HQ to the top of R&D"
                   :effect (effect (move target :deck {:front true}))}]}})
