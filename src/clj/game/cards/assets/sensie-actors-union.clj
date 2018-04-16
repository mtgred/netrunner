(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-sensie-actors-union
  {"Sensie Actors Union"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req unprotected)}
    :abilities [{:label "Draw 3 cards and add 1 card in HQ to the bottom of R&D"
                 :once :per-turn
                 :msg "draw 3 cards"
                 :effect (effect (draw 3)
                                 (resolve-ability
                                   {:prompt "Select a card in HQ to add to the bottom of R&D"
                                    :choices {:req #(and (= (:side %) "Corp")
                                                         (in-hand? %))}
                                    :msg "add 1 card from HQ to the bottom of R&D"
                                    :effect (effect (move target :deck))}
                                  card nil))}]}})
