(in-ns 'game.cards.assets)

(def card-definition-open-forum
  {"Open Forum"
   {:events {:corp-mandatory-draw
             {:interactive (req true)
              :msg (msg (if (-> corp :deck count pos?)
                          (str "reveal and draw " (-> corp :deck first :title) " from R&D")
                          "reveal and draw from R&D but it is empty"))
              :async true
              :effect (effect (draw 1)
                              (continue-ability
                                {:prompt "Choose a card in HQ to put on top of R&D"
                                 :async true
                                 :choices {:req #(and (in-hand? %)
                                                      (= (:side %) "Corp"))}
                                 :msg "add 1 card from HQ to the top of R&D"
                                 :effect (effect (move target :deck {:front true})
                                                 (effect-completed eid))}
                                card nil))}}}})
