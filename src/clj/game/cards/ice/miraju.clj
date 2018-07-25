(in-ns 'game.cards.ice)

(def card-definition-miraju
  {"Mirāju"
   {:abilities [{:label "Runner broke subroutine: Redirect run to Archives"
                 :msg "make the Runner continue the run on Archives. Mirāju is derezzed"
                 :effect (req (swap! state update-in [:run]
                                     #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                               :server [:archives]))
                              (derez state side card))}]
    :subroutines [{:label "Draw 1 card, then shuffle 1 card from HQ into R&D"
                   :effect (req (wait-for (resolve-ability
                                            state side
                                            {:optional
                                             {:prompt "Draw 1 card?"
                                              :yes-ability {:async true
                                                            :msg "draw 1 card"
                                                            :effect (effect (draw eid 1 nil))}}}
                                            card nil)
                                          (resolve-ability
                                            state side
                                            {:prompt "Choose 1 card in HQ to shuffle into R&D"
                                             :choices {:req #(and (in-hand? %) (= (:side %) "Corp"))}
                                             :msg "shuffle 1 card in HQ into R&D"
                                             :effect (effect (move target :deck)
                                                             (shuffle! :deck))}
                                            card nil)))}]}})
