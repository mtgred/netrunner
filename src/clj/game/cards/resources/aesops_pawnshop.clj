(in-ns 'game.cards.resources)

(def card-definition-aesops-pawnshop
  {"Aesop's Pawnshop"
   {:flags {:runner-phase-12 (req (>= (count (all-installed state :runner)) 2))}
    :abilities [{:effect (req (resolve-ability
                                state side
                                {:msg (msg "trash " (:title target) " and gain 3 [Credits]")
                                 :choices {:req #(and (card-is? % :side :runner)
                                                      (installed? %)
                                                      (not (card-is? % :cid (:cid card))))}
                                 :effect (effect (gain-credits 3)
                                                 (trash target {:unpreventable true}))}
                                card nil))}]}})
