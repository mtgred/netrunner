(in-ns 'game.core)

(def card-definitions-resources-aesops-pawnshop
  {"Aesops Pawnshop"
   {:flags {:runner-phase-12 (req (>= 2 (count (all-installed state :runner))))}
    :abilities [{:effect (req (resolve-ability
                                state side
                                {:msg (msg "trash " (:title target) " and gain 3 [Credits]")
                                 :choices {:req #(and (card-is? % :side :runner) (installed? %) (not (card-is? % :cid (:cid card))))}
                                 :effect (effect (gain :credit 3) (trash target {:unpreventable true}))}
                                card nil))}]}})
