(in-ns 'game.core)

(def card-definitions-operations-distract-the-masses
  {"Distract the Masses"
   (let [shuffle-two {:delayed-completion true
                      :effect (effect (rfg-and-shuffle-rd-effect (find-cid (:cid card) (:discard corp)) 2))}
         trash-from-hq {:delayed-completion true
                        :prompt "Select up to 2 cards in HQ to trash"
                        :choices {:max 2
                                  :req #(and (= (:side %) "Corp")
                                             (in-hand? %))}
                        :msg (msg "trash " (quantify (count targets) "card") " from HQ")
                        :effect (req (when-completed
                                       (trash-cards state side targets nil)
                                       (continue-ability state side shuffle-two card nil)))
                        :cancel-effect (req (continue-ability state side shuffle-two card nil))}]
     {:delayed-completion true
      :msg "give The Runner 2 [Credits]"
      :effect (effect (gain :runner :credit 2)
                      (continue-ability trash-from-hq card nil))})})
