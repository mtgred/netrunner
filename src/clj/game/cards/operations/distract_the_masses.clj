(in-ns 'game.cards.operations)

(def card-definition-distract-the-masses
  {"Distract the Masses"
   (let [shuffle-two {:async true
                      :effect (effect (rfg-and-shuffle-rd-effect (find-cid (:cid card) (:discard corp)) 2))}
         trash-from-hq {:async true
                        :prompt "Select up to 2 cards in HQ to trash"
                        :choices {:max 2
                                  :req #(and (= (:side %) "Corp")
                                             (in-hand? %))}
                        :msg (msg "trash " (quantify (count targets) "card") " from HQ")
                        :effect (req (wait-for
                                       (trash-cards state side targets nil)
                                       (continue-ability state side shuffle-two card nil)))
                        :cancel-effect (req (continue-ability state side shuffle-two card nil))}]
     {:async true
      :msg "give The Runner 2 [Credits]"
      :effect (effect (gain-credits :runner 2)
                      (continue-ability trash-from-hq card nil))})})
