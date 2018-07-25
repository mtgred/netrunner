(in-ns 'game.cards.operations)

(def card-definition-reuse
  {"Reuse"
   {:async true
    :effect (req (let [n (count (:hand corp))]
                   (continue-ability state side
                     {:prompt (msg "Select up to " n " cards in HQ to trash with Reuse")
                      :choices {:max n
                                :req #(and (= (:side %) "Corp")
                                           (in-hand? %))}
                      :msg (msg (let [m (count targets)]
                                  (str "trash " (quantify m "card")
                                       " and gain " (* 2 m) " [Credits]")))
                      :effect (effect (trash-cards targets)
                                      (gain-credits (* 2 (count targets))))} card nil)))}})
