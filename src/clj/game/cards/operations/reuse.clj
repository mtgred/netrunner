(in-ns 'game.core)

(def card-definitions-operations-reuse
  {"Reuse"
   {:delayed-completion true
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
                                      (gain :credit (* 2 (count targets))))} card nil)))}})
