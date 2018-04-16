(in-ns 'game.core)

(def card-hardware-rabbit-hole
  {"Rabbit Hole"
   {:in-play [:link 1]
    :effect
    (effect (resolve-ability
             {:optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck runner)))
                         :prompt "Install another Rabbit Hole?" :msg "install another Rabbit Hole"
                         :yes-ability {:effect (req (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                                      (:deck runner))]
                                                     (trigger-event state side :searched-stack nil)
                                                     (shuffle! state :runner :deck)
                                                     (runner-install state side c)))}}} card nil))}})