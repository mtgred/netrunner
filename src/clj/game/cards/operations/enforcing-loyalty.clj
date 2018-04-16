(in-ns 'game.core)

(def card-operations-enforcing-loyalty
  {"Enforcing Loyalty"
   {:trace {:base 3
            :label "Trash a card not matching the faction of the Runner's identity"
            :delayed-completion true
            :effect (req (let [f (:faction (:identity runner))]
                           (continue-ability
                             state side
                             {:prompt "Select an installed card not matching the faction of the Runner's identity"
                              :choices {:req #(and (installed? %) (not= f (:faction %)) (card-is? % :side :runner))}
                              :msg (msg "trash " (:title target))
                              :effect (effect (trash target))}
                            card nil)))}}})