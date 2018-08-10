(in-ns 'game.cards.upgrades)

(def card-definition-overseer-matrix
  {"Overseer Matrix"
   (let [om {:req (req (in-same-server? card target))
             :async true
             :effect (effect (show-wait-prompt :runner "Corp to use Overseer Matrix")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 1 [Credits] to use Overseer Matrix ability?"
                                 :player :corp
                                 :end-effect (effect (clear-wait-prompt :runner)
                                                     (effect-completed eid))
                                 :yes-ability {:cost [:credit 1]
                                               :msg "give the Runner 1 tag"
                                               :async true
                                               :effect (req (gain-tags state :corp eid 1))}}}
                               card nil))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card)))
                      (:run @state)))
       :effect (effect (register-events {:runner-trash (assoc om :req (req (or (= (:zone (get-nested-host target))
                                                                                  (:previous-zone card))
                                                                               (= (central->zone (:zone target))
                                                                                  (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:run-ends nil
               :runner-trash om}})})
