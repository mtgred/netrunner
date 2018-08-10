(in-ns 'game.cards.upgrades)

(def card-definition-georgia-emelyov
  {"Georgia Emelyov"
   {:events {:unsuccessful-run {:req (req (= (first (:server target))
                                             (second (:zone card))))
                                :async true
                                :msg "do 1 net damage"
                                :effect (effect (damage eid :net 1 {:card card}))}}
    :abilities [{:cost [:credit 2]
                 :label "Move to another server"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Choose a server"
                                    :choices (server-list state)
                                    :msg (msg "move to " target)
                                    :effect (req (let [c (move state side card
                                                               (conj (server->zone state target) :content))]
                                                   (unregister-events state side card)
                                                   (register-events state side (:events (card-def c)) c)))}
                                   card nil))}]}})
