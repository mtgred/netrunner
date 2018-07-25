(in-ns 'game.cards.operations)

(def card-definition-exchange-of-information
  {"Exchange of Information"
   {:req (req (and tagged
                   (seq (:scored runner))
                   (seq (:scored corp))))
    :async true
    :effect (req
              (continue-ability
                state side
                {:prompt "Select a stolen agenda in the Runner's score area to swap"
                 :choices {:req #(in-runner-scored? state side %)}
                 :async true
                 :effect (req
                           (let [stolen target]
                             (continue-ability
                               state side
                               {:prompt (msg "Select a scored agenda to swap for " (:title stolen))
                                :choices {:req #(in-corp-scored? state side %)}
                                :effect (req (let [scored target]
                                               (swap-agendas state side scored stolen)
                                               (system-msg state side (str "uses Exchange of Information to swap "
                                                                           (:title scored) " for " (:title stolen)))
                                               (effect-completed state side eid)))}
                               card nil)))}
                card nil))}})
