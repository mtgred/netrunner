(in-ns 'game.core)

(declare run-event)

(def card-events-networking
  {"Networking"
   {:msg "remove 1 tag"
    :effect (effect (lose :tag 1))
    :optional {:prompt "Pay 1 [Credits] to add Networking to Grip?"
               :yes-ability {:cost [:credit 1]
                             :msg "add it to their Grip"
                             :effect (effect (move (last (:discard runner)) :hand))}}}})
