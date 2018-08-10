(in-ns 'game.cards.resources)

(def card-definition-off-campus-apartment
  {"Off-Campus Apartment"
   {:flags {:runner-install-draw true}
    :abilities [{:label "Install and host a connection on Off-Campus Apartment"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Select a connection in your Grip to install on Off-Campus Apartment"
                                    :choices {:req #(and (has-subtype? % "Connection")
                                                         (can-pay? state side nil :credit (:cost %))
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target) " and draw 1 card")
                                    :effect (effect (runner-install target {:host-card card}))}
                                  card nil))}
                {:label "Host an installed connection"
                 :prompt "Select a connection to host on Off-Campus Apartment"
                 :choices {:req #(and (has-subtype? % "Connection")
                                      (installed? %))}
                 :msg (msg "host " (:title target) " and draw 1 card")
                 :effect (effect (host card target) (draw))}]
    :events {:runner-install {:req (req (= (:cid card) (:cid (:host target))))
                              :effect (effect (draw))}}}})
