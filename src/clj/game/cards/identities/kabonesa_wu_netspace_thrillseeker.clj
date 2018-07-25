(in-ns 'game.cards.identities)

(def card-definition-kabonesa-wu-netspace-thrillseeker
  {"Kabonesa Wu: Netspace Thrillseeker"
   {:abilities [{:label "[:click] Install a non-virus program from your stack, lowering the cost by 1 [Credit]"
                 :cost [:click 1]
                 :prompt "Choose a program"
                 :choices (req (cancellable
                                (filter #(and (is-type? % "Program")
                                              (not (has-subtype? % "Virus")))
                                        (:deck runner))))
                 :msg (str "install a non-virus program from their stack, lowering the cost by 1 [Credit]")
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (install-cost-bonus [:credit -1])
                                 (runner-install (assoc-in target [:special :kabonesa] true)))
                 :end-turn
                 {:req (req (get-in (find-cid (:cid target) (all-active-installed state :runner)) [:special :kabonesa]))
                  :msg (msg "remove " (:title target) " from the game")
                  :effect (req (move state side (find-cid (:cid target) (all-active-installed state :runner))
                                     :rfg))}}]}})
