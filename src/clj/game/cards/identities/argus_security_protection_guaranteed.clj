(in-ns 'game.cards.identities)

(def card-definition-argus-security-protection-guaranteed
  {"Argus Security: Protection Guaranteed"
   {:events {:agenda-stolen
             {:prompt "Take 1 tag or suffer 2 meat damage?"
              :async true
              :choices ["1 tag" "2 meat damage"] :player :runner
              :msg "make the Runner take 1 tag or suffer 2 meat damage"
              :effect (req (if (= target "1 tag")
                             (do (system-msg state side "chooses to take 1 tag")
                                 (gain-tags state :runner eid 1))
                             (do (system-msg state side "chooses to suffer 2 meat damage")
                                 (damage state :runner eid :meat 2 {:unboostable true :card card}))))}}}})
