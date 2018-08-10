(in-ns 'game.cards.ice)

(def card-definition-mlinzi
  {"Mlinzi"
   (letfn [(net-or-trash [net-dmg mill-cnt]
             {:label (str "Do " net-dmg " net damage")
              :effect (req (show-wait-prompt state :corp "Runner to choose an option for Mlinzi")
                           (resolve-ability
                             state :runner
                             {:prompt "Take net damage or trash cards from the stack?"
                              :choices [(str "Take " net-dmg " net damage")
                                        (str "Trash the top " mill-cnt " cards of the stack")]
                              :effect (req (if (.startsWith target "Take")
                                             (do (system-msg state :corp
                                                             (str "uses Mlinzi to do "
                                                                  net-dmg " net damage"))
                                                 (clear-wait-prompt state :corp)
                                                 (damage state :runner eid :net net-dmg {:card card}))
                                             (do (system-msg state :corp
                                                             (str "uses Mlinzi to trash "
                                                                  (join ", " (map :title (take mill-cnt (:deck runner))))
                                                                  " from the runner's stack"))
                                                 (clear-wait-prompt state :corp)
                                                 (mill state :runner mill-cnt))))}
                             card nil))})]
     {:subroutines [(net-or-trash 1 2)
                    (net-or-trash 2 3)
                    (net-or-trash 3 4)]})})
