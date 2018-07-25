(in-ns 'game.cards.ice)

(def card-definition-otoroshi
  {"Otoroshi"
   {:subroutines [{:async true
                   :label "Place 3 advancement tokens on installed card"
                   :msg "place 3 advancement tokens on installed card"
                   :prompt "Choose an installed Corp card"
                   :choices {:req #(and (= (:side %) "Corp")
                                        (installed? %))}
                   :effect (req (let [c target
                                      title (if (:rezzed c)
                                              (:title c)
                                              "selected unrezzed card")]
                                  (add-counter state side c :advancement 3)
                                  (show-wait-prompt state side "Runner to resolve Otoroshi")
                                  (continue-ability
                                    state side
                                    {:player :runner
                                     :async true
                                     :prompt (str "Access " title " or pay 3 [Credits]?")
                                     :choices (concat ["Access card"]
                                                      (when (>= (:credit runner) 3)
                                                        ["Pay 3 [Credits]"]))
                                     :msg (msg "force the Runner to "
                                               (if (= target "Access card")
                                                 (str "access " title)
                                                 "pay 3 [Credits]"))
                                     :effect (req (clear-wait-prompt state :corp)
                                                  (if (= target "Access card")
                                                    (access-card state :runner eid c)
                                                    (pay-sync state :runner eid card :credit 3)))}
                                    card nil)))}]}})
