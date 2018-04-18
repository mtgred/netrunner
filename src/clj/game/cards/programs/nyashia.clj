(in-ns 'game.core)

(def card-definitions-programs-nyashia
  {"Nyashia"
   {:data {:counter {:power 3}}
    :events {:pre-access {:delayed-completion true
                          :req (req (and (pos? (get-in card [:counter :power] 0))
                                         (= target :rd)))
                          :effect (effect (show-wait-prompt :corp "Runner to use Nyashia")
                                          (continue-ability
                                            {:optional
                                             {:prompt "Spend a power counter on Nyashia to access 1 additional card?"
                                              :yes-ability {:msg "access 1 additional card from R&D"
                                                            :effect (effect (access-bonus 1)
                                                                            (add-counter card :power -1)
                                                                            (clear-wait-prompt :corp))}
                                              :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                                            card nil))}}}})
