(in-ns 'game.cards.programs)

(def card-definition-nyashia
  {"Nyashia"
   {:data {:counter {:power 3}}
    :events {:pre-access {:async true
                          :req (req (and (pos? (get-counters card :power))
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
