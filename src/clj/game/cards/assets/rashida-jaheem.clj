(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-rashida-jaheem
  {"Rashida Jaheem"
   {:events {:corp-turn-begins {:delayed-completion true
                                :effect (effect (show-wait-prompt :runner "Corp to use Rashida Jaheem")
                                                (continue-ability
                                                  {:optional
                                                   {:prompt "Trash Rashida Jaheem to gain 3[Credits] and draw 3 cards?"
                                                    :yes-ability {:msg "gain 3[Credits] and draw 3 cards"
                                                                  :effect (effect (gain :credit 3)
                                                                                  (draw 3)
                                                                                  (trash card)
                                                                                  (clear-wait-prompt :runner)
                                                                                  (effect-completed eid))}
                                                    :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                                                 (effect-completed eid))}}}
                                                  card nil))}}}})