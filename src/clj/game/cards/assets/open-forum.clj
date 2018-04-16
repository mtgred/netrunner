(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-open-forum
  {"Open Forum"
   {:events {:corp-mandatory-draw {:interactive (req true)
                                   :msg (msg (let [deck (:deck corp)]
                                               (if (pos? (count deck))
                                               (str "reveal and draw " (:title (first deck)) " from R&D")
                                               "reveal & draw from R&D but it is empty")))
                                   :delayed-completion true
                                   :effect (effect (draw)
                                                   (continue-ability
                                                     {:prompt "Choose a card in HQ to put on top of R&D"
                                                      :delayed-completion true
                                                      :choices {:req #(and (in-hand? %)
                                                                           (= (:side %) "Corp"))}
                                                      :msg "add 1 card from HQ to the top of R&D"
                                                      :effect (effect (move target :deck {:front true})
                                                                      (effect-completed eid))}
                                                     card nil))}}}})
