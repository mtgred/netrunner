(in-ns 'game.core)

(def card-definitions-upgrades-disposable-hq
  {"Disposable HQ"
   (letfn [(dhq [n i]
             {:req (req (pos? i))
              :prompt "Select a card in HQ to add to the bottom of R&D"
              :choices {:req #(and (= (:side %) "Corp")
                                   (in-hand? %))}
              :delayed-completion true
              :msg "add a card to the bottom of R&D"
              :effect (req (move state side target :deck)
                           (if (< n i)
                             (continue-ability state side (dhq (inc n) i) card nil)
                             (do
                               (clear-wait-prompt state :runner)
                               (effect-completed state side eid))))
              :cancel-effect (final-effect (clear-wait-prompt :runner))})]
     {:flags {:rd-reveal (req true)}
      :access {:delayed-completion true
               :effect (req (let [n (count (:hand corp))]
                              (show-wait-prompt state :runner "Corp to finish using Disposable HQ")
                              (continue-ability state side
                                {:optional
                                 {:prompt "Use Disposable HQ to add cards to the bottom of R&D?"
                                  :yes-ability {:delayed-completion true
                                                :msg "add cards in HQ to the bottom of R&D"
                                                :effect (effect (continue-ability (dhq 1 n) card nil))}
                                  :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil)))}})})
