(in-ns 'game.cards.ice)

(def card-definition-news-hound
  {"News Hound"
   {:subroutines [(tag-trace 3)
                  {:label "End the run if a Current is active"
                   :req (req (or (not (empty? (runner :current)))
                                 (not (empty? (corp :current)))))
                   :effect (effect (end-run)) :msg "end the run"}]}})
