(in-ns 'game.cards.agendas)

(def card-definition-project-atlas
  {"Project Atlas"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (max 0 (- (get-counters card :advancement) 3))))
    :abilities [{:counter-cost [:agenda 1]
                 :prompt "Choose a card"
                 :label "Search R&D and add 1 card to HQ"
                 ;; we need the req or the prompt will still show
                 :req (req (pos? (get-counters card :agenda)))
                 :msg (msg "add " (:title target) " to HQ from R&D")
                 :choices (req (cancellable (:deck corp) :sorted))
                 :cancel-effect (effect (system-msg "cancels the effect of Project Atlas"))
                 :effect (effect (shuffle! :deck)
                                 (move target :hand))}]}})
