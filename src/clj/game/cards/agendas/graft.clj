(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-graft
  {"Graft"
   (letfn [(graft [n] {:prompt "Choose a card to add to HQ with Graft"
                       :delayed-completion true
                       :choices (req (cancellable (:deck corp) :sorted))
                       :msg (msg "add " (:title target) " to HQ from R&D")
                       :cancel-effect (req (shuffle! state side :deck)
                                           (system-msg state side (str "shuffles R&D"))
                                           (effect-completed state side eid))
                       :effect (req (move state side target :hand)
                                    (if (< n 3)
                                      (continue-ability state side (graft (inc n)) card nil)
                                      (do (shuffle! state side :deck)
                                          (system-msg state side (str "shuffles R&D"))
                                          (effect-completed state side eid card))))})]
     {:delayed-completion true
      :msg "add up to 3 cards from R&D to HQ"
      :effect (effect (continue-ability (graft 1) card nil))})})