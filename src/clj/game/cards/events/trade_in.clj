(in-ns 'game.cards.events)

(def card-definition-trade-in
  {"Trade-In"
   ;; Basically a hack. Ideally the additional cost cause the cost trash to be passed in as targets
   (letfn [(trashed-hw [state] (last (get-in @state [:runner :discard])))]
     {:additional-cost [:hardware 1]
      :msg (msg (let [{:keys [title cost]} (trashed-hw state)]
                  (str "trash " title " and gain " (quot cost 2) " [Credits]")))
      :effect (req (let [{:keys [cost]} (trashed-hw state)]
                     (gain-credits state :runner (quot cost 2))
                     (continue-ability state :runner
                                       {:prompt "Choose a Hardware to add to your Grip from your Stack"
                                        :choices (req (filter #(is-type? % "Hardware")
                                                              (:deck runner)))
                                        :msg (msg "add " (:title target) " to their Grip (and shuffle their Stack)")
                                        :effect (effect (trigger-event :searched-stack nil)
                                                        (shuffle! :deck)
                                                        (move target :hand))}
                                       card nil)))})})
