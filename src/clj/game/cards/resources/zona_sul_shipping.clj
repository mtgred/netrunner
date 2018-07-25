(in-ns 'game.cards.resources)

(def card-definition-zona-sul-shipping
  {"Zona Sul Shipping"
   {:events {:runner-turn-begins {:effect (effect (add-counter card :credit 1))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "gain " (get-counters card :credit) " [Credits]")
                 :label "Take all credits"
                 :effect (effect (gain-credits (get-counters card :credit))
                                 (add-counter card :credit
                                              (- (get-counters card :credit))))}]
    :effect (req (add-watch state (keyword (str "zona-sul-shipping" (:cid card)))
                            (fn [k ref old new]
                              (when (is-tagged? new)
                                (remove-watch ref (keyword (str "zona-sul-shipping" (:cid card))))
                                (trash ref :runner card)
                                (system-msg ref side "trashes Zona Sul Shipping for being tagged")))))
    :reactivate {:effect (req (when tagged
                                (trash state :runner card {:unpreventable true})))}}})
