(in-ns 'game.cards.resources)

(def card-definition-the-supplier
  {"The Supplier"
   (let [ability  {:label "Install a hosted card (start of turn)"
                   :prompt "Choose a card hosted on The Supplier to install"
                   :req (req (some #(can-pay? state side nil (modified-install-cost state side % [:credit -2]))
                                        (:hosted card)))
                   :choices {:req #(and (= "The Supplier" (:title (:host %)))
                                        (= "Runner" (:side %)))}
                   :once :per-turn
                   :effect (req
                             (runner-can-install? state side target nil)
                             (when (and (can-pay? state side nil (modified-install-cost state side target [:credit -2]))
                                           (not (and (:uniqueness target) (in-play? state target))))
                                  (install-cost-bonus state side [:credit -2])
                                  (runner-install state side target)
                                  (system-msg state side (str "uses The Supplier to install " (:title target) " lowering its install cost by 2"))
                                  (update! state side (-> card
                                                          (assoc :supplier-installed (:cid target))
                                                          (update-in [:hosted]
                                                                     (fn [coll]
                                                                       (remove-once #(= (:cid %) (:cid target)) coll)))))))}]
   {:flags {:drip-economy true}  ; not technically drip economy, but has an interaction with Drug Dealer
    :abilities [{:label "Host a resource or piece of hardware" :cost [:click 1]
                 :prompt "Select a card to host on The Supplier"
                 :choices {:req #(and (#{"Resource" "Hardware"} (:type %))
                                      (in-hand? %))}
                 :effect (effect (host card target)) :msg (msg "host " (:title target) "")}
                ability]
    ; A card installed by The Supplier is ineligible to receive the turn-begins event for this turn.
    :suppress {:runner-turn-begins {:req (req (= (:cid target) (:supplier-installed (get-card state card))))}}
    :events {:runner-turn-begins ability
             :runner-turn-ends {:req (req (:supplier-installed card))
                                :effect (effect (update! (dissoc card :supplier-installed)))}}})})
