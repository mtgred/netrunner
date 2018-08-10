(in-ns 'game.cards.operations)

(def card-definition-fast-break
  {"Fast Break"
   {:async true
    :req (req (-> runner :scored count pos?))
    :effect
    (req (let [X (-> runner :scored count)
               draw {:async true
                     :prompt "Draw how many cards?"
                     :choices {:number (req X)
                               :max (req X)
                               :default (req 1)}
                     :msg (msg "draw " target " cards")
                     :effect (effect (draw eid target nil))}
               install-cards (fn install-cards
                               [server n]
                               {:prompt "Select a card to install"
                                :choices {:req #(and (= (:side %) "Corp")
                                                     (not (is-type? % "Operation"))
                                                     (in-hand? %)
                                                     (seq (filter (fn [c] (= server c)) (corp-install-list state %))))}
                                :effect (req (wait-for
                                               (corp-install state side target server nil)
                                               (let [server (if (= "New remote" server)
                                                              (-> (turn-events state side :corp-install)
                                                                  ffirst :zone second zone->name)
                                                              server)]
                                                 (if (< n X)
                                                   (continue-ability state side (install-cards server (inc n)) card nil)
                                                   (effect-completed state side eid)))))
                                :cancel-effect (effect (effect-completed eid))})
               select-server {:async true
                              :prompt "Install cards in which server?"
                              :choices (req (conj (vec (get-remote-names state)) "New remote"))
                              :effect (effect (continue-ability (install-cards target 1) card nil))}]
           (gain-credits state :corp X)
           (wait-for (resolve-ability state side draw card nil)
                     (continue-ability state side select-server card nil))))}})
