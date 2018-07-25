(in-ns 'game.cards.agendas)

(def card-definition-15-minutes
  {"15 Minutes"
   {:abilities [{:cost [:click 1] :msg "shuffle 15 Minutes into R&D"
                 :label "Shuffle 15 Minutes into R&D"
                 :effect (req (let [corp-agendas (get-in corp [:scored])
                                    agenda-owner (if (some #(= (:cid %) (:cid card)) corp-agendas) :corp :runner)]
                                (gain-agenda-point state agenda-owner (- (:agendapoints card))))
                              ; refresh agendapoints to 1 before shuffle in case it was modified by e.g. The Board
                              (move state :corp (dissoc (assoc card :agendapoints 1) :seen :rezzed) :deck {:front true})
                              (shuffle! state :corp :deck))}]
    :flags {:has-abilities-when-stolen true}}})
