(in-ns 'game.cards.resources)

(def card-definition-district-99
  {"District 99"
   {:implementation "Adding power counters must be done manually for programs/hardware trashed manually (e.g. by being over MU)"
    :abilities [{:label "Add a card from your heap to your grip"
                 :req (req (seq (filter #(= (:faction (:identity runner)) (:faction %)) (:discard runner))))
                 :counter-cost [:power 3] :cost [:click 1]
                 :prompt (msg "Which card to add to grip?")
                 :choices (req (filter #(= (:faction (:identity runner)) (:faction %)) (:discard runner)))
                 :effect (effect (move target :hand))
                 :msg (msg "Add " (:title target) " to grip")}
                {:label "Add a power counter manually"
                 :once :per-turn
                 :effect (effect (add-counter card :power 1))
                 :msg (msg "manually add a power counter.")}]
    :events (let [prog-or-hw (fn [t] (or (program? (first t)) (hardware? (first t))))
                  trash-event (fn [side-trash] {:once :per-turn
                                                :req (req (first-event? state side side-trash prog-or-hw))
                                                :effect (effect (add-counter card :power 1))})]
              {:corp-trash (trash-event :corp-trash)
               :runner-trash (trash-event :runner-trash)})}})
