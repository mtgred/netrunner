(in-ns 'game.cards.agendas)

(def card-definition-helium-3-deposit
  {"Helium-3 Deposit"
   {:async true
    :interactive (req true)
    :prompt "How many power counters?"
    :choices ["0" "1" "2"]
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:req #(pos? (get-counters % :power))}
                      :msg (msg "add " c " power counters on " (:title target))
                      :effect (effect (add-counter target :power c))}
                     card nil)))}})
