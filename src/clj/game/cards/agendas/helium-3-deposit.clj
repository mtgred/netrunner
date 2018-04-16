(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-helium-3-deposit
  {"Helium-3 Deposit"
   {:interactive (req true)
    :prompt "How many power counters?"
    :choices ["0" "1" "2"]
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:req #(< 0 (get-in % [:counter :power] 0))}
                      :msg (msg "add " c " power counters on " (:title target))
                      :effect (final-effect (add-counter target :power c))}
                     card nil)))}})
