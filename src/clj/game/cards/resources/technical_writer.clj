(in-ns 'game.cards.resources)

(def card-definition-technical-writer
  {"Technical Writer"
   {:events {:runner-install {:silent (req true)
                              :req (req (some #(= % (:type target)) '("Hardware" "Program")))
                              :effect (effect (add-counter :runner card :credit 1)
                                              (system-msg (str "places 1 [Credits] on Technical Writer")))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "gain " (get-counters card :credit) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain-credits (get-counters card :credit)))}]}})
