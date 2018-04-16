(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-technical-writer
  {"Technical Writer"
   {:events {:runner-install {:silent (req true)
                              :req (req (some #(= % (:type target)) '("Hardware" "Program")))
                              :effect (effect (add-counter :runner card :credit 1)
                                              (system-msg (str "places 1 [Credits] on Technical Writer")))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "gain " (get-in card [:counter :credit] 0) " [Credits]")
                 :effect (effect (gain :credit (get-in card [:counter :credit] 0))
                                 (trash card {:cause :ability-cost}))}]}})
