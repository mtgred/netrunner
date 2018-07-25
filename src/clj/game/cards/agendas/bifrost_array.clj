(in-ns 'game.cards.agendas)

(def card-definition-bifrost-array
  {"Bifrost Array"
   {:req (req (not (empty? (filter #(not= (:title %)
                                          "Bifrost Array")
                                   (:scored corp)))))
    :optional {:prompt "Trigger the ability of a scored agenda?"
               :yes-ability {:prompt "Select an agenda to trigger the \"when scored\" ability of"
                             :choices {:req #(and (is-type? % "Agenda")
                                                  (not= (:title %)
                                                        "Bifrost Array")
                                                  (= (first (:zone %))
                                                     :scored)
                                                  (when-scored? %)
                                                  (:abilities %))}
                             :msg (msg "trigger the \"when scored\" ability of " (:title target))
                             :effect (effect (continue-ability (card-def target) target nil))}
               :no-ability {:effect (effect (clear-wait-prompt :runner))}}}})
