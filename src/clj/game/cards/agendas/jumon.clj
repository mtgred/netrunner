(in-ns 'game.cards.agendas)

(def card-definition-jumon
  {"Jumon"
   {:events
    {:corp-turn-ends
     {:req (req (some #(and (= (last (:zone %)) :content)
                            (is-remote? (second (:zone %))))
                      (all-installed state :corp)))
      :prompt "Select a card to place 2 advancement tokens on"
      :player :corp
      :choices {:req #(and (= (last (:zone %)) :content)
                           (is-remote? (second (:zone %))))}
      :msg (msg "place 2 advancement token on " (card-str state target))
      :effect (effect (add-prop :corp target :advance-counter 2 {:placed true}))}}}})
