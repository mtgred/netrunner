(in-ns 'game.cards.hardware)

(def card-definition-mirror
  {"Mirror"
   {:in-play [:memory 2]
    :events {:successful-run
             {:async true
              :req (req (= target :rd))
              :effect (effect (continue-ability
                                {:prompt "Select a card and replace 1 spent [Recurring Credits] on it"
                                 :choices {:req #(< (get-counters % :recurring) (:recurring (card-def %) 0))}
                                 :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                 :effect (effect (add-prop target :rec-counter 1))}
                               card nil))}}}})
