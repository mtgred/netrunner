(in-ns 'game.core)

(def card-hardware-mirror
  {"Mirror"
   {:in-play [:memory 2]
    :events {:successful-run
             {:delayed-completion true
              :req (req (= target :rd))
              :effect (effect (continue-ability
                                {:prompt "Select a card and replace 1 spent [Recurring Credits] on it"
                                 :choices {:req #(< (:rec-counter % 0) (:recurring (card-def %) 0))}
                                 :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                 :effect (effect (add-prop target :rec-counter 1))}
                               card nil))}}}})