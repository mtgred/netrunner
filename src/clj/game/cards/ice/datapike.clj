(in-ns 'game.cards.ice)

(def card-definition-datapike
  {"Datapike"
   {:subroutines [{:msg "force the Runner to pay 2 [Credits] if able"
                   :effect (effect (pay :runner card :credit 2))}
                  end-the-run]}})
