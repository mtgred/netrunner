(in-ns 'game.cards.ice)

(def card-definition-tribunal
  {"Tribunal"
   {:subroutines [{:msg "force the Runner to trash 1 installed card"
                   :effect (effect (resolve-ability :runner trash-installed card nil))}]}})
