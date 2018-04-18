(in-ns 'game.core)

(def card-definitions-ice-tribunal
  {"Tribunal"
   {:subroutines [{:msg "force the Runner to trash 1 installed card"
                   :effect (effect (resolve-ability :runner trash-installed card nil))}]}})
