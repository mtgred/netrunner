(in-ns 'game.cards.ice)

(def card-definition-dna-tracker
  {"DNA Tracker"
   {:subroutines [{:msg "do 1 net damage and make the Runner lose 2 [Credits]"
                   :effect (req (wait-for (damage state side :net 1 {:card card})
                                          (lose-credits state :runner 2)))}]}})
