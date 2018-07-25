(in-ns 'game.cards.ice)

(def card-definition-ashigaru
  {"Ashigaru"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand corp)) " subroutines")}]
    :subroutines [end-the-run]}})
