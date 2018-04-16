(in-ns 'game.core)

(def card-operations-liquidation
  {"Liquidation"
   {:delayed-completion true
    :effect (req (let [n (count (filter #(not (is-type? % "Agenda")) (all-active-installed state :corp)))]
                   (continue-ability state side
                     {:prompt "Select any number of rezzed cards to trash"
                      :choices {:max n
                                :req #(and (rezzed? %)
                                           (not (is-type? % "Agenda")))}
                      :msg (msg "trash " (join ", " (map :title (sort-by :title targets))) " and gain " (* (count targets) 3) " [Credits]")
                      :effect (req (doseq [c targets]
                                     (trash state side c))
                                   (gain state side :credit (* (count targets) 3)))}
                    card nil)))}})