(in-ns 'game.cards.hardware)

(def card-definition-monolith
  {"Monolith"
   (letfn [(mhelper [n]
             {:prompt "Select a program to install"
              :choices {:req #(and (is-type? % "Program")
                                   (in-hand? %))}
              :effect (req (install-cost-bonus state side [:credit -4])
                           (runner-install state side target nil)
                           (when (< n 3)
                             (resolve-ability state side (mhelper (inc n)) card nil)))})]
     {:interactions {:prevent [{:type #{:net :brain}
                                :req (req true)}]}
      :in-play [:memory 3]
      :effect (effect (resolve-ability (mhelper 1) card nil))
      :abilities [{:label "Prevent 1 brain or net damage"
                   :msg (msg "prevent 1 brain or net damage by trashing " (:title target))
                   :priority 50
                   :choices {:req #(and (is-type? % "Program")
                                        (in-hand? %))}
                   :prompt "Choose a program to trash from your Grip"
                   :effect (effect (trash target)
                                   (damage-prevent :brain 1)
                                   (damage-prevent :net 1))}]})})
