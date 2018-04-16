(in-ns 'game.core)

(def card-hardware-monolith
  {"Monolith"
   (let [mhelper (fn mh [n] {:prompt "Select a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (install-cost-bonus state side [:credit -4])
                                          (runner-install state side target nil)
                                            (when (< n 3)
                                              (resolve-ability state side (mh (inc n)) card nil)))})]
     {:prevent {:damage [:net :brain]}
      :in-play [:memory 3]
      :effect (effect (resolve-ability (mhelper 1) card nil))
      :abilities [{:msg (msg "prevent 1 brain or net damage by trashing " (:title target))
                   :priority 50
                   :choices {:req #(and (is-type? % "Program")
                                        (in-hand? %))}
                   :prompt "Choose a program to trash from your Grip"
                   :effect (effect (trash target)
                                   (damage-prevent :brain 1)
                                   (damage-prevent :net 1))}]})})