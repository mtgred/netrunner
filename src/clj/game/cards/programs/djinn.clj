(in-ns 'game.core)

(declare can-host?)

(def card-programs-djinn
  {"Djinn"
   {:abilities [{:label "Search your Stack for a virus program and add it to your Grip"
                 :prompt "Choose a Virus"
                 :msg (msg "add " (:title target) " to their Grip")
                 :choices (req (cancellable (filter #(and (is-type? % "Program")
                                                          (has-subtype? % "Virus"))
                                                    (:deck runner)) :sorted))
                 :cost [:click 1 :credit 1]
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (move target :hand) )}
                {:label "Install a non-Icebreaker program on Djinn"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a non-Icebreaker program in your Grip to install on Djinn"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (not (has-subtype? % "Icebreaker"))
                                                         (in-hand? %))}
                                    :msg (msg "install and host " (:title target))
                                    :effect (effect (gain :memory (:memoryunits target))
                                                    (runner-install target {:host-card card})
                                                    (update! (assoc (get-card state card)
                                                                    :hosted-programs
                                                                    (cons (:cid target) (:hosted-programs card)))))}
                                  card nil))}
                {:label "Host an installed non-Icebreaker program on Djinn"
                 :prompt "Choose an installed non-Icebreaker program to host on Djinn"
                 :choices {:req #(and (is-type? % "Program")
                                      (not (has-subtype? % "Icebreaker"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (lose :memory (:memoryunits target)))}}}})