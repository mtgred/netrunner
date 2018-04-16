(in-ns 'game.core)

(declare can-host?)

(def card-programs-scheherazade
  {"Scheherazade"
   {:abilities [{:label "Install and host a program from Grip"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program to install on Scheherazade from your grip"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target) " and gain 1 [Credits]")
                                    :effect (effect (runner-install target {:host-card card}) (gain :credit 1))}
                                  card nil))}
                {:label "Host an installed program"
                 :prompt "Choose a program to host on Scheherazade" :priority 2
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target) " and gain 1 [Credits]")
                 :effect (req (when (host state side card target)
                                (gain state side :credit 1)))}]}})