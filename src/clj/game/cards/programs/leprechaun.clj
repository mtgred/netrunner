(in-ns 'game.core)

(declare can-host?)

(def card-programs-leprechaun
  {"Leprechaun"
   {:abilities [{:label "Install a program on Leprechaun"
                 :req (req (< (count (:hosted card)) 2))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Leprechaun"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target))
                                    :effect (effect (gain :memory (:memoryunits target))
                                                    (runner-install target {:host-card card})
                                                    (update! (assoc (get-card state card)
                                                                    :hosted-programs
                                                                    (cons (:cid target) (:hosted-programs card)))))}
                                  card nil))}
                {:label "Host an installed program on Leprechaun"
                 :req (req (< (count (:hosted card)) 2))
                 :prompt "Choose an installed program to host on Leprechaun"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (lose :memory (:memoryunits target)))}}}
   "LLDS Energy Regulator"
   {:prevent {:trash [:hardware]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent a hardware from being trashed"
                 :effect (effect (trash-prevent :hardware 1))}
                {:label "[Trash]: Prevent a hardware from being trashed"
                 :msg "prevent a hardware from being trashed"
                 :effect (effect (trash-prevent :hardware 1)
                                 (trash card {:cause :ability-cost}))}]}})