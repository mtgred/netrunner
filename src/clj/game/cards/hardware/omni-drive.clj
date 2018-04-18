(in-ns 'game.core)

(def card-definitions-hardware-omni-drive
  {"Omni-drive"
   {:recurring 1
    :abilities [{:label "Install and host a program of 1[Memory Unit] or less on Omni-drive"
                 :req (req (empty? (:hosted card)))
                 :cost [:click 1]
                 :prompt "Select a program of 1[Memory Unit] or less to install on Omni-drive from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card})
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}
                {:label "Host an installed program of 1[Memory Unit] or less on Omni-drive"
                 :prompt "Select an installed program of 1[Memory Unit] or less to host on Omni-drive"
                 :choices {:req #(and (is-type? % "Program")
                                      (<= (:memoryunits %) 1)
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}]
   :events {:card-moved {:req (req (= (:cid target) (:Omnidrive-prog (get-card state card))))
                          :effect (effect (update! (dissoc card :Omnidrive-prog))
                                          (lose :memory (:memoryunits target)))}}}})
