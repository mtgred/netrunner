(in-ns 'game.cards.programs)

(def card-definition-progenitor
  {"Progenitor"
   {:abilities [{:label "Install a virus program on Progenitor"
                 :req (req (empty? (:hosted card)))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a Virus program to install on Progenitor"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (has-subtype? % "Virus")
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc (get-card state card)
                                                                    :hosted-programs
                                                                    (cons (:cid target) (:hosted-programs card)))))}
                                  card nil))}
                {:label "Host an installed virus on Progenitor"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed virus program to host on Progenitor"
                 :choices {:req #(and (is-type? % "Program")
                                      (has-subtype? % "Virus")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (free-mu (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:pre-purge {:effect (req (when-let [c (first (:hosted card))]
                                        (update! state side (assoc-in card [:special :numpurged] (get-counters c :virus)))))}
             :purge {:req (req (pos? (get-in card [:special :numpurged] 0)))
                     :effect (req (when-let [c (first (:hosted card))]
                                    (add-counter state side c :virus 1)))}
             :card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (use-mu (:memoryunits target)))}}}})
