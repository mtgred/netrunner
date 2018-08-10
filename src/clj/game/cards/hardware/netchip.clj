(in-ns 'game.cards.hardware)

(def card-definition-netchip
  {"NetChip"
   {:abilities [{:label "Install a program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                                (resolve-ability state side
                                  {:cost [:click 1]
                                   :prompt "Select a program in your Grip to install on NetChip"
                                   :choices {:req #(and (is-type? % "Program")
                                                        (runner-can-install? state side % false)
                                                        (<= (:memoryunits %) n)
                                                        (in-hand? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (runner-install target {:host-card card :no-mu true})
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                 card nil)))}
                {:label "Host an installed program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                                (resolve-ability state side
                                  {:prompt "Select an installed program to host on NetChip"
                                   :choices {:req #(and (is-type? % "Program")
                                                        (<= (:memoryunits %) n)
                                                        (installed? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (host card target)
                                                   (free-mu (:memoryunits target))
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                 card nil)))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs
                                                          (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (use-mu (:memoryunits target)))}}}})
