(in-ns 'game.cards.programs)

(def card-definition-dhegdheer
  {"Dhegdheer"
   {:abilities [{:label "Install a program on Dhegdheer"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Dhegdheer"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target) (when (-> target :cost pos?) ", lowering its cost by 1 [Credit]"))
                                    :effect (effect (when (-> target :cost pos?)
                                                      (install-cost-bonus state side [:credit -1]))
                                                    (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}
                                  card nil))}
                {:label "Host an installed program on Dhegdheer with [Credit] discount"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :prompt "Choose an installed program to host on Dhegdheer with [Credit] discount"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target) (when (-> target :cost pos?) ", lowering its cost by 1 [Credit]"))
                 :effect (req (free-mu state (:memoryunits target))
                              (when (-> target :cost pos?)
                                (gain-credits state side 1))
                              (update-breaker-strength state side target)
                              (host state side card (get-card state target))
                              (update! state side (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}
                {:label "Host an installed program on Dhegdheer"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :prompt "Choose an installed program to host on Dhegdheer"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target) (when (-> target :cost pos?)))
                 :effect (effect (free-mu (:memoryunits target))
                                 (update-breaker-strength target)
                                 (host card (get-card state target))
                                 (update! (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}]
    :events {:card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :dheg-prog])))
                          :effect (effect (update! (dissoc-in card [:special :dheg-prog]))
                                          (use-mu (:memoryunits target)))}}}})
