(in-ns 'game.core)

(declare can-host?)

(def card-programs-dhegdheer
  {"Dhegdheer"
   {:abilities [{:label "Install a program on Dhegdheer"
                 :req (req (empty? (:hosted card)))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Dhegdheer"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target) (when (-> target :cost pos?) ", lowering its cost by 1 [Credit]"))
                                    :effect (effect (gain :memory (:memoryunits target))
                                                    (when (-> target :cost pos?)
                                                      (install-cost-bonus state side [:credit -1]))
                                                    (runner-install target {:host-card card})
                                                    (update! (assoc (get-card state card) :dheg-prog (:cid target))))}
                                  card nil))}
                {:label "Host an installed program on Dhegdheer with [Credit] discount"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed program to host on Dhegdheer with [Credit] discount"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target) (when (-> target :cost pos?) ", lowering its cost by 1 [Credit]"))
                 :effect (effect (host card target)
                                 (when (-> target :cost pos?)
                                   (gain state side :credit 1))
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card) :dheg-prog (:cid target))))}
                {:label "Host an installed program on Dhegdheer"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed program to host on Dhegdheer"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target) (when (-> target :cost pos?)))
                 :effect (effect (host card target)
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card) :dheg-prog (:cid target))))}]
    :events {:card-moved {:req (req (= (:cid target) (:dheg-prog (get-card state card))))
                          :effect (effect (update! (dissoc card :dheg-prog))
                                          (lose :memory (:memoryunits target)))}}}})
