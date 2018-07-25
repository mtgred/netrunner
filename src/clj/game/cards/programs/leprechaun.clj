(in-ns 'game.cards.programs)

(def card-definition-leprechaun
  {"Leprechaun"
   {:abilities [{:label "Install a program on Leprechaun"
                 :req (req (< (count (get-in card [:special :hosted-programs])) 2))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Leprechaun"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc-in (get-card state card)
                                                                    [:special :hosted-programs]
                                                                    (cons (:cid target)
                                                                          (get-in card [:special :hosted-programs])))))}
                                  card nil))}
                {:label "Host an installed program on Leprechaun"
                 :req (req (< (count (get-in card [:special :hosted-programs])) 2))
                 :prompt "Choose an installed program to host on Leprechaun"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (free-mu (:memoryunits target))
                                 (update-breaker-strength target)
                                 (host card (get-card state target))
                                 (update! (assoc-in (get-card state card)
                                                    [:special :hosted-programs]
                                                    (cons (:cid target)
                                                          (get-in card [:special :hosted-programs])))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (get-in card [:special :hosted-programs])))
                          :effect (effect (update! (assoc-in card
                                                             [:special :hosted-programs]
                                                             (remove #(= (:cid target) %)
                                                                     (get-in card [:special :hosted-programs]))))
                                          (use-mu (:memoryunits target)))}}}})
