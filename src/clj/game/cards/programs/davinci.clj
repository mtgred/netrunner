(in-ns 'game.core)

(def card-definitions-programs-davinci
  {"DaVinci"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:effect
                 (req (let [c card]
                        (resolve-ability state side
                                         {:prompt "Choose a card to install from your Grip"
                                          :choices {:req #(and (<= (:cost %) (get-in c [:counter :power] 0))
                                                               (#{"Hardware" "Program" "Resource"} (:type %))
                                                               (in-hand? %))}
                                          :req (req (not (install-locked? state side)))
                                          :msg (msg "install " (:title target) " at no cost")
                                          :effect (effect (trash card {:cause :ability-cost})
                                                          (runner-install target {:no-cost true}))}
                                         card nil)))}]}})
