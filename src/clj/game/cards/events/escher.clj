(in-ns 'game.core)

(declare run-event)

(def card-events-escher
  {"Escher"
   (letfn [(es [] {:prompt "Select two pieces of ICE to swap positions"
                   :choices {:req #(and (installed? %) (ice? %)) :max 2}
                   :effect (req (if (= (count targets) 2)
                                  (do (swap-ice state side (first targets) (second targets))
                                      (resolve-ability state side (es) card nil))
                                  (system-msg state side "has finished rearranging ICE")))})]
     {:req (req hq-runnable)
            :effect (effect (run :hq {:replace-access
                                {:msg "rearrange installed ICE"
                                 :effect (effect (resolve-ability (es) card nil))}} card))})})