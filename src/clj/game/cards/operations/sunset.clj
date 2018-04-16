(in-ns 'game.core)

(def card-operations-sunset
  {"Sunset"
   (letfn [(sun [serv]
             {:prompt "Select two pieces of ICE to swap positions"
              :choices {:req #(and (= serv (rest (butlast (:zone %)))) (ice? %))
                        :max 2}
              :delayed-completion true
              :effect (req (if (= (count targets) 2)
                             (do (swap-ice state side (first targets) (second targets))
                                 (continue-ability state side (sun serv) card nil))
                             (do (system-msg state side "has finished rearranging ICE")
                                 (effect-completed state side eid card))))})]
     {:prompt "Choose a server"
      :choices (req servers)
      :delayed-completion true
      :msg (msg "rearrange ICE protecting " target)
      :effect (req (let [serv (next (server->zone state target))]
                     (continue-ability state side (sun serv) card nil)))})})
