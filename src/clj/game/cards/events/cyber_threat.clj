(in-ns 'game.cards.events)

(def card-definition-cyber-threat
  {"Cyber Threat"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :effect (req (let [serv target]
                   (continue-ability
                     state :corp
                     {:optional
                      {:prompt (msg "Rez a piece of ICE protecting " serv "?")
                       :yes-ability {:prompt (msg "Select a piece of ICE protecting " serv " to rez")
                                     :player :corp
                                     :choices {:req #(and (not (:rezzed %))
                                                          (= (last (:zone %)) :ices))}
                                     :effect (req (rez state :corp target nil))}
                       :no-ability {:effect (effect (game.core/run eid serv nil card))
                                    :msg (msg "make a run on " serv " during which no ICE can be rezzed")}}}
                    card nil)))}})
