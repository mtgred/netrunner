(in-ns 'game.core)

(def card-definitions-events-kraken
  {"Kraken"
   {:req (req (:stole-agenda runner-reg)) :prompt "Choose a server" :choices (req servers)
    :msg (msg "force the Corp to trash an ICE protecting " target)
    :effect (req (let [serv (next (server->zone state target))
                       servname target]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Select a piece of ICE in " target " to trash")
                      :choices {:req #(and (= (last (:zone %)) :ices)
                                           (= serv (rest (butlast (:zone %)))))}
                      :effect (req (trash state :corp target)
                                   (system-msg state side (str "trashes "
                                    (card-str state target))))}
                    card nil)))}})
