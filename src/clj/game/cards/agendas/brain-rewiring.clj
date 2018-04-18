(in-ns 'game.core)

(def card-definitions-agendas-brain-rewiring
  {"Brain Rewiring"
   {:effect (effect (show-wait-prompt :runner "Corp to use Brain Rewiring")
                    (resolve-ability
                      {:optional
                       {:prompt "Pay credits to add random cards from Runner's Grip to the bottom of their Stack?"
                        :yes-ability {:prompt "How many credits?"
                                      :choices {:number (req (min (:credit corp)
                                                                  (count (:hand runner))))}
                                      :effect (req (when (pos? target)
                                                     (pay state :corp card :credit target)
                                                     (let [from (take target (shuffle (:hand runner)))]
                                                       (doseq [c from]
                                                         (move state :runner c :deck))
                                                       (system-msg state side (str "uses Brain Rewiring to pay " target
                                                                                   " [Credits] and add " target
                                                                                   " cards from the Runner's Grip"
                                                                                   " to the bottom of their Stack."
                                                                                   " The Runner draws 1 card"))
                                                       (draw state :runner)
                                                       (clear-wait-prompt state :runner))))}
                        :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                     card nil))}})
