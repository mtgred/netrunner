(in-ns 'game.cards.operations)

(def card-definition-targeted-marketing
  {"Targeted Marketing"
   (let [gaincr {:req (req (= (:title target) (:marketing-target card)))
                 :effect (effect (gain-credits :corp 10))
                 :msg (msg "gain 10 [Credits] from " (:marketing-target card))}]
     {:prompt "Name a Runner card"
      :choices {:card-title (req (and (card-is? target :side "Runner")
                                      (not (card-is? target :type "Identity"))))}
      :effect (effect (update! (assoc card :marketing-target target))
                      (system-msg (str "uses Targeted Marketing to name " target)))
      :events {:runner-install gaincr
               :play-event gaincr}})})
