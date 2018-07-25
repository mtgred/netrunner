(in-ns 'game.cards.resources)

(def card-definition-hunting-grounds
  {"Hunting Grounds"
   {:abilities [{:label "Prevent a \"when encountered\" ability on a piece of ICE"
                 :msg "prevent a \"when encountered\" ability on a piece of ICE"
                 :once :per-turn}
                 {:label "[Trash]: Install the top 3 cards of your Stack facedown"
                  :msg "install the top 3 cards of their Stack facedown"
                  :effect (req (trash state side card {:cause :ability-cost})
                               (doseq [c (take 3 (get-in @state [:runner :deck]))]
                                 (runner-install state side c {:facedown true})))}]}})
