(in-ns 'game.cards.agendas)

(def card-definition-cfc-excavation-contract
  {"CFC Excavation Contract"
   {:effect (req (let [bios (count (filter #(has-subtype? % "Bioroid") (all-active-installed state :corp)))
                       bucks (* bios 2)]
                   (gain-credits state side bucks)
                   (system-msg state side (str "gains " bucks " [Credits] from CFC Excavation Contract"))))}})
