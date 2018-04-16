(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-cfc-excavation-contract
  {"CFC Excavation Contract"
   {:effect (req (let [bios (count (filter #(has-subtype? % "Bioroid") (all-active-installed state :corp)))
                       bucks (* bios 2)]
                   (gain state side :credit bucks)
                   (system-msg state side (str "gains " bucks " [Credits] from CFC Excavation Contract"))))}})