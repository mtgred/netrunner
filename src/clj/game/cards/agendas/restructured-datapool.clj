(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-restructured-datapool
  {"Restructured Datapool"
   {:abilities [{:cost [:click 1]
                 :trace {:base 2
                         :msg "give the Runner 1 tag"
                         :delayed-completion true
                         :effect (effect (tag-runner :runner eid 1))}}]}})