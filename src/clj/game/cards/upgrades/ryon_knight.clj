(in-ns 'game.cards.upgrades)

(def card-definition-ryon-knight
  {"Ryon Knight"
   {:abilities [{:label "[Trash]: Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-server (zero? (:click runner))))
                 :async true
                 :effect (effect (trash card) (damage eid :brain 1 {:card card}))}]}})
