(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-ryon-knight
  {"Ryon Knight"
   {:abilities [{:label "[Trash]: Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-server (zero? (:click runner))))
                 :delayed-completion true
                 :effect (effect (trash card) (damage eid :brain 1 {:card card}))}]}})
