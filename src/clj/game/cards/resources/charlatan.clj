(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-charlatan
  {"Charlatan"
   {:abilities [{:cost [:click 2]
                 :label "Make a run"
                 :prompt "Choose a server"
                 :choices (req runnable-servers)
                 :msg (msg "make a run on " target)
                 :effect (effect (run target nil card))}
                {:label "Pay credits equal to strength of approached rezzed ICE to bypass it"
                 :once :per-run
                 :req (req (and (:run @state) (rezzed? current-ice)))
                 :msg (msg "pay " (:current-strength current-ice) " [Credits] and bypass " (:title current-ice))
                 :effect (effect (pay :runner card :credit (:current-strength current-ice)))}]}})