(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-mumbad-virtual-tour
  {"Mumbad Virtual Tour"
   {:implementation "Only forces trash if runner has no Imps and enough credits in the credit pool"
    :flags {:must-trash true}
    :access {:req (req installed)
             :effect (req (let [trash-cost (trash-cost state side card)
                                no-salsette (remove #(= (:title %) "Salsette Slums") (all-active state :runner))
                                slow-trash (any-flag-fn? state :runner :slow-trash true no-salsette)]
                            (if (and (can-pay? state :runner nil :credit trash-cost)
                                     (not slow-trash))
                              (do (toast state :runner "You have been forced to trash Mumbad Virtual Tour" "info")
                                  (swap! state assoc-in [:runner :register :force-trash] true))
                              (toast state :runner
                                     (str "You must trash Mumbad Virtual Tour, if able, using any available means "
                                          "(Whizzard, Imp, Ghost Runner, Net Celebrity...)")))))}
    :trash-effect {:when-inactive true
                   :effect (req (swap! state assoc-in [:runner :register :force-trash] false))}}})
