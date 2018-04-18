(in-ns 'game.core)

(def card-definitions-operations-ad-blitz
  {"Ad Blitz"
   (let [abhelp (fn ab [n total]
                  {:prompt "Select an Advertisement to install and rez" :show-discard true
                   :delayed-completion true
                   :choices {:req #(and (= (:side %) "Corp")
                                        (has-subtype? % "Advertisement")
                                        (or (in-hand? %)
                                            (= (:zone %) [:discard])))}
                   :effect (req (when-completed
                                  (corp-install state side target nil {:install-state :rezzed})
                                  (if (< n total)
                                    (continue-ability state side (ab (inc n) total) card nil)
                                    (effect-completed state side eid))))})]
     {:prompt "How many Advertisements?"
      :delayed-completion true
      :choices :credit
      :msg (msg "install and rez " target " Advertisements")
      :effect (effect (continue-ability (abhelp 1 target) card nil))})})
