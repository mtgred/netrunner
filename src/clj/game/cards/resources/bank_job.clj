(in-ns 'game.cards.resources)

(def card-definition-bank-job
  {"Bank Job"
   {:data {:counter {:credit 8}}
    :events {:successful-run
             {:silent (req true)
              :req (req (is-remote? (:server run)))
              :effect (req (let [bj (get-card state card)]
                             (when-not (:replace-access (get-in @state [:run :run-effect]))
                               (swap! state assoc-in [:run :run-effect :replace-access]
                                      {:effect (req (if (> (count (filter #(= (:title %) "Bank Job") (all-active-installed state :runner))) 1)
                                                      (resolve-ability state side
                                                        {:prompt "Select a copy of Bank Job to use"
                                                         :choices {:req #(and (installed? %) (= (:title %) "Bank Job"))}
                                                         :effect (req (let [c target
                                                                            creds (get-counters (get-card state c) :credit)]
                                                                        (resolve-ability state side
                                                                          {:prompt "How many Bank Job credits?"
                                                                           :choices {:number (req (get-counters (get-card state c) :credit))}
                                                                           :msg (msg "gain " target " [Credits]")
                                                                           :effect (req (gain-credits state side target)
                                                                                        (set-prop state side c :counter {:credit (- creds target)})
                                                                                        (when (not (pos? (get-counters (get-card state c) :credit)))
                                                                                          (trash state side c {:unpreventable true})))}
                                                                         card nil)))}
                                                       bj nil)
                                                      (resolve-ability state side
                                                        {:prompt "How many Bank Job credits?"
                                                         :choices {:number (req (get-counters (get-card state card) :credit))}
                                                         :msg (msg "gain " target " [Credits]")
                                                         :effect (req (let [creds (get-counters (get-card state card) :credit)]
                                                                        (gain-credits state side target)
                                                                        (set-prop state side card :counter {:credit (- creds target)})
                                                                        (when (not (pos? (get-counters (get-card state card) :credit)))
                                                                          (trash state side card {:unpreventable true}))))}
                                                       bj nil)))}))))}}}})
