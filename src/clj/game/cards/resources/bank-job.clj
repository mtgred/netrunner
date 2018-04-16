(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-bank-job
  {"Bank Job"
   {:data {:counter {:credit 8}}
    :events {:successful-run
             {:silent (req true)
              :req (req (is-remote? (:server run)))
              :effect (req (let [bj card]
                             (when-not (:replace-access (get-in @state [:run :run-effect]))
                               (swap! state assoc-in [:run :run-effect :replace-access]
                                      {:effect (req (if (> (count (filter #(= (:title %) "Bank Job") (all-active-installed state :runner))) 1)
                                                      (resolve-ability state side
                                                        {:prompt "Select a copy of Bank Job to use"
                                                         :choices {:req #(and installed? (= (:title %) "Bank Job"))}
                                                         :effect (req (let [c target
                                                                            creds (get-in c [:counter :credit])]
                                                                        (resolve-ability state side
                                                                          {:prompt "How many Bank Job credits?"
                                                                           :choices {:number (req (get-in c [:counter :credit]))}
                                                                           :msg (msg "gain " target " [Credits]")
                                                                           :effect (req (gain state side :credit target)
                                                                                        (set-prop state side c :counter {:credit (- creds target)})
                                                                                        (when (= target creds)
                                                                                          (trash state side c {:unpreventable true})))}
                                                                         card nil)))}
                                                       bj nil)
                                                      (resolve-ability state side
                                                        {:prompt "How many Bank Job credits?"
                                                         :choices {:counter :credit}
                                                         :msg (msg "gain " target " [Credits]")
                                                         :effect (req (gain state side :credit target)
                                                                      (when (= target (get-in card [:counter :credit]))
                                                                        (trash state side card {:unpreventable true})))}
                                                       bj nil)))}))))}}}})
