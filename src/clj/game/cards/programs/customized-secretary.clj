(in-ns 'game.core)

(def card-definitions-programs-customized-secretary
  {"Customized Secretary"
   (letfn [(custsec-host [cards]
             {:prompt "Choose a program to host on Customized Secretary"
              :choices (cons "None" cards)
              :delayed-completion true
              :effect (req (if (or (= target "None") (not (is-type? target "Program")))
                             (do (clear-wait-prompt state :corp)
                                 (shuffle! state side :deck)
                                 (system-msg state side (str "shuffles their Stack"))
                                 (effect-completed state side eid card))
                             (do (host state side (get-card state card) target)
                                 (system-msg state side (str "hosts " (:title target) " on Customized Secretary"))
                                 (continue-ability state side (custsec-host (remove-once #(= % target) cards))
                                                   card nil))))})]
     {:delayed-completion true
      :interactive (req (some #(card-flag? % :runner-install-draw true) (all-active state :runner)))
      :msg (msg "reveal the top 5 cards of their Stack: " (join ", " (map :title (take 5 (:deck runner)))))
      :effect (req (show-wait-prompt state :corp "Runner to host programs on Customized Secretary")
                   (let [from (take 5 (:deck runner))]
                     (continue-ability state side (custsec-host from) card nil)))
      :abilities [{:cost [:click 1]
                   :prompt "Choose a program hosted on Customized Secretary to install"
                   :choices (req (cancellable (filter #(can-pay? state side nil :credit (:cost %))
                                                      (:hosted card))))
                   :msg (msg "install " (:title target))
                   :effect (req (when (can-pay? state side nil :credit (:cost target))
                                  (runner-install state side target)))}]})
   "Consume"
   {:events {:runner-trash {:delayed-completion true
                            :effect (req (let [trashed targets
                                               ab {:req (req (some #(card-is? % :side :corp) trashed))
                                                   :prompt "Place virus counters on Consume?"
                                                   :choices {:number (req (count (filter #(card-is? % :side :corp) trashed)))
                                                             :default (req (count (filter #(card-is? % :side :corp) trashed)))}
                                                   :msg (msg "places " (quantify target "virus counter") " on Consume")
                                                   :effect (effect (add-counter :runner card :virus target))}] 
                                           (resolve-ability state side eid ab card targets)))}}
    :abilities [{:cost [:click 1]
                 :effect (req (gain state side :credit (* 2 (get-virus-counters state side card)))
                              (update! state side (assoc-in card [:counter :virus] 0))
                              (when-let [hiveminds (filter #(= "Hivemind" (:title %)) (all-active-installed state :runner))]
                                        (doseq [h hiveminds]
                                               (update! state side (assoc-in h [:counter :virus] 0)))))
                 :msg (msg (let [local-virus (get-in card [:counter :virus])
                                 global-virus (get-virus-counters state side card)
                                 hivemind-virus (- global-virus local-virus)]
                             (str "gain " (* 2 global-virus) " [Credits], removing " local-virus " virus counter(s) from Consume"
                             (when (pos? hivemind-virus)
                                   (str " (and " hivemind-virus " from Hivemind)")))))}]}})
