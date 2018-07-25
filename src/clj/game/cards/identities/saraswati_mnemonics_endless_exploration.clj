(in-ns 'game.cards.identities)

(def card-definition-saraswati-mnemonics-endless-exploration
  {"Saraswati Mnemonics: Endless Exploration"
   (letfn [(install-card [chosen]
                        {:prompt "Select a remote server"
                         :choices (req (conj (vec (get-remote-names state)) "New remote"))
                         :async true
                         :effect (req (let [tgtcid (:cid chosen)]
                                        (register-turn-flag! state side
                                                             card :can-rez
                                                             (fn [state side card]
                                                               (if (= (:cid card) tgtcid)
                                                                 ((constantly false) (toast state :corp "Cannot rez due to Saraswati Mnemonics: Endless Exploration." "warning"))
                                                                 true)))
                                        (register-turn-flag! state side
                                                             card :can-score
                                                             (fn [state side card]
                                                               (if (and (= (:cid card) tgtcid)
                                                                        (>= (get-counters card :advancement) (or (:current-cost card) (:advancementcost card))))
                                                                 ((constantly false) (toast state :corp "Cannot score due to Saraswati Mnemonics: Endless Exploration." "warning"))
                                                                 true))))
                                      (wait-for (corp-install state side chosen target nil)
                                                (add-prop state :corp (find-latest state chosen) :advance-counter 1 {:placed true})
                                                (effect-completed state side eid)))})]
   {:abilities [{:async true
                 :label "Install a card from HQ"
                 :cost [:click 1 :credit 1]
                 :prompt "Select a card to install from HQ"
                 :choices {:req #(and (#{"Asset" "Agenda" "Upgrade"} (:type %))
                                      (= (:side %) "Corp")
                                      (in-hand? %))}
                 :msg (msg "install a card in a remote server and place 1 advancement token on it")
                 :effect (effect (continue-ability (install-card target) card nil))}]})})
