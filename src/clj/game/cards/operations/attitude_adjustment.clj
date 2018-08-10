(in-ns 'game.cards.operations)

(def card-definition-attitude-adjustment
  {"Attitude Adjustment"
   {:async true
    :effect (req (wait-for (draw state side 2 nil)
                           (continue-ability
                             state side
                             {:prompt "Choose up to 2 agendas in HQ or Archives"
                              :choices {:max 2
                                        :req #(and (= (:side %) "Corp")
                                                   (is-type? % "Agenda")
                                                   (or (in-hand? %)
                                                       (in-discard? %)))}
                              :effect (req (gain-credits state side (* 2 (count targets)))
                                           (doseq [c targets]
                                             (move state :corp c :deck))
                                           (shuffle! state :corp :deck)
                                           (let [from-hq (map :title (filter #(= [:hand] (:zone %)) targets))
                                                 from-archives (map :title (filter #(= [:discard] (:zone %)) targets))]
                                             (system-msg
                                               state side
                                               (str "uses Attitude Adjustment to shuffle "
                                                    (join " and "
                                                          (filter identity
                                                                  [(when (not-empty from-hq)
                                                                     (str (join " and " from-hq)
                                                                          " from HQ"))
                                                                   (when (not-empty from-archives)
                                                                     (str (join " and " from-archives)
                                                                          " from Archives"))]))
                                                    " into R&D and gain " (* 2 (count targets)) " [Credits]"))))}
                             card nil)))}})
