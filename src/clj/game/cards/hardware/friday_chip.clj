(in-ns 'game.cards.hardware)

(def card-definition-friday-chip
  {"Friday Chip"
   (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                  :req (req (and (pos? (get-counters card :virus))
                                 (pos? (count-virus-programs state))))
                  :choices {:req is-virus-program?}
                  :effect (req (add-counter state :runner card :virus -1)
                               (add-counter state :runner target :virus 1))}]
     {:abilities [{:effect (effect (update! (update-in card [:special :auto-accept] #(not %)))
                                   (toast (str "Friday Chip will now "
                                               (if (get-in card [:special :auto-accept]) "no longer " "")
                                               "automatically add counters.") "info"))
                   :label "Toggle auomatically adding virus counters"}]
      :effect (effect (toast "Tip: You can toggle automatically adding virus counters by clicking Friday Chip."))
      :events {:runner-turn-begins ability
               :runner-trash {:async true
                              :req (req (some #(card-is? % :side :corp) targets))
                              :effect (req (let [amt-trashed (count (filter #(card-is? % :side :corp) targets))
                                                 auto-ab {:effect (effect (system-msg :runner
                                                                                      (str "places "
                                                                                           (quantify amt-trashed "virus counter")
                                                                                           " on Friday Chip"))
                                                                    (add-counter :runner card :virus amt-trashed))}
                                                 sing-ab {:optional {:prompt "Place a virus counter on Friday Chip?"
                                                                     :yes-ability {:effect (effect (system-msg
                                                                                                     :runner
                                                                                                     "places 1 virus counter on Friday Chip")
                                                                                                   (add-counter :runner card :virus 1))}}}
                                                 mult-ab {:prompt "Place virus counters on Friday Chip?"
                                                          :choices {:number (req amt-trashed)
                                                                    :default (req amt-trashed)}
                                                          :effect (effect (system-msg :runner
                                                                                      (str "places "
                                                                                           (quantify target "virus counter")
                                                                                           " on Friday Chip"))
                                                                          (add-counter :runner card :virus target))}
                                                 ab (if (> amt-trashed 1) mult-ab sing-ab)
                                                 ab (if (get-in card [:special :auto-accept]) auto-ab ab)]
                                             (continue-ability state side ab card targets)))}}})})
