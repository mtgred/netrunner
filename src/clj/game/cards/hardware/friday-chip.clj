(in-ns 'game.core)

(def card-definitions-hardware-friday-chip
  {"Friday Chip"
   (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                  :req (req (and (pos? (get-in card [:counter :virus] 0))
                                 (pos? (count-virus-programs state))))
                  :choices {:req #(and (has-subtype? % "Virus")
                                       (is-type? % "Program"))}
                  :effect (req (add-counter state :runner card :virus -1)
                               (add-counter state :runner target :virus 1))}]
     {:events {:runner-turn-begins ability
               :runner-trash {:req (req (= (:side target) "Corp"))
                              :optional
                              {:prompt "Gain a virus counter on Friday Chip?"
                               :yes-ability
                               {:effect (effect (add-counter :runner card :virus 1)
                                                (system-msg :runner (str "places 1 virus counter on Friday Chip")))}}}}})})
