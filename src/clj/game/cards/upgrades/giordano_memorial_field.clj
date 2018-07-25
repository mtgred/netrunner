(in-ns 'game.cards.upgrades)

(def card-definition-giordano-memorial-field
  {"Giordano Memorial Field"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req this-server)
      :msg "force the Runner to pay or end the run"
      :effect (req (let [credits (:credit runner)
                         cost (* 2 (count (:scored runner)))
                         pay-str (str "pay " cost " [Credits]")
                         c-pay-str (capitalize pay-str)]
                     (show-wait-prompt state :corp (str "Runner to " pay-str " or end the run"))
                     (continue-ability
                       state :runner
                       {:player :runner
                        :async true
                        :prompt (msg "You must " pay-str " or end the run")
                        :choices (concat (when (>= credits cost)
                                           [c-pay-str])
                                         ["End the run"])
                        :effect (req (clear-wait-prompt state :corp)
                                     (if (= c-pay-str target)
                                       (do (pay state :runner card :credit cost)
                                           (system-msg state :runner (str "pays " cost " [Credits]")))
                                       (do (end-run state side)
                                           (system-msg state :corp "ends the run")))
                                     (effect-completed state side eid))}
                       card nil)))}}}})
