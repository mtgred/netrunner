(in-ns 'game.core)

(def card-hardware-dedicated-processor
  {"Dedicated Processor"
   {:implementation "Click Dedicated Processor to use ability"
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
    :hosting {:req #(and (has-subtype? % "Icebreaker")
                         (not (has-subtype? % "AI"))
                         (installed? %))}
    :abilities [{:cost [:credit 2]
                 :req (req run)
                 :effect (effect (pump (get-card state (:host card)) 4))
                 :msg (msg (str "pump the strength of " (get-in card [:host :title]) " by 4"))}]}})
