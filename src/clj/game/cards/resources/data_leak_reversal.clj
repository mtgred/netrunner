(in-ns 'game.cards.resources)

(def card-definition-data-leak-reversal
  {"Data Leak Reversal"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :abilities [{:req (req tagged) :cost [:click 1] :effect (effect (mill :corp))
                 :msg "force the Corp to trash the top card of R&D"}]}})
