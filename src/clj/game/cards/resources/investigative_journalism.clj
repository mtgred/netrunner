(in-ns 'game.cards.resources)

(def card-definition-investigative-journalism
  {"Investigative Journalism"
   {:req (req has-bad-pub)
    :abilities [{:cost [:click 4] :msg "give the Corp 1 bad publicity"
                 :effect (effect (gain-bad-publicity :corp 1)
                                 (trash card {:cause :ability-cost}))}]}})
