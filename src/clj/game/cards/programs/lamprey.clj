(in-ns 'game.cards.programs)

(def card-definition-lamprey
  {"Lamprey"
   {:events {:successful-run {:req (req (= target :hq))
                              :msg "force the Corp to lose 1 [Credits]"
                              :effect (effect (lose-credits :corp 1))}
             :purge {:effect (effect (trash card {:cause :purge}))}}}})
