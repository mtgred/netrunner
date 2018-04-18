(in-ns 'game.core)

(def card-definitions-programs-lamprey
  {"Lamprey"
   {:events {:successful-run {:req (req (= target :hq)) :msg "force the Corp to lose 1 [Credits]"
                              :effect (effect (lose :corp :credit 1))}
             :purge {:effect (effect (trash card))}}}})
