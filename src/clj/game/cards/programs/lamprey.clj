(in-ns 'game.core)

(declare can-host?)

(def card-programs-lamprey
  {"Lamprey"
   {:events {:successful-run {:req (req (= target :hq)) :msg "force the Corp to lose 1 [Credits]"
                              :effect (effect (lose :corp :credit 1))}
             :purge {:effect (effect (trash card))}}}})