(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-bhagat
  {"Bhagat"
   {:events {:successful-run {:req (req (and (= target :hq)
                                             (first-successful-run-on-server? state :hq)))
                              :msg "force the Corp to trash the top card of R&D"
                              :effect (effect (mill :corp))}}}})