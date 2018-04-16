(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-enhanced-vision
  {"Enhanced Vision"
   {:events {:successful-run {:silent (req true)
                              :msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand corp)))))
                              :req (req (genetics-trigger? state side :successful-run))}}}})