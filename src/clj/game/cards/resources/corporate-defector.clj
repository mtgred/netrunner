(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-corporate-defector
  {"Corporate Defector"
   {:events {:corp-click-draw {:msg (msg "reveal " (-> target first :title))}}}})
