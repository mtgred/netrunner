(in-ns 'game.core)

(def card-definitions-events-frame-job
  {"Frame Job"
   {:prompt "Choose an agenda to forfeit"
    :choices (req (:scored runner))
    :effect (effect (forfeit target)
                    (gain-bad-publicity :corp 1))
    :msg (msg "forfeit " (:title target) " and give the Corp 1 bad publicity")}})
