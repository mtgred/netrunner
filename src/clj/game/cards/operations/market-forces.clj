(in-ns 'game.core)

(def card-operations-market-forces
  {"Market Forces"
   (letfn [(credit-diff [runner]
             (min (* 3 (:tag runner))
                  (:credit runner)))]
     {:req (req tagged)
      :msg (msg (let [c (credit-diff runner)]
                  (str "make the runner lose " c " [Credits], and gain " c " [Credits]")))
      :effect (req (let [c (credit-diff runner)]
                     (lose state :runner :credit c)
                     (gain state :corp :credit c)))})})
