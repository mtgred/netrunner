(in-ns 'game.cards.operations)

(def card-definition-market-forces
  {"Market Forces"
   (letfn [(credit-diff [runner]
             (min (* 3 (:tag runner))
                  (:credit runner)))]
     {:req (req tagged)
      :msg (msg (let [c (credit-diff runner)]
                  (str "make the runner lose " c " [Credits], and gain " c " [Credits]")))
      :effect (req (let [c (credit-diff runner)]
                     (lose-credits state :runner c)
                     (gain-credits state :corp c)))})})
