(in-ns 'game.core)

(def card-definitions-assets-rex-campaign
  {"Rex Campaign"
   (let [ability {:once :per-turn
                  :label "Remove 1 counter (start of turn)"
                  :effect (req (add-counter state side card :power -1)
                               (when (<= (get-in card [:counter :power]) 1)
                                 (trash state side card)
                                 (resolve-ability state side
                                                  {:prompt "Remove 1 bad publicity or gain 5 [Credits]?"
                                                   :choices ["Remove 1 bad publicity" "Gain 5 [Credits]"]
                                                   :msg (msg (if (= target "Remove 1 bad publicity")
                                                               "remove 1 bad publicity" "gain 5 [Credits]"))
                                                   :effect (req (if (= target "Remove 1 bad publicity")
                                                                  (lose state side :bad-publicity 1)
                                                                  (gain state side :credit 5)))}
                                                  card targets)))}]
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :ability [ability]})})
