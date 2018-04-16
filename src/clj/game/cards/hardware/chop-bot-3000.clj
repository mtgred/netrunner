(in-ns 'game.core)

(def card-hardware-chop-bot-3000
  {"Chop Bot 3000"
   {:flags {:runner-phase-12 (req (>= 2 (count (all-installed state :runner))))}
    :abilities [{:msg (msg "trash " (:title target))
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))
                           :not-self (req (:cid card))}
                 :effect (effect (trash target)
                                 (resolve-ability
                                   {:prompt "Draw 1 card or remove 1 tag" :msg (msg (.toLowerCase target))
                                    :choices ["Draw 1 card" "Remove 1 tag"]
                                    :effect (req (if (= target "Draw 1 card")
                                                   (draw state side)
                                                   (lose state side :tag 1)))} card nil))}]}})