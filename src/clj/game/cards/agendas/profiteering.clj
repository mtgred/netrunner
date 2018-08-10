(in-ns 'game.cards.agendas)

(def card-definition-profiteering
  {"Profiteering"
   {:interactive (req true)
    :choices ["0" "1" "2" "3"] :prompt "How many bad publicity?"
    :msg (msg "take " target " bad publicity and gain " (* 5 (str->int target)) " [Credits]")
    :effect (req (let [bp (:bad-publicity (:corp @state))]
                   (gain-bad-publicity state :corp eid (str->int target))
                   (if (< bp (:bad-publicity (:corp @state)))
                     (gain-credits state :corp (* 5 (str->int target))))))}})
