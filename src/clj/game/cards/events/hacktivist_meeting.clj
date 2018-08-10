(in-ns 'game.cards.events)

(def card-definition-hacktivist-meeting
  {"Hacktivist Meeting"
   {:implementation "Does not prevent rez if HQ is empty"
    :events {:rez {:req (req (and (not (ice? target))
                                  (pos? (count (:hand corp)))))
                   ;; FIXME the above condition is just a bandaid, proper fix would be preventing the rez altogether
                   :msg "force the Corp to trash 1 card from HQ at random"
                   :effect (effect (trash (first (shuffle (:hand corp)))))}}}})
