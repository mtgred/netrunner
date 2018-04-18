(in-ns 'game.core)

(def card-definitions-hardware-blackguard
  {"Blackguard"
   {:in-play [:memory 2]
    :events {:expose
             {:msg (msg "attempt to force the rez of " (:title target))
              :delayed-completion true
              :effect (req (let [c target
                                 cdef (card-def c)
                                 cname (:title c)]
                             (if (:additional-cost cdef)
                               (do (show-wait-prompt state :runner (str "Corp to decide if they will rez " cname))
                                   (continue-ability state side
                                     {:optional
                                      {:prompt (msg "Pay additional cost to rez " cname "?")
                                       :player :corp
                                       :yes-ability {:effect (effect (rez :corp c)
                                                                     (clear-wait-prompt :runner))}
                                       :no-ability {:effect (effect (system-msg :corp (str "declines to pay additional costs"
                                                                                       " and is not forced to rez " cname))
                                                                    (clear-wait-prompt :runner))}}}
                                    card nil))
                               (do (rez state :corp target)
                                   (effect-completed state side eid)))))}}}})
