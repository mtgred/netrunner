(in-ns 'game.cards.events)

(def card-definition-glut-cipher
  {"Glut Cipher"
   (let [corp-choose {:show-discard true
                      :async true
                      :player :corp
                      :prompt (msg "Select 5 cards from Archives to add to HQ")
                      :choices {:max 5
                                :all true
                                :req #(and (= (:side %) "Corp")
                                           (= (:zone %) [:discard]))}
                      :msg (msg "move "
                                (let [seen (filter :seen targets)
                                      m (count  (remove :seen targets))]
                                  (str (join ", " (map :title seen))
                                       (when (pos? m)
                                         (str (when-not (empty? seen) " and ")
                                              (quantify m "unseen card")))
                                       " into HQ, then trash 5 cards")))
                      :effect (req (wait-for
                                     (resolve-ability state side
                                                      {:effect (req (doseq [c targets]
                                                                      (move state side c :hand)))}
                                                      card targets)
                                     (continue-ability state side
                                                       {:async true
                                                        :effect (req (doseq [c (take 5 (shuffle (:hand corp)))]
                                                                       (trash state :corp c))
                                                                     (clear-wait-prompt state :runner)
                                                                     (effect-completed state :runner eid))}
                                                       card nil)))}
         access-effect {:mandatory true
                        :async true
                        :req (req (>= (count (:discard corp)) 5))
                        :effect (req (show-wait-prompt
                                       state :runner
                                       "Corp to choose which cards to pick up from Archives") ;; For some reason it just shows successful-run-trigger-message, but this works!?
                                     (continue-ability state side
                                                       corp-choose
                                                       card nil))}]
     {:req (req archives-runnable)
      :makes-run true
      :effect (effect (run :archives
                           {:req (req (= target :archives))
                            :replace-access access-effect}
                           card))})})
