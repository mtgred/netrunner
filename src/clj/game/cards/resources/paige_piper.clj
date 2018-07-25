(in-ns 'game.cards.resources)

(def card-definition-paige-piper
  {"Paige Piper"
   (letfn [(pphelper [title cards]
             {:optional
              {:prompt (str "Use Paige Piper to trash copies of " title "?")
               :yes-ability {:prompt "How many would you like to trash?"
                             :choices (take (inc (count cards)) ["0" "1" "2" "3" "4" "5"])
                             :msg "shuffle their Stack"
                             :effect (req (let [target (str->int target)]
                                            (trigger-event state side :searched-stack nil)
                                            (shuffle! state :runner :deck)
                                            (doseq [c (take target cards)]
                                              (trash state side c {:unpreventable true}))
                                            (when (pos? target)
                                              (system-msg state side (str "trashes "
                                                                          (quantify target "cop" "y" "ies")
                                                                          " of " title)))))}}})]
     {:events {:runner-install {:req (req (first-event? state side :runner-install))
                                :async true
                                :effect (effect (continue-ability
                                                 (pphelper (:title target)
                                                           (->> (:deck runner)
                                                                (filter #(has? % :title (:title target)))
                                                                (vec)))
                                                 card nil))}}})})
