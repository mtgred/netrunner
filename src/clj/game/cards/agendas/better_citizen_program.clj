(in-ns 'game.cards.agendas)

(def card-definition-better-citizen-program
  {"Better Citizen Program"
   (letfn [(ability [kind]
             (effect (show-wait-prompt :runner "Corp to use Better Citizen Program")
                     (continue-ability
                       :corp
                       {:optional
                        {:prompt "Give the runner 1 tag?"
                         :yes-ability {:async true
                                       :msg (str "give the Runner a tag for " kind)
                                       :effect (req (swap! state assoc-in [:per-turn (:cid card)] true)
                                                    (gain-tags state :corp eid 1))}
                         :end-effect (effect (clear-wait-prompt :runner))}}
                       card nil)))]
    {:events {:play-event {:req (req (and (first-event? state :runner :run)
                                          (has-subtype? target "Run")
                                          (not (used-this-turn? (:cid card) state))))
                           :async true
                           :effect (ability "playing a run event")}
              :runner-install {:silent (req true)
                               :req (req (and (has-subtype? target "Icebreaker")
                                              (first-event? state :runner :runner-install #(has-subtype? (first %) "Icebreaker"))
                                              (not (used-this-turn? (:cid card) state))))
                               :async true
                               :effect (ability "installing an icebreaker")}}})})
