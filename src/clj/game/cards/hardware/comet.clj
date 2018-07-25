(in-ns 'game.cards.hardware)

(def card-definition-comet
  {"Comet"
   {:in-play [:memory 1]
    :events {:play-event {:req (req (first-event? state side :play-event))
                          :effect (req (system-msg state :runner
                                                   (str "can play another event without spending a [Click] by clicking on Comet"))
                                       (update! state side (assoc card :comet-event true)))}}
    :abilities [{:req (req (:comet-event card))
                 :prompt "Select an Event in your Grip to play"
                 :choices {:req #(and (is-type? % "Event")
                                      (in-hand? %))}
                 :msg (msg "play " (:title target))
                 :effect (effect (play-instant target)
                                 (update! (dissoc (get-card state card) :comet-event)))}]}})
