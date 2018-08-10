(in-ns 'game.cards.assets)

(def card-definition-the-board
  {"The Board"
   (let [the-board {:req (req (and (= :runner (:as-agenda-side target))
                                   (not= (:cid target) (:cid card))))
                    :effect (effect (lose :runner :agenda-point 1))}]
         {:effect (effect (lose :runner :agenda-point (count (:scored runner))))
          :leave-play (effect (gain :runner :agenda-point (count (:scored runner))))
          :trash-effect {:when-inactive true
                         :req (req (:access @state))
                         :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                         :async true
                         :effect (req (as-agenda state :runner eid card 2))}
          :events {:agenda-stolen (dissoc the-board :req)
                   :as-agenda the-board
                   :pre-card-moved {:req (req (let [c (first targets)
                                                    c-cid (:cid c)]
                                                (some #(when (= c-cid (:cid %)) %) (:scored runner))))
                                    :effect (req (gain state :runner :agenda-point 1))}}})})
