(ns game-test.cards.events.glut-cipher
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest glut-cipher
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Wraparound" 2) "Hedge Fund"]}
               :runner {:deck [(qty "Glut Cipher" 3)]}})
    (take-credits state :corp)
    (trash-from-hand state :corp "Ice Wall")
    (trash-from-hand state :corp "Ice Wall")
    (trash-from-hand state :corp "Hedge Fund")
    (is (= 3 (count (:discard (get-corp)))) "There are 3 cards in Archives")
    (play-from-hand state :runner "Glut Cipher")
    (is (= 3 (count (:discard (get-corp)))) "Glut Cipher did not fire when < 5 cards")
    (is (zero? (count (filter :seen (:discard (get-corp))))) "There are no faceup cards in Archives")
    (run-on state :archives)
    (run-successful state)
    (is (= 3 (count (filter :seen (:discard (get-corp))))) "There are 3 faceup cards in Archives")
    (trash-from-hand state :corp "Wraparound")
    (trash-from-hand state :corp "Wraparound")
    (trash-from-hand state :corp "Ice Wall")
    (is (= 3 (count (filter :seen (:discard (get-corp))))) "There are 3 faceup cards in Archives")
    (is (= 6 (count (:discard (get-corp)))) "There are 6 cards in Archives")
    (play-run-event state "Glut Cipher" :archives)
    (click-card state :corp (get-discarded state :corp 0))
    (click-card state :corp (get-discarded state :corp 1))
    (click-card state :corp (get-discarded state :corp 3))
    (is (:prompt (get-corp)) "There is still a prompt")
    (click-card state :corp (get-discarded state :corp 4))
    (click-card state :corp (get-discarded state :corp 5))
    (is (nil? (-> (get-corp) :prompt first)) "Selecting 5 cards closed prompt")
    (let [discard (:discard (get-corp))]
      (is (find-card "Hedge Fund" discard) "Hedge Fund is still in Archives")
      (is (= 6 (count discard)) "There are 6 cards in Archives")
      (is (= 1 (count (filter :seen discard))) "There is 1 seen card in Archives"))
    (is (zero? (count (:hand (get-corp)))) "There are no cards in hand")))
