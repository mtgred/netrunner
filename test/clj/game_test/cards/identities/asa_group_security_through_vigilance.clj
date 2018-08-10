(ns game-test.cards.identities.asa-group-security-through-vigilance
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest asa-group-security-through-vigilance
  (testing "Asa Group should not allow installing operations"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "BOOM!" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "BOOM!" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))
  (testing "Asa Group should not allow installing agendas"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "Project Vitruvius" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "Project Vitruvius" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group did not install Agenda with its ability")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))
  (testing "Asa Group ordering correct when playing Mirrormorph"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Shipment from MirrorMorph"
                               "Pup"
                               "Red Herrings"
                               "Marilyn Campaign"
                               "Project Vitruvius"]}})
      (let  [marilyn (find-card "Marilyn Campaign" (:hand (get-corp)))
             pup (find-card "Pup" (:hand (get-corp)))
             herrings (find-card "Red Herrings" (:hand (get-corp)))
             vitruvius (find-card "Project Vitruvius" (:hand (get-corp)))]
        (play-from-hand state :corp "Shipment from MirrorMorph")
        (click-card state :corp marilyn)
        (click-prompt state :corp "New remote")
        (is (= (:cid marilyn) (:cid (get-content state :remote1 0))) "Marilyn is installed as first card")
        (click-card state :corp herrings) ;; This should be the Asa prompt, should be automatically installed in remote1
        (is (= (:cid herrings) (:cid (get-content state :remote1 1))) "Red Herrings is installed in Server 1")
        (click-card state :corp vitruvius)
        (click-prompt state :corp "New remote")
        (click-card state :corp pup)
        (click-prompt state :corp "New remote")
        (is (empty? (:prompt (get-corp))) "No more prompts")
        (is (= 6 (count (:servers (get-corp)))) "There are six servers, including centrals"))))
  (testing "don't allow installation of operations"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "BOOM!" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "BOOM!" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote"))))
