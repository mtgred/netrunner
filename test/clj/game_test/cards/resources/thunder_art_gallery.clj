(ns game-test.cards.resources.thunder-art-gallery
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest thunder-art-gallery
  ;; Thunder Art Gallery
  (testing "Works when removing/avoiding tags"
    (do-game
      (new-game {:runner {:deck ["Thunder Art Gallery" "New Angeles City Hall" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Thunder Art Gallery")
      (core/gain-credits state :runner 1)
      (core/gain-tags state :corp 1)
      (core/remove-tag state :runner nil)
      (click-card state :runner "New Angeles City Hall")
      (is (= 1 (:credit (get-runner))) "Runner paid one less to install (but 2 to remove tag)")
      (is (= "New Angeles City Hall" (:title (get-resource state 1))) "NACH is installed")
      (take-credits state :runner)
      (is (= 3 (:credit (get-runner))) "Runner is now at 3 credits")
      (core/gain-tags state :corp 1)
      (card-ability state :runner (get-resource state 1) 0)
      (click-prompt state :runner "Done")
      (click-card state :runner "Corroder")
      (is (= 0 (:credit (get-runner))) "Runner paid one less to install")
      (is (= "Corroder" (:title (get-program state 0))) "Corroder is installed"))))
