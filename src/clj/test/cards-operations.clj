(in-ns 'test.core)

(deftest hedge-fund
  (do-game
    (new-game (default-corp) (default-runner))
    (is (= 5 (:credit (get-corp))))
    (core/play state :corp {:card (first (:hand (get-corp)))})
    (is (= 9 (:credit (get-corp))))))