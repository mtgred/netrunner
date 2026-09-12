(ns web.game-test
  (:require
   [clojure.test :refer :all]
   [game.core.diffs :as diffs]
   [web.game :as game]))

(deftest structured-notification-diff-test
  (let [state (atom {:log [] :history []})
        parts [{:username "Mirror"} " has left the game."]
        sent (atom nil)]
    (with-redefs [game/send-state-diffs (fn [_ result] (reset! sent result))]
      (game/handle-message-and-send-diffs! {:state state} nil nil parts))
    (is (= parts (get-in @state [:log 0 :public :parts])))
    (is (= "Mirror has left the game." (get-in @state [:log 0 :public :text])))
    (is (= 1 (count (:history @state))))
    (is (= (diffs/message-diffs {:log []} state) @sent))
    (is (= (:hist-diff @sent) (first (:history @state))))))
