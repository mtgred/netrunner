(ns nr.utils-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [nr.utils :as utils]
   [reagent.dom.server :as dom]))

(defn- render-log [message corp runner timestamp]
  (dom/render-to-static-markup
    (utils/render-system-message message corp runner timestamp)))

(deftest username-parts-test
  (doseq [translation ["Mirror" "镜子"]
          username ["Mirror" "MirrorCubeSquare" "Mirror[credit][!]" "<Mirror>"]]
    (testing (str username " with card translation " translation)
      (with-redefs [utils/card-patterns
                    (fn [] [["Mirror" [:span {:data-card-title "Mirror"} translation]]])]
        (let [message {:parts [{:username username} " uses Mirror and gains 1[credit].[hr]"
                               {:username username} " joined."]}
              html (render-log message username "Runner" "[12:00]")]
          (is (= 1 (count (re-seq #"data-card-title=" html))))
          (is (= 2 (count (re-seq #"corp-username" html))))
          (is (= 1 (count (re-seq #"timestamp-wrapper-system" html))))
          (is (= 1 (count (re-seq #"anr-icon credit" html))))
          (is (= 1 (count (re-seq #"<hr" html))))
          (is (= -1 (.indexOf html "smallwarning")))
          (is (<= 0 (.indexOf html (if (= username "<Mirror>")
                                    "&lt;Mirror&gt;" username))))
          (is (<= 0 (.indexOf html (str ">" translation "</span>")))))))))

(deftest other-username-and-warning-test
  (with-redefs [utils/card-patterns (constantly [])]
    (let [html (render-log {:parts ["[!]" {:username "Spectator[credit]"} " uses a command."]}
                           "Corp" "Runner" "[12:00]")]
      (is (<= 0 (.indexOf html "Spectator[credit]")))
      (is (<= 0 (.indexOf html "smallwarning")))
      (is (= -1 (.indexOf html "anr-icon")))
      (is (<= 0 (.indexOf html "timestamp-wrapper-system"))))))

(deftest spectator-plain-style-test
  (with-redefs [utils/card-patterns
                (fn [] [["Mirror" [:span {:data-card-title "Mirror"} "镜子"]]])]
    (doseq [text [" joined the game as a spectator." " left the game." " has left the game."]
            corp ["Corp" "Mirror[credit]"]]
      (let [html (render-log {:parts [{:username "Mirror[credit]" :spectator true} text]}
                             corp "Runner" "[12:00]")]
        (is (<= 0 (.indexOf html "Mirror[credit]")))
        (doseq [absent ["username" "timestamp" "[12:00]" "data-card-title" "anr-icon" "镜子"]]
          (is (= -1 (.indexOf html absent))))))))

(deftest unmarked-username-style-test
  (with-redefs [utils/card-patterns (constantly [])]
    (doseq [[username css-class] [["Corp" "corp-username"]
                                ["Runner" "runner-username"]
                                ["DepartedPlayer" "username"]]]
      (let [html (render-log {:parts [{:username username} " has left the game."]}
                             "Corp" "Runner" "[12:00]")]
        (is (<= 0 (.indexOf html (str "class=\"" css-class "\""))))
        (is (<= 0 (.indexOf html "timestamp-wrapper-system")))))))

(deftest legacy-message-test
  (with-redefs [utils/card-patterns (constantly [])]
    (let [text "Corp gains 1[credit]."]
      (is (= (dom/render-to-static-markup
               (utils/render-message (utils/render-player-highlight text "Corp" "Runner" nil)))
             (render-log {:text text} "Corp" "Runner" nil))))))
