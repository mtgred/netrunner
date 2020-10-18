(ns nr.end-of-game-stats
  (:require [nr.utils :refer [map-longest]]))

(defn corp-stats [s]
  [["Clicks Gained" (get-in s [:gain :click])]
   ["Credits Gained" (get-in s [:gain :credit])]
   ["Credits Spent" (get-in s [:spent :credit])]
   ["Credits by Click" (get-in s [:click :credit])]
   ["Cards Drawn" (get-in s [:gain :card])]
   ["Cards Drawn by Click" (get-in s [:click :draw])]
   ["Damage Done" (get-in s [:damage :all])]
   ["Cards Rezzed" (get-in s [:cards :rezzed])]])

(defn runner-stats [s]
  [["Clicks Gained" (get-in s [:gain :click])]
   ["Credits Gained" (get-in s [:gain :credit])]
   ["Credits Spent" (get-in s [:spent :credit])]
   ["Credits by Click" (get-in s [:click :credit])]
   ["Cards Drawn" (get-in s [:gain :card])]
   ["Cards Drawn by Click" (get-in s [:click :draw])]
   ["Tags Gained" (get-in s [:gain :tag])]
   ["Runs Made" (get-in s [:runs :started])]
   ["Cards Accessed" (get-in s [:access :cards])]])

(defn show-stat
  "Determines statistic counter and if it should be shown"
  [side]
  (when-let [stat (-> side second)]
    (if (pos? stat) stat "-")))

(defn build-game-stats
  "Builds the end of game statistics div & table"
  [corp runner]
  (let [stats (map-longest list nil (corp-stats corp) (runner-stats runner))]
    [:div
     [:table.win.table
      [:tbody
       [:tr.win.th
        [:td.win.th "Corp"] [:td.win.th]
        [:td.win.th "Runner"] [:td.win.th]]
       (doall (map-indexed
                (fn [i [corp runner]]
                  [:tr {:key i}
                   [:td (first corp)] [:td (show-stat corp)]
                   [:td (first runner)] [:td (show-stat runner)]])
                stats))]]]))

