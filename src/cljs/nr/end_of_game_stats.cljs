(ns nr.end-of-game-stats
  (:require [nr.translations :refer [tr]]
            [nr.utils :refer [map-longest]]))

(defn corp-stats [s]
  [[(tr [:stats.clicks-gained "Clicks Gained"]) (get-in s [:gain :click])]
   [(tr [:stats.credits-gained "Credits Gained"]) (get-in s [:gain :credit])]
   [(tr [:stats.credits-spent "Credits Spent"]) (get-in s [:spent :credit])]
   [(tr [:stats.credits-click "Credits by Click"]) (get-in s [:click :credit])]
   [(tr [:stats.cards-drawn "Cards Drawn"]) (get-in s [:gain :card])]
   [(tr [:stats.cards-click "Cards Drawn by Click"]) (get-in s [:click :draw])]
   [(tr [:stats.damage-done "Damage Done"]) (get-in s [:damage :all])]
   [(tr [:stats.cards-rezzed "Cards Rezzed"]) (get-in s [:cards :rezzed])]])

(defn runner-stats [s]
  [[(tr [:stats.clicks-gained "Clicks Gained"]) (get-in s [:gain :click])]
   [(tr [:stats.credits-gained "Credits Gained"]) (get-in s [:gain :credit])]
   [(tr [:stats.credits-spent "Credits Spent"]) (get-in s [:spent :credit])]
   [(tr [:stats.credits-click "Credits by Click"]) (get-in s [:click :credit])]
   [(tr [:stats.cards-drawn "Cards Drawn"]) (get-in s [:gain :card])]
   [(tr [:stats.cards-click "Cards Drawn by Click"]) (get-in s [:click :draw])]
   [(tr [:stats.tags-gained "Tags Gained"]) (get-in s [:gain :tag :base])]
   [(tr [:stats.runs-made "Runs Made"]) (get-in s [:runs :started])]
   [(tr [:stats.cards-accessed "Cards Accessed"]) (get-in s [:access :cards])]])

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
        [:td.win.th (tr [:side.corp "Corp"])] [:td.win.th]
        [:td.win.th (tr [:side.runner "Runner"])] [:td.win.th]]
       (doall (map-indexed
                (fn [i [corp runner]]
                  [:tr {:key i}
                   [:td (first corp)] [:td (show-stat corp)]
                   [:td (first runner)] [:td (show-stat runner)]])
                stats))]]]))
