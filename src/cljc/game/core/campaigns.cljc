(ns game.core.campaigns)

(defrecord Campaign
  [title
   description
   url
   campaign-card])

(def available-campaigns
  {:borealis-a-breeze-in-the-dark
   (map->Campaign {:title "A Breeze in the Dark"
                   :description "Test campaign for the Borealis Booster. Some more text can follow here."
                   :url "https://nisei.net/a-breeze-in-the-dark"
                   :campaign-card "A Breeze in the Dark"})})
