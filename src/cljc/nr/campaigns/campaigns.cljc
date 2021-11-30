(ns nr.campaigns
  (:require
    [reagent-modals.modals :as reagent-modals]
    [reagent.core :as r]))

(defrecord Campaign
  [title
   description
   url
   campaign-card])

(def available-campaigns
  {:borealis-a-breeze-in-the-dark {:title "A Breeze in the Dark"
                                   :description "Test campaign for the Borealis Booster. Some more text can follow here."
                                   :url "https://nisei.net/a-breeze-in-the-dark"}})
