(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-cybernetics-court
  {"Cybernetics Court"
   {:in-play [:hand-size-modification 4]}})