(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-elizas-toybox
  {"Elizas Toybox"
   {:abilities [{:cost [:click 3] :choices {:req #(not (:rezzed %))}
                 :label "Rez a card at no cost" :msg (msg "rez " (:title target) " at no cost")
                 :effect (effect (rez target {:ignore-cost :all-costs}))}]}})
