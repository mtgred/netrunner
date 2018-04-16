(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-personality-profiles
  {"Personality Profiles"
   (let [pp {:req (req (pos? (count (:hand runner))))
             :effect (effect (trash (first (shuffle (:hand runner)))))
             :msg (msg "force the Runner to trash " (:title (last (:discard runner))) " from their Grip at random")}]
     {:events {:searched-stack pp
               :runner-install (assoc pp :req (req (and (some #{:discard} (:previous-zone target))
                                                        (pos? (count (:hand runner))))))}})})
