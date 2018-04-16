(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-underway-renovation
  {"Underway Renovation"
   (letfn [(adv4? [s c] (if (>= (:advance-counter (get-card s c)) 4) 2 1))]
     {:install-state :face-up
      :events {:advance {:req (req (= (:cid card) (:cid target)))
                         :msg (msg (if (pos? (count (:deck runner)))
                                     (str "trash "
                                          (join ", " (map :title (take (adv4? state card) (:deck runner))))
                                          " from the Runner's stack")
                                     "trash from the Runner's stack but it is empty"))
                         :effect (effect (mill :corp :runner (adv4? state card)))}}})})