(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-warroid-tracker
  {"Warroid Tracker"
   (letfn [(wt [card n t]
             {:prompt "Choose an installed card to trash due to Warroid Tracker"
              :delayed-completion true
              :player :runner
              :priority 2
              :choices {:req #(and (installed? %) (= (:side %) "Runner"))}
              :effect (req (system-msg state side (str "trashes " (card-str state target) " due to Warroid Tracker"))
                           (trash state side target {:unpreventable true})
                           (if (> n t)
                             (continue-ability state side (wt card n (inc t)) card nil)
                             (do (clear-wait-prompt state :corp)
                                 (effect-completed state side eid card)))
                           ;; this ends-the-run if WT is the only card and is trashed, and trashes at least one runner card
                           (when (zero? (count (cards-to-access state side (get-in @state [:run :server]))))
                             (handle-end-run state side)))})]
   {:implementation "Does not handle UFAQ interaction with Singularity"
    :events {:runner-trash {:delayed-completion true
                            :req (req (= (-> card :zone second) (-> target :zone second)))
                            :trace {:base 4
                                    :effect (req (let [n (count (all-installed state :runner))
                                                       n (if (> n 2) 2 n)]
                                                   (if (pos? n) (do (system-msg state side (str "uses Warroid Tracker to force the runner to trash " n " installed card(s)"))
                                                                    (show-wait-prompt state :corp "Runner to choose cards to trash")
                                                                    (resolve-ability state side (wt card n 1) card nil))
                                                                (system-msg state side (str "uses Warroid Tracker but there are no installed cards to trash")))))}}}})})
