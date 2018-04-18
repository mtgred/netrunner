(in-ns 'game.core)

(def card-definitions-operations-invasion-of-privacy
  {"Invasion of Privacy"
   (letfn [(iop [x]
             {:delayed-completion true
              :req (req (pos? (count (filter #(or (is-type? % "Resource")
                                                  (is-type? % "Event")) (:hand runner)))))
              :prompt "Choose a resource or event to trash"
              :msg (msg "trash " (:title target))
              :choices (req (cancellable
                              (filter #(or (is-type? % "Resource")
                                           (is-type? % "Event")) (:hand runner)) :sorted))
              :effect (req (trash state side target)
                           (if (pos? x)
                             (continue-ability state side (iop (dec x)) card nil)
                             (effect-completed state side eid card)))})]
     {:trace {:base 2 :msg "reveal the Runner's Grip and trash up to X resources or events"
              :effect (req (let [x (- target (second targets))]
                             (system-msg state :corp
                                         (str "reveals the Runner's Grip ( "
                                              (join ", " (map :title (sort-by :title (:hand runner))))
                                              " ) and can trash up to " x " resources or events"))
                             (continue-ability state side (iop (dec x)) card nil)))
              :unsuccessful {:msg "take 1 bad publicity" :effect (effect (gain-bad-publicity :corp 1))}}})})
