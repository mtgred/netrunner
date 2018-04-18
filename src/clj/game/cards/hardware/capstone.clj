(in-ns 'game.core)

(def card-definitions-hardware-capstone
  {"Capstone"
   {:abilities [{:req (req (> (count (:hand runner)) 0))
                 :cost [:click 1]
                 :effect (req (let [handsize (count (:hand runner))]
                                (resolve-ability state side
                                  {:prompt "Select any number of cards to trash from your Grip"
                                   :choices {:max handsize
                                             :req #(and (= (:side %) "Runner")
                                                        (in-hand? %))}
                                   :effect (req (let [trashed (count targets)
                                                      remaining (- handsize trashed)]
                                                  (doseq [c targets]
                                                    (when (not (empty? (filter #(= (:title c) (:title %))
                                                                               (all-active-installed state :runner))))
                                                      (draw state side)))
                                                  (trash-cards state side targets)
                                                  (system-msg state side
                                                    (str "spends [Click] to use Capstone to trash "
                                                      (join ", " (map :title targets)) " and draw "
                                                      (- (count (get-in @state [:runner :hand])) remaining) " cards"))))}
                                 card nil)))}]}})
