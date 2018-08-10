(in-ns 'game.cards.upgrades)

(def card-definition-keegan-lane
  {"Keegan Lane"
   {:abilities [{:label "[Trash], remove a tag: Trash a program"
                 :req (req (and this-server
                                (pos? (get-in @state [:runner :tag]))
                                (not (empty? (filter #(is-type? % "Program")
                                                     (all-active-installed state :runner))))))
                 :msg (msg "remove 1 tag")
                 :effect (req (resolve-ability state side trash-program card nil)
                              (trash state side card {:cause :ability-cost})
                              (lose-tags state :corp 1))}]}})
