(in-ns 'game.cards.upgrades)

(def card-definition-self-destruct
  {"Self-destruct"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:req (req this-server)
                 :label "[Trash]: Trace X - Do 3 net damage"
                 :effect (req (let [serv (card->server state card)
                                    cards (concat (:ices serv) (:content serv))]
                                (trash state side card)
                                (doseq [c cards]
                                  (trash state side c))
                                (resolve-ability
                                  state side
                                  {:trace {:base (req (dec (count cards)))
                                           :successful {:msg "do 3 net damage"
                                                        :effect (effect (damage eid :net 3 {:card card}))}}}
                                  card nil)))}]}})
