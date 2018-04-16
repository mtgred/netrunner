(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-self-destruct
  {"Self-destruct"
   {:abilities [{:req (req this-server)
                 :label "[Trash]: Trace X - Do 3 net damage"
                 :effect (req (let [serv (card->server state card)
                                    cards (concat (:ices serv) (:content serv))]
                                (trash state side card)
                                (doseq [c cards] (trash state side c))
                                (resolve-ability
                                  state side
                                  {:trace {:base (req (dec (count cards)))
                                           :effect (effect (damage eid :net 3 {:card card}))
                                           :msg "do 3 net damage"}} card nil)))}]}})