(in-ns 'game.core)

(def card-definitions-resources-no-one-home
  {"No One Home"
   (letfn [(start-trace [type]
             (let [message (str "avoid any " (if (= type :net) "amount of net damage" "number of tags"))]
             {:player :corp
              :label (str "Trace 0 - if unsuccessful, " message)
              :trace {:base 0
                      :priority 11
                      :unsuccessful {:msg message
                                     :effect (req (if (= type :net)
                                                    (damage-prevent state side :net Integer/MAX_VALUE)
                                                    (tag-prevent state side Integer/MAX_VALUE)))}}}))]
   {:prevent {:tag [:all]
              :damage [:net]}
    :abilities [{:msg "force the Corp to trace"
                 :delayed-completion true
                 :once :per-turn
                 :effect (req (let [type (get-in @state [:prevent :current])]
                                (when-completed (trash state side card {:unpreventable true})
                                                (continue-ability state side (start-trace type)
                                                                  card nil))))}]
    :events {:pre-resolve-damage {:silent (req true)
                                  :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}
             :pre-resolve-tag {:silent (req true)
                               :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}})
   "Off-Campus Apartment"
   {:flags {:runner-install-draw true}
    :abilities [{:label "Install and host a connection on Off-Campus Apartment"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Select a connection in your Grip to install on Off-Campus Apartment"
                                    :choices {:req #(and (has-subtype? % "Connection")
                                                         (can-pay? state side nil :credit (:cost %))
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target) " and draw 1 card")
                                    :effect (effect (runner-install target {:host-card card}))}
                                  card nil))}
                {:label "Host an installed connection"
                 :prompt "Select a connection to host on Off-Campus Apartment"
                 :choices {:req #(and (has-subtype? % "Connection")
                                      (installed? %))}
                 :msg (msg "host " (:title target) " and draw 1 card")
                 :effect (effect (host card target) (draw))}]
    :events {:runner-install {:req (req (= (:cid card) (:cid (:host target))))
                              :effect (effect (draw))}}}})
