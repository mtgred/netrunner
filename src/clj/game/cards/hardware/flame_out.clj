(in-ns 'game.cards.hardware)

(def card-definition-flame-out
  {"Flame-out"
   (let [turn-end {:async true
                   :effect (req (unregister-events state :runner card)
                                (if-let [hosted (first (:hosted card))]
                                  (do
                                    (system-msg state :runner (str "trashes " (:title hosted) " from Flame-out"))
                                    (trash state side eid hosted nil))
                                  (effect-completed state side eid)))}]
   {:implementation "Credit usage restriction not enforced"
    :data {:counter {:credit 9}}
    :abilities [{:label "Take 1 [Credits] from Flame-out"
                 :req (req (and (not-empty (:hosted card))
                                (pos? (get-counters card :credit))))
                 :counter-cost [:credit 1]
                 :effect (req (gain-credits state :runner 1)
                              (system-msg state :runner "takes 1 [Credits] from Flame-out")
                              (register-events
                                state :runner
                                {:runner-turn-ends turn-end
                                 :corp-turn-ends turn-end}
                                (get-card state card)))}
                {:label "Take all [Credits] from Flame-out"
                 :req (req (and (not-empty (:hosted card))
                                (pos? (get-counters card :credit))))
                 :effect (req (let [credits (get-counters card :credit)]
                                (gain-credits state :runner credits)
                                (update! state :runner (dissoc-in card [:counter :credit]))
                                (system-msg state :runner (str "takes " credits "[Credits] from Flame-out"))
                                (register-events
                                  state :runner
                                  {:runner-turn-ends turn-end
                                   :corp-turn-ends turn-end}
                                  (get-card state card))))}
                 {:label "Install a program on Flame-out"
                 :req (req (empty? (:hosted card)))
                 :cost [:click 1]
                 :prompt "Select a program in your Grip to install on Flame-out"
                 :choices {:req #(and (is-type? % "Program")
                                      (in-hand? %))}
                 :effect (effect (runner-install target {:host-card card})
                                 (update! (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}
                {:label "Host an installed program on Flame-out"
                 :req (req (empty? (:hosted card)))
                 :prompt "Select an installed program to host on Flame-out"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (->> target
                                (get-card state)
                                (host state side card))
                              (update! state side (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}]
    :events {:card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :flame-out])))
                          :effect (effect (update! (dissoc-in card [:special :flame-out])))}
             :runner-turn-ends nil
             :corp-turn-ends nil}})})
