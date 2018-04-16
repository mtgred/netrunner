(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-john-masanori
  {"John Masanori"
   {:events {:successful-run {:req (req (= 1 (count (get-in @state [:runner :register :successful-run]))))
                              :msg "draw 1 card" :once-key :john-masanori-draw
                              :effect (effect (draw))}
             :unsuccessful-run {:req (req (= 1 (count (get-in @state [:runner :register :unsuccessful-run]))))
                                :delayed-completion true
                                :msg "take 1 tag" :once-key :john-masanori-tag
                                :effect (effect (tag-runner :runner eid 1))}}}})