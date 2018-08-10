(in-ns 'game.cards.upgrades)

(def card-definition-k-p-lynn
  {"K. P. Lynn"
   (let [abi {:prompt "Choose one"
              :player :runner
              :choices ["Take 1 tag" "End the run"]
              :effect (req (if (= target "Take 1 tag")
                             (do (gain-tags state :runner 1)
                                 (system-msg state :corp (str "uses K. P. Lynn. Runner chooses to take 1 tag")))
                             (do (end-run state side)
                                 (system-msg state :corp (str "uses K. P. Lynn. Runner chooses to end the run")))))}]
     {:events {:pass-ice {:req (req (and this-server (= (:position run) 1))) ; trigger when last ice passed
                          :async true
                          :effect (req (continue-ability state :runner abi card nil))}
               :run {:req (req (and this-server
                                    (zero? (:position run)))) ; trigger on unprotected server
                     :async true
                     :effect (req (continue-ability state :runner abi card nil))}}})})
