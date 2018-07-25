(in-ns 'game.cards.assets)

(def card-definition-warden-fatuma
  {"Warden Fatuma"
   (let [new-sub {:label "[Warden Fatuma] Force the Runner to lose 1 [Click], if able"}]
     (letfn [(all-rezzed-bios [state]
               (filter #(and (ice? %)
                             (has-subtype? % "Bioroid")
                             (rezzed? %))
                       (all-installed state :corp)))
             (remove-one [cid state ice]
               (remove-extra-subs state :corp cid ice))
             (add-one [cid state ice]
               (add-extra-sub state :corp cid ice 0 new-sub))
             (update-all [state func]
               (doseq [i (all-rezzed-bios state)]
                 (func state i)))]
       {:effect (req (system-msg
                       state :corp
                       "uses Warden Fatuma to add \"[Subroutine] The Runner loses [Click], if able\" before all other subroutines")
                  (update-all state (partial add-one (:cid card))))
        :leave-play (req (system-msg state :corp "loses Warden Fatuma additional subroutines")
                      (update-all state (partial remove-one (:cid card))))
        :sub-effect {:msg "force the Runner to lose 1 [Click], if able"
                     :effect (req (lose state :runner :click 1))}
        :events {:rez {:req (req (and (ice? target)
                                      (has-subtype? target "Bioroid")))
                       :effect (req (add-one (:cid card) state (get-card state target)))}}}))})
