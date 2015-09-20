(in-ns 'test.core)

(deftest cyberdex-virus-suite-purge
  "Cyberdex Virus Suite - Purge ability"
  (do-game
    (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
              (default-runner [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
    (take-credits state :corp 2)
    ; runner's turn
    ; install cache and medium
    (play-from-hand state :runner "Cache")
    (is (= 3 (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program])))))
    (play-from-hand state :runner "Medium")
    (take-credits state :runner 2)
    (let [cvs (first (get-in @state [:corp :servers :hq :content]))]
      (core/rez state :corp cvs)
      (card-ability state :corp (core/get-card state cvs) 0)
      ; nothing in hq content
      (is (empty? (get-in @state [:corp :servers :hq :content])) "CVS was trashed")
      ; purged counters
      (is (zero? (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program]))))
          "Cache has no counters")
      (is (zero? (core/get-virus-counters state :runner (find-card "Medium" (get-in @state [:runner :rig :program]))))
          "Medium has no counters"))))

(deftest cyberdex-virus-suite-access
  "Cyberdex Virus Suite - Purge on access"
  (do-game
    (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
              (default-runner [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :corp "Cyberdex Virus Suite" "New remote")
    (take-credits state :corp 2)
    ; runner's turn
    ; install cache and medium
    (play-from-hand state :runner "Cache")
    (is (= 3 (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program])))))
    (play-from-hand state :runner "Medium")
    (core/click-run state :runner {:server "Server 0"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    ; corp now has optional prompt to trigger virus purge
    (core/resolve-prompt state :corp {:choice "Yes"})
    ; runner has prompt to trash CVS
    (core/resolve-prompt state :runner {:choice "Yes"})
    ; purged counters
    (is (zero? (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program]))))
        "Cache has no counters")
    (is (zero? (core/get-virus-counters state :runner (find-card "Medium" (get-in @state [:runner :rig :program]))))
        "Medium has no counters")))
