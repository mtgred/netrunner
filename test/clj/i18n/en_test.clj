(ns i18n.en-test
  (:require
   [clojure.test :refer :all]
   [i18n.defs :refer :all]
   [i18n.en :refer :all]))

(defn- render-test
  [input output]
  (is (= output (render-map "en" input))))

(deftest create-game
  (render-test {:username "Corp" :type :create-game}
               "Corp has created the game."))

(deftest keep-hand
  (render-test {:username "Corp"
                :type :keep-hand}
               "Corp keeps [their] hand."))

(deftest mulligan-hand
  (render-test {:username "Runner"
                :type :mulligan-hand}
               "Runner takes a mulligan."))

(deftest mandatory-draw
  (render-test {:username "Corp"
                :type :mandatory-draw}
               "Corp makes [their] mandatory start of turn draw."))

(deftest no-action
  (render-test {:username "Corp" :type :no-action}
               "Corp has no further action."))

(deftest turn-state
  (render-test {:username "Corp"
                :type :turn-state
                :state {:phase "start-turn"
                        :turn 1
                        :credits 5
                        :cards 5
                        :side "corp"}}
               "Corp started [their] turn 1 with 5 [Credit] and 5 cards in HQ.")
  (render-test {:username "Corp"
                :type :turn-state
                :state {:phase "end-turn"
                        :turn 3
                        :credits 15
                        :cards 0
                        :side "corp"}}
               "Corp is ending [their] turn 3 with 15 [Credit] and 0 cards in HQ.")
  (render-test {:username "Runner"
                :type :turn-state
                :state {:phase "start-turn"
                        :turn 1
                        :credits 5
                        :cards 5
                        :side "runner"}}
               "Runner started [their] turn 1 with 5 [Credit] and 5 cards in [their] Grip.")
  (render-test {:username "Runner"
                :type :turn-state
                :state {:phase "end-turn"
                        :turn 3
                        :credits 15
                        :cards 0
                        :side "runner"}}
               "Runner is ending [their] turn 3 with 15 [Credit] and 0 cards in [their] Grip."))

(deftest play
  (render-test {:username "Corp"
                :type :play
                :card "Goverment Subsidy"}
               "Corp plays Goverment Subsidy.")
  (render-test {:username "Corp"
                :type :play
                :cost {:click 1}
                :card "Goverment Subsidy"}
               "Corp spends [Click] to play Goverment Subsidy."))

(deftest install
  (render-test {:username "Corp"
                :type :install
                :card-type :unknown
                :server [:servers :remote1]
                :new-remote true}
               "Corp installs a card in the root of Server 1 (new remote).")
  (render-test {:username "Corp"
                :type :install
                :card-type :unknown
                :server [:servers :remote1]
                :new-remote false}
               "Corp installs a card in the root of Server 1.")
  (render-test {:username "Corp"
                :type :install
                :card-type :unknown
                :server [:servers :hq]}
               "Corp installs a card in the root of HQ.")
  (render-test {:username "Corp"
                :type :install
                :card-type nil
                :card "Manegarm Skunkworks"
                :server [:servers :hq]}
               "Corp installs Manegarm Skunkworks in the root of HQ.")
  (render-test {:username "Corp"
                :type :install
                :card-type :ice
                :server [:servers :hq :ices]}
               "Corp installs ice protecting HQ.")
  (render-test {:username "Corp"
                :type :install
                :card-type :ice
                :server [:servers :remote3]
                :new-remote true}
               "Corp installs ice protecting Server 3 (new remote).")
  (render-test {:username "Corp"
                :type :install
                :card-type :ice
                :card "Whitespace"
                :server [:servers :remote3]}
               "Corp installs Whitespace protecting Server 3.")
  (render-test {:username "Runner"
                :type :install
                :cost {:credits 0}
                :card "Daily Casts"
                :install-source "Career Fair"
                :origin [:hand]
                :cost-bonus -3}
               "Runner pays 0 [Credits] to use Career Fair to install Daily Casts from the Grip.")
  (render-test {:username "Corp"
                :type :install
                :side :corp
                :card-type :facedown
                :origin [:discard]}
               "Corp installs an unseen card from Archives.")
  (render-test {:username "Corp"
                :type :install
                :side :corp
                :card "Manegarm Skunkworks"
                :origin [:discard]}
               "Corp installs Manegarm Skunkworks from Archives."))

(deftest rez
  (render-test {:username "Corp"
                :type :rez
                :card "Regolith Mining License"}
               "Corp rezzes Regolith Mining License.")
  (render-test {:username "Corp"
                :type :rez
                :card "Regolith Mining License"
                :ignore-cost true}
               "Corp rezzes Regolith Mining License at no cost.")
  (render-test {:username "Corp"
                :type :rez
                :card "Regolith Mining License"
                :alternative-cost true}
               "Corp rezzes Regolith Mining License by paying its alternative cost."))

;; TODO :use ?

(deftest advance
  (render-test {:username "Corp"
                :type :advance
                :cost {:click 1 :credits 1}
                :card "a card"
                :server [:servers :remote1]}
               "Corp spends [Click] and pays 1 [Credits] to advance a card in Server 1."))

(deftest score
  (render-test {:username "Corp"
                :type :score
                :card "Offworld Office"
                :points 2}
               "Corp scores Offworld Office and gains 2 agenda points."))

(deftest steal
  (render-test {:username "Runner"
                :type :steal
                :card "Offworld Office"
                :points 2}
               "Runner steals Offworld Office and gains 2 agenda points."))

;; runs
(deftest start-run
  (render-test {:username "Runner"
                :type :start-run
                :server [:servers :hq]}
               "Runner makes a run on HQ.")
  (render-test {:username "Runner"
                :type :start-run
                :server [:servers :remote1]
                :ignore-costs true}
               "Runner makes a run on Server 1, ignoring all costs."))

(deftest continue-run
  (render-test {:username "Runner" :type :continue-run}
               "Runner will continue the run."))

(deftest jack-out
  (render-test {:username "Runner" :type :jack-out :cost {:credits 1}}
               "Runner pays 1 [Credits] to jack out.")
  (render-test {:username "Runner" :type :jack-out}
               "Runner jacks out."))

(deftest approach-ice
  (render-test {:username "Runner"
                :type :approach-ice
                :ice {:pos 0 :server [:servers :hq] :card "ice"}}
               "Runner approaches ice protecting HQ at position 0."))

;; TODO this was from a unit test, need to check if bypass is supposed to have server info
(deftest bypass-ice
  (render-test {:username "Runner"
                :type :bypass-ice
                :ice "Ice Wall"}
               "Runner bypasses Ice Wall."))

(deftest encounter-ice
  (render-test {:username "Runner"
                :type :encounter-ice
                :ice {:pos 0 :server [:servers :hq] :card "Whitespace"}}
               "Runner encounters Whitespace protecting HQ at position 0."))

(deftest encounter-effect
  (render-test {:username "Runner"
                :type :encounter-effect
                :card "Tollbooth" :cost {:credits 3}}
               "Runner pays 3 [Credits] to on encountering Tollbooth."))

(deftest pass-ice
  (render-test {:username "Runner"
                :type :pass-ice
                :ice {:pos 0 :server [:servers :hq] :card "Whitespace"}}
               "Runner passes Whitespace protecting HQ at position 0."))

(deftest break-subs
  (render-test {:username "Runner"
                :type :break-subs :card "Quetzal: Free Spirit"
                :ice "Ice Wall" :subtype "Barrier " :subs '("End the run")}
               "Runner uses Quetzal: Free Spirit to break 1 Barrier subroutine on Ice Wall (\"[subroutine] End the run\").")
  (render-test {:username "Runner"
                :type :break-subs :card "Cleaver" :ice "Palisade" :subtype "Barrier"
                :sub-count 1
                :break-type :all
                :str-boost 4}
               "Runner increases the strength of Cleaver to 4 and break all 1 subroutines on Palisade.")
  (render-test {:username "Runner"
                :type :break-subs :card "Cleaver" :ice "Palisade" :subtype "Barrier"
                :sub-count 1
                :break-type :remaining}
               "Runner uses Cleaver to break the remaining 1 subroutines on Palisade."))

(deftest resolve-subs
  (render-test {:username "Corp"
                :type :resolve-subs
                :resolved {:ice "Ice Wall" :subs ["End the run"]}}
               "Corp resolves 1 unbroken subroutine on Ice Wall (\"[subroutine] End the run\")."))

(deftest approach-server
  (render-test {:username "Runner"
                :type :approach-server
                :server [:servers :archives]}
               "Runner approaches Archives."))

(deftest breach-server
  (render-test {:username "Runner"
                :type :breach-server
                :server [:servers :remote2]}
               "Runner breaches Server 2."))

(deftest access
  (render-test {:username "Runner"
                :type :access
                :card "Manegarm Skunkworks"
                :server [:servers :remote1 :content]}
               "Runner accesses Manegarm Skunkworks from Server 1.")
  (render-test {:username "Runner"
                :type :access
                :card nil
                :server [:deck]}
               "Runner accesses an unseen card from R&D.")
  ;; server side not implemented yet
  #_(render-test {}
                 "Runner accesses everything else in Archives."))

(deftest trash
  (render-test {:username "Runner" :type :trash :card "bar"}
               "Runner trashes bar.")
  (render-test {:username "Corp" :type :trash :card "bar" :server [:servers :remote1 :contents]}
               "Corp trashes bar from Server 1."))

(deftest take-damage
  (render-test {:username "Runner" :type :take-damage :cards ["bar"] :cause :net}
               "Runner trashes bar due to net damage.")
  (render-test {:username "Runner" :type :take-damage :cards ["bar"] :cause :meat}
               "Runner trashes bar due to meat damage.")
  (render-test {:username "Runner" :type :take-damage :cards ["bar" "baz"] :cause :brain}
               "Runner trashes bar and baz due to core damage."))

(deftest rfg
  (render-test {:username "Corp" :type :rfg :card "bar"}
               "Corp removes bar from the game."))

(deftest discard
  (render-test {:username "Corp" :side :corp
                :type :discard :card 2 :reason :end-turn}
               "Corp discards 2 cards from HQ at end of turn."))

(deftest win-game
  (render-test {:username "Runner" :type :win-game}
               "Runner wins the game."))

;; TODO currently relies on hack for "to" word handling
#_(deftest cost
  (render-test {:username "Corp" :cost {:click 1}}
               "Corp spends [Click].")
  (render-test {:username "Corp" :cost {:lose-click 2}}
               "Corp loses [Click][Click].")
  (render-test {:username "Corp" :cost {:credits 1}}
               "Corp pays 1 [Credits].")
  (render-test {:username "Corp" :cost {:trash "bar"}}
               "Corp trashes bar.")
  (render-test {:username "Corp" :cost {:forfeit "bar"}}
               "Corp forfeits bar.")
  (render-test {:username "Corp" :cost {:gain-tag 1}}
               "Corp takes 1 tag.")
  (render-test {:username "Corp" :cost {:tag 2}}
               "Corp removes 2 tags.")
  (render-test {:username "Corp" :cost {:bad-pub 1}}
               ;; TODO is this right? should it be takes?
               "Corp gains 1 bad publicity.")
  (render-test {:username "Corp" :cost {:return-to-hand "bar"}}
               "Corp returns bar to HQ.")
  (render-test {:username "Corp" :cost {:remove-from-game "bar"}}
               "Corp removes bar from the game.")
  (render-test {:username "Corp" :cost {:rfg-program ["bar"]}}
               "Corp removes 1 installed program from the game (bar).")
  (render-test {:username "Corp" :cost {:trash-installed ["bar"]}}
               "Corp trashes 1 installed card (bar).")
  (render-test {:username "Corp" :cost {:hardware ["bar"]}}
               "Corp trashes 1 installed piece of hardware (bar).")
  (render-test {:username "Corp" :cost {:derez ["bar"]}}
               "Corp derezzes 1 card (bar).")
  (render-test {:username "Corp" :cost {:program ["bar"]}}
               "Corp trashes 1 installed program (bar).")
  (render-test {:username "Corp" :cost {:resource ["bar"]}}
               "Corp trashes 1 installed resource (bar).")
  (render-test {:username "Corp" :cost {:connection ["bar"]}}
               "Corp trashes 1 installed connection (bar).")
  (render-test {:username "Corp" :cost {:ice ["bar"]}}
               ;; TODO eh?
               "Corp trashes 1 installed rezzed ice (bar).")
  (render-test {:username "Corp" :cost {:trash-from-deck 1}}
               "Corp trashes 1 card from the top of R&D.")
  (render-test {:username "Corp" :cost {:trash-from-hand 1}}
               "Corp trashes 1 card from HQ.")
  (render-test {:username "Corp" :cost {:trash-from-hand ["bar"]}}
               "Corp trashes 1 card (bar) from HQ.")
  (render-test {:username "Corp" :cost {:randomly-trash-from-hand 2}}
               "Corp trashes 2 cards randomly from hand.")
  (render-test {:username "Corp" :cost {:trash-entire-hand 1}}
               "Corp trashes all (1) cards in hand.")
  (render-test {:username "Corp" :cost {:trash-hardware-from-hand ["bar"]}}
               "Corp trashes 1 piece of hardware (bar) from HQ.")
  (render-test {:username "Corp" :cost {:trash-program-from-hand ["bar"]}}
               "Corp trashes 1 program (bar) from HQ.")
  (render-test {:username "Corp" :cost {:trash-resource-from-hand ["bar"]}}
               "Corp trashes 1 resource (bar) from HQ.")
  (render-test {:username "Corp" :cost {:take-net 1}}
               "Corp suffers 1 net damage.")
  (render-test {:username "Corp" :cost {:take-meat 2}}
               "Corp suffers 2 meat damage.")
  (render-test {:username "Corp" :cost {:take-core 3}}
               "Corp suffers 3 core damage.")
  (render-test {:username "Corp" :cost {:shuffle-installed-to-stack ["bar"]}}
               "Corp shuffles 1 card (bar) into R&D.")
  (render-test {:username "Corp" :cost {:add-installed-to-bottom-of-deck ["bar"]}}
               "Corp adds 1 installed card (bar) to the bottom of R&D.")
  (render-test {:username "Corp" :cost {:add-random-from-hand-to-bottom-of-deck ["bar" "baz"]}}
               "Corp adds 2 random cards from HQ to the bottom of R&D.")
  (render-test {:username "Corp" :cost {:agenda-counter ["bar" 1]}}
               "Corp spends 1 hosted agenda counter from on bar.")
  (render-test {:username "Corp" :cost {:virus ["bar" 2]}}
               "Corp spends 2 hosted virus counters from on bar.")
  (render-test {:username "Corp" :cost {:advancement ["bar" 3]}}
               "Corp spends 3 hosted advancement counters from on bar.")
  (render-test {:username "Corp" :cost {:power ["bar" 4]}}
               "Corp spends 4 hosted power counters from on bar."))

;; messing around with how "to" is rendered
#_(deftest to-check
    (render-test {:username "Corp" :type "use" :card "bar" :cost {:credits 1} :effect {:gain-credits 1}}
                 "")
    (render-test {:username "Corp" :type "use" :card "bar" :cost {:credits 1}}
                 "")
    (render-test {:username "Corp" :type "use" :card "bar" :effect {:gain-credits 1}}
                 "")
    (render-test {:username "Corp" :cost {:credits 1} :effect {:gain-credits 1}}
                 "")
    (render-test {:username "Corp" :type "use" :card "bar"}
                 "")
    (render-test {:username "Corp" :cost {:credits 1}}
                 "")
    (render-test {:username "Corp" :effect {:gain-credits 1}}
                 ""))

(deftest effect
  (render-test {:username "Corp" :type :use :card "Basic Action Card" :effect {:advance {:card "Offworld Office"}}}
               "Corp uses Basic Action Card to advance Offworld Office.")
  (render-test {:username "Corp" :type :use :card "Basic Action Card" :effect {:draw-cards 1}}
               "Corp uses Basic Action Card to draw 1 card.")
  (render-test {:username "Corp" :type :use :card "Basic Action Card" :effect {:gain-credits 1}}
               "Corp uses Basic Action Card to gain 1 [Credits].")
  (render-test {:username "Corp" :type :use :card "Luminal Transubstantation" :effect {:gain-click 3}}
               "Corp uses Luminal Transubstantation to gain [Click][Click][Click].")
  (render-test {:username "Runner" :type :use :card "Eli 1.0" :effect {:lose-click 1}}
               "Runner uses Eli 1.0 to lose [Click].")
  (render-test {:username "Corp" :type :use :card "Reversed Accounts" :effect {:lose-credits 4}}
               "Corp uses Reversed Accounts to force the Runner to lose 4 [Credits].")
  (render-test {:username "Corp" :type :use :card "Public Trail" :effect {:give-tag 1}}
               "Corp uses Public Trail to give the Runner 1 tag.")
  (render-test {:username "Runner" :type :use :card "Privileged Access" :effect {:take-tag 1}}
               "Runner uses Privileged Access to take 1 tag.")
  (render-test {:username "Runner" :type :use :card "Basic Action Card" :effect {:remove-tag 1}}
               "Runner uses Basic Action Card to remove 1 tag.")
  (render-test {:username "Corp" :type :use :card "Hostile Takeover" :effect {:take-bp 1}}
               "Corp uses Hostile Takeover to take 1 bad publicity.")

  (render-test {:username "Corp" :type :use :card "foo" :effect {:add-from-stack "bar"}}
               "Corp uses foo to add bar from the stack to the grip and shuffle the stack.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:add-from-rnd "bar"}}
               "Corp uses foo to reveal bar from R&D and add it to HQ.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:add-to-hq {:card "bar"}}}
               "Corp uses foo to add bar to HQ.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:add-to-grip {:card "bar"}}}
               "Corp uses foo to add bar to the Grip.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:add-to-hq-unseen 2}}
               "Corp uses foo to add 2 cards to HQ.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:move-to-top-stack "bar"}}
               "Corp uses foo to move bar to the top of the stack.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:shuffle-rnd true}}
               "Corp uses foo to shuffle R&D.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:reveal-and-add ["bar" [:deck] [:hand]]}}
               "Corp uses foo to add bar from R&D to HQ.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:reveal-from-hq ["bar"]}}
               "Corp uses foo to reveal bar from HQ.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:make-run [:servers :hq]}}
               "Corp uses foo to make a run on HQ.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:end-run true}}
               "Corp uses foo to end the run.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:gain-type ["bar" "Code Gate"]}}
               "Corp uses foo to make bar gain Code Gate until the end of the run.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:move-counter [:adv 1 {:card "bar"} {:card "baz"}]}}
               "Corp uses foo to move 1 advancement counter from bar to baz.")
  (render-test {:username "Runner" :type :use :card "foo" :effect {:add-str ["bar" 1]}}
               "Runner uses foo to add 1 strength to bar.")
  (render-test {:username "Runner" :type :use :card "foo" :effect {:reduce-str [{:card "bar"} 1]}}
               "Runner uses foo to give -1 strength to bar for the remainder of the encounter.")
  (render-test {:username "Runner" :type :use :card "foo" :effect {:access-additional-from-hq 1}}
               "Runner uses foo to access 1 additional card from HQ.")
  (render-test {:username "Runner" :type :use :card "foo" :effect {:access-additional-from-rnd 2}}
               "Runner uses foo to access 2 additional cards from R&D.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:deal-net 1}}
               "Corp uses foo to deal 1 net damage.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:deal-meat 2}}
               "Corp uses foo to deal 2 meat damage.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:deal-core 3}}
               "Corp uses foo to deal 3 core damage.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:install "bar"}}
               "Corp uses foo to install bar.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:rez "bar"}}
               "Corp uses foo to rez bar.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:install-and-rez-free "bar"}}
               "Corp uses foo to install and rez bar, ignoring all costs.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:host {:card "bar"}}}
               "Corp uses foo to host bar.")
  (render-test {:username "Runner" :type :use :card "foo" :effect {:bypass {:card "bar"}}}
               "Runner uses foo to bypass bar.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:trash-free "bar"}}
               "Corp uses foo to trash bar at no cost.")

  (render-test {:username "Corp" :type :use :card "foo" :effect {:shuffle-into-rnd ["bar"]}}
               "Corp uses foo to shuffle bar into R&D.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:shuffle-into-rnd ["bar" "unseen"]}}
               "Corp uses foo to shuffle 1 unseen card and bar into R&D.")
  (render-test {:username "Corp" :type :use :card "foo" :effect {:shuffle-into-rnd ["bar" nil]}}
               "Corp uses foo to shuffle itself and bar into R&D.")

  (render-test {:username "Runner" :type :use :card "Leech" :effect {:place-counter [:virus 1]}}
               "Runner uses Leech to place 1 virus counter on itself.")
  (render-test {:username "Runner" :type :use :card "Smartware Distributor" :effect {:place-counter [:credit 3]}}
               "Runner uses Smartware Distributor to place 3 [Credits] on itself.")
  (render-test {:username "Runner" :type :use :card "Cookbook" :effect {:place-counter [:virus 1 {:card "Leech"}]}}
               "Runner uses Cookbook to place 1 virus counter on Leech.")
  (render-test {:username "Runner" :type :use :card "Leech"
                :effect {:reduce-str [{:card "Ice Wall" :server [:servers :hq] :pos 1} 1]}}
               "Runner uses Leech to give -1 strength to Ice Wall protecting HQ at position 1 for the remainder of the encounter.")
  (render-test {:username "Runner" :type :use :card "Mutual Favor" :effect {:add-from-stack "Carmen"}}
               "Runner uses Mutual Favor to add Carmen from the stack to the grip and shuffle the stack.")
  (render-test {:username "Corp" :type :use :card "Malapert Data Vault" :effect {:add-from-rnd "Ice Wall"}}
               "Corp uses Malapert Data Vault to reveal Ice Wall from R&D and add it to HQ.")
  (render-test {:username "Runner" :type :use :card "Docklands Pass" :effect {:access-additional-from-hq 1}}
               "Runner uses Docklands Pass to access 1 additional card from HQ.")
  (render-test {:username "Runner" :type :use :card "Jailbreak" :effect {:access-additional-from-rnd 1}}
               "Runner uses Jailbreak to access 1 additional card from R&D.")
  (render-test {:username "Runner"
                :type :use :card "Cleaver"
                :effect {:str-pump [3 4]}}
               "Runner uses Cleaver to increase its strength from 3 to 4.")
  (render-test {:username "Runner"
                :type :use :card "Cleaver"
                :effect {:str-pump [3 4 :end-of-run]}}
               "Runner uses Cleaver to increase its strength from 3 to 4 for the remainder of the run.")
  (render-test {:username "Runner"
                :type :use :card "Cleaver"
                :effect {:str-pump [3 4 :end-of-turn]}}
               "Runner uses Cleaver to increase its strength from 3 to 4 for the remainder of the turn."))

(deftest effect-force
  (render-test {:username "Corp" :side :corp :type :use :card "foo" :effect {:take-tag-force 2}}
               "Corp uses foo to force the Runner to take 2 tags."))
