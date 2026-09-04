(ns game.core.schemas
  "this must not pull in any game.core namespaces, it's gotta be standalone"
  (:refer-clojure :exclude [assert])
  (:require
   [clojure.string :as str]
   [game.core.card :refer [card?]]
   [malli.core :as m]
   [malli.error :as me]
   [malli.util :as mu]))

(defmacro assert
  "malli schema assert that throws an ex-info. schema comes last to allow for threading"
  [value schema]
  `(let [schema# ~schema
         value# ~value]
     (if (m/validate schema# value#)
       value#
       (let [msg# (->> (me/humanize (m/explain schema# value#))
                       (str/join \newline))]
         (throw (ex-info msg# {:schema '~(symbol (resolve schema))
                               :value value#}))))))

(def Payment
  (m/schema
   [:map
    [:paid/type :keyword]
    [:paid/side [:enum :corp :runner]]
    [:paid/msg {:optional true} :string]
    [:paid/value :some]
    [:paid/targets {:optional true} [:maybe [:sequential :some]]]]))

;; individual keys for use in concrete schemas

(def card-schema
  [:fn {:error/message "should be a card"} card?])

;; standalone
(def $username [:username :string])
(def $do-ability [:do-ability :string])
(def $payment [:payment :string])

;; :effect
(def $add-count [:effect/add-count :int])
(def $bonus [:effect/bonus :int])
(def $card-str [:effect/card-str card-schema])
(def $card-str2 [:effect/card-str2 card-schema])
(def $card-strs [:effect/card-strs [:sequential card-schema]])
(def $cards [:effect/cards :int])
(def $choice [:effect/choice :string])
(def $count [:effect/count :int])
(def $credits [:effect/credits :int])
(def $discount [:effect/discount :int])
(def $position [:effect/position :int])
(def $seen [:effect/seen [:sequential card-schema]])
(def $server [:effect/server [:or :string :keyword]])
(def $server-n [:effect/server-n :int])
(def $title [:effect/title [:or :string card-schema]])
(def $titles [:effect/titles [:sequential [:or :string card-schema]]])
(def $top-count [:effect/top-count :int])
(def $turn [:effect/turn :int])
(def $turns [:effect/turns :int])
(def $unseen-cnt [:effect/unseen-cnt :int])
(def $value [:effect/value :int])

(defn map-schema
  [kvs]
  (m/schema (apply vector :map kvs)))

(def effect-registry (atom {}))
(defn register-effect
  [kw & kvs]
  (swap! effect-registry assoc kw (map-schema kvs))
  kw)

(register-effect :use-card $username $title $do-ability)
(register-effect :pay-use-card $username $payment $title $do-ability)

;; generic

(register-effect :do-nothing)
(register-effect :select $card-str)
(register-effect :play-card $title)
(register-effect :play-card-no-additional-costs $title)

;; trashing

(register-effect :trash-self)
(register-effect :trash-card $card-str)
(register-effect :trash-card-at-no-cost $card-str)
(register-effect :trash-n-cards $count)
(register-effect :trash-cards $count $card-strs)
(register-effect :trash-accessed-card $title)
(register-effect :trash-all-cards-in-grip)
(register-effect :trash-all-agendas-by-type)
(register-effect :trash-all-assets-by-type)
(register-effect :trash-all-events-by-type)
(register-effect :trash-all-hardware-by-type)
(register-effect :trash-all-ice-by-type)
(register-effect :trash-all-operations-by-type)
(register-effect :trash-all-resource-by-type)
(register-effect :trash-all-upgrade-by-type)
(register-effect :trash-all-cards-in-server-at-no-cost $server)

;; credits

(register-effect :gain-credits $count)
(register-effect :corp-gains-credits $count)
(register-effect :runner-gains-credits $count)

;; drawing cards

(register-effect :draw-cards $count)

;; clicks

(register-effect :gain-clicks $count)
(register-effect :lose-clicks $count)

;; tags

(register-effect :avoid-tags $count)
(register-effect :take-tags $count)
(register-effect :remove-tags $count)
(register-effect :remove-all-tags $count)

;; runner shuffling

(register-effect :shuffle-grip-into-stack)
(register-effect :shuffle-grip-and-heap-into-stack)
(register-effect :shuffle-self-into-stack)
(register-effect :shuffle-cards-into-stack $count $titles)
(register-effect :shuffle-stack)

;; corp shuffling

(register-effect :shuffle-cards-in-server-into-rd $server)
(register-effect :shuffle-cards-into-rd $count $titles)

;; score area stuff

(register-effect :forfeit $title)
(register-effect :add-self-to-score-area $value)
(register-effect :give-bad-publicity $count)

;; moving cards

(register-effect :add-self-to-grip)
(register-effect :add-card-to-grip $title)
(register-effect :add-card-to-hq $card-str)
(register-effect :add-card-from-stack-to-grip $card-str)
(register-effect :add-card-to-top-of-stack $card-str)
(register-effect :add-card-to-bottom-of-stack $card-str)
(register-effect :add-card-to-top-of-rd $title)
(register-effect :add-card-to-bottom-of-rd $title)
(register-effect :add-cards-from-heap-to-grip $titles)
(register-effect :force-add-all-hq-cards-to-top-of-rd)
(register-effect :move-seen-unseen-into-grip $seen $unseen-cnt)
(register-effect :move-seen-into-grip $seen)
(register-effect :move-unseen-into-grip $unseen-cnt)
(register-effect :move-seen-unseen-into-hq $seen $unseen-cnt)
(register-effect :move-seen-into-hq $seen)
(register-effect :move-unseen-into-hq $unseen-cnt)

;; remove from the game (rfg)

(register-effect :rfg-card $title)

;; reveal

(register-effect :expose-card $title)
(register-effect :reveal-n-cards-in-hq $count)
(register-effect :reveal-cards-in-hq $count $titles)
(register-effect :reveal-cards-in-grip $count $titles)
(register-effect :reveal-top-of-stack $title)

(register-effect :disable-corp-id)
(register-effect :disable-runner-id)

;; turns

(register-effect :take-additional-turn)
(register-effect :reduce-corp-max-hand-size-bad-publicity)
(register-effect :reduce-corp-click-next-turn $count)

;; rearrange stuff

(register-effect :rearrange-installed-ice)
(register-effect :rearrange-top-n-cards-rd $count)
(register-effect :trash-or-rearrange-top-of-stack $count)
(register-effect :swap-two-ice-positions $card-str $card-str2)

;; advancement counters

(register-effect :place-n-advancement-counters $count $card-str)
(register-effect :remove-advancement-counters $count $card-str)
(register-effect :place-virus-counters $count $title)
(register-effect :charge-card $card-str $count)

(register-effect :place-credits-on-self $credits)
(register-effect :place-credits-on-self-for-trash-costs $credits)

(register-effect :look-at-top-cards-add-to-grip $top-count $add-count)

(register-effect :guess $choice)

(register-effect :reveal-copies-of-self $count)

;; forcing

(register-effect :force-take-bad-publicity $count)
(register-effect :force-trash-installed-ice $server)
(register-effect :force-corp-trash-top-of-rd $count)
(register-effect :force-corp-trash-additional-top-of-rd $count)
(register-effect :force-corp-rez $title)
(register-effect :force-corp-trash $title)
(register-effect :force-corp-pay-credits $credits)
(register-effect :force-corp-lose-credits $credits)
(register-effect :force-corp-draw-cards $count)
(register-effect :force-corp-discard-from-hq $count)
(register-effect :force-corp-random-discard-from-hq $count)

(register-effect :force-runner-draw-cards $count)
(register-effect :force-runner-gain-credits $credits)

(register-effect :each-player-draws-cards $count)

;; runner installs

(register-effect :runner-install-card $title)

(register-effect :install-with-discount $title $discount)
(register-effect :install-from-grip $title)
(register-effect :install-from-grip-with-discount $title $discount)

(register-effect :install-from-stack $title)
(register-effect :install-from-stack-with-discount $title $discount)
(register-effect :install-program-from-heap)
(register-effect :install-program-from-stack)


;; hosting

(register-effect :host-self-as-condition-counter $card-str)
(register-effect :host-card-on-card $title $card-str)

;; rezzing

(register-effect :rez-card $card-str)
(register-effect :derez-card $card-str)
(register-effect :derez-cards $card-strs)

;; make a run

(register-effect :make-a-run)
(register-effect :make-a-run-on $server)
(register-effect :make-a-run-on-preventing-all-damage $server)
(register-effect :run-on-with-no-rezzed-ice $server)
(register-effect :rfg-to-make-a-run-on $title $server)

;; redirect run

(register-effect :redirect-run-to-archives)
(register-effect :redirect-run-to-hq)
(register-effect :redirect-run-to-rd)

;; icebreaker strength

(register-effect :give-strength-to-icebreaker-during-run $bonus $title)
(register-effect :give-strength-to-icebreaker-remainder-of-run $bonus $card-str)
(register-effect :give-strength-all-icebreakers-during-run $bonus)

;; ice

(register-effect :ice-gains-barrier-code-gate-sentry-end-of-turn $card-str)

(register-effect :bypass-ice $card-str)

(register-effect :prevent-run-ending)
(register-effect :prevent-ice-rezzed-during-run)
(register-effect :prevent-corp-rez-card-during-turn $card-str)
(register-effect :prevent-corp-rez-non-ice-on-runner-turn)
(register-effect :increase-rez-cost-first-unrezzed-approached-ice $credits)

;; prevention

(register-effect :prevent-core-damage $count)
(register-effect :prevent-net-damage $count)
(register-effect :prevent-meat-damage $count)

(register-effect :prevent-corp-advancing-cards-next-turn)

;; damage

(register-effect :suffer-meat-damage $value)
(register-effect :suffer-net-damage $value)
(register-effect :suffer-brain-damage $value)
(register-effect :suffer-core-damage $value)
(register-effect :prevent-damage-until-next-turn)

;; access

(register-effect :access-another-card)
(register-effect :access-card $card-str)
(register-effect :access-additional-in-hq $count)
(register-effect :access-additional-in-rd $count)
(register-effect :access-from-bottom-of-rd)

;; searching

(register-effect :search-stack-for-connection-resource)
(register-effect :search-stack-for-run-event)
(register-effect :search-stack-for-virtual-resource)

;; Specific card abilities

(register-effect :trash-all-installed-corp)
(register-effect :turn-all-installed-runner-facedown)
(register-effect :change-identity $title)

;; payments

(register-effect :payment-click $value)
(register-effect :payment-credit $value)
(register-effect :payment-x-credit $value)
(register-effect :payment-credit-pool $value)
(register-effect :payment-hosted-credit $value $title)
(register-effect :payment-bad-publicity $value)
(register-effect :payment-extend $title)
(register-effect :payment-trash-can)
(register-effect :payment-trash-self $title)
(register-effect :payment-forfeit $count $titles)
(register-effect :payment-gain-tags $count)
(register-effect :payment-tag $count)
(register-effect :payment-bad-publicity $count)
(register-effect :payment-return-to-grip $title)
(register-effect :payment-return-to-hq $title)
(register-effect :payment-return-from-game $title)
(register-effect :payment-rfg-program $count $titles)
(register-effect :payment-trash-installed $count $titles)
(register-effect :payment-trash-hardware $count $titles)
(register-effect :payment-trash-program $count $titles)
(register-effect :payment-trash-resource $count $titles)
(register-effect :payment-trash-connection $count $titles)
(register-effect :payment-trash-ice $count $titles)
(register-effect :payment-trash-bioroid $count $titles)
(register-effect :payment-trash-from-stack $count)
(register-effect :payment-trash-from-rd $count)
(register-effect :payment-trash-from-grip $count $titles)
(register-effect :payment-trash-from-hq $count)
(register-effect :payment-reveal-trash-from-grip $count $titles)
(register-effect :payment-reveal-trash-from-hq $count $titles)
(register-effect :payment-random-trash-from-grip $count $titles)
(register-effect :payment-random-trash-from-hq $count)
(register-effect :payment-random-reveal-trash-from-grip $count $titles)
(register-effect :payment-random-reveal-trash-from-hq $count $titles)
(register-effect :payment-trash-all-cards-in-hq $count)
(register-effect :payment-trash-all-cards-in-grip $count $titles)
(register-effect :payment-trash-hardware-in-grip $count $titles)
(register-effect :payment-trash-program-in-grip $count $titles)
(register-effect :payment-trash-resource-in-grip $count $titles)
(register-effect :payment-meat $value)
(register-effect :payment-net $value)
(register-effect :payment-core $value)
(register-effect :payment-shuffle-installed-into-stack $count $titles)
(register-effect :payment-shuffle-installed-into-rd $count)
(register-effect :payment-add-installed-bottom-stack $count $titles)
(register-effect :payment-add-installed-bottom-rd $count $titles)
(register-effect :payment-add-random-from-hand-to-bottom-of-stack $count)
(register-effect :payment-add-random-from-hand-to-bottom-of-rd $count)
(register-effect :payment-hosted-to-hq $count $titles)
(register-effect :payment-any-agenda-counter $count $title)
(register-effect :payment-any-virus-counter $count $title)
(register-effect :payment-derez-harmonic $count $titles)

;; card strs

(register-effect :card-str-runner-seen $title)
(register-effect :card-str-runner-unknown)
(register-effect :card-str-runner-hosted-seen $title)
(register-effect :card-str-runner-hosted-unknown)

(register-effect :card-str-corp-scored $title)
(register-effect :card-str-corp-rfg $title)
(register-effect :card-str-corp-play-area $title)
(register-effect :card-str-corp-destroyed $title)

(register-effect :card-str-corp-hosted-seen $title)
(register-effect :card-str-corp-hosted-known $title)
(register-effect :card-str-corp-hosted-unknown $server $server-n)
(register-effect :card-str-corp-installed-remote-seen $title $server-n)
(register-effect :card-str-corp-installed-remote-known $title $server-n)
(register-effect :card-str-corp-installed-remote-unknown $server-n)
(register-effect :card-str-corp-installed-central-seen $title $server)
(register-effect :card-str-corp-installed-central-known $title $server)
(register-effect :card-str-corp-installed-central-unknown $server)
(register-effect :card-str-corp-installed-ice-seen $title $server $position $server-n)
(register-effect :card-str-corp-installed-ice-known $title $server $position $server-n)
(register-effect :card-str-corp-installed-ice-unknown $server $position $server-n)

(def EffectMsg
  (m/schema
   `[:multi {:dispatch :effect/type}
     ~@@effect-registry]))

(comment
  (me/humanize (m/explain EffectMsg {:effect/type :add-card-to-hq
                                     :effect/title {:title "hello"}}))
  ,)

(def Msg
  (m/schema
    [:map
     [:msg/type :keyword]
     [:msg/effect-msgs [:sequential EffectMsg]]
     [:msg/payments [:sequential :map]]
     [:title [:maybe :string]]]))

(def msg-registry (atom {}))
(defn register-msg
  [kw & kvs]
  (swap! msg-registry assoc kw (mu/merge Msg (map-schema kvs)))
  kw)

(register-msg :increase-trace-strength $username $payment $value)
(register-msg :corp-start-of-turn $username $turn $credits $cards)
(register-msg :corp-end-of-turn $username $turn $credits $cards)
(register-msg :runner-start-of-turn $username $turn $credits $cards)
(register-msg :runner-end-of-turn $username $turn $credits $cards)

(register-msg :mandatory-start-of-turn-draw $username)

(register-msg :no-further-actions $username)

(register-msg :skip-discard-step $username)

(register-msg :corp-discard-cards-from-hand-eot $username $cards)
(register-msg :runner-discard-cards-from-hand-eot $username $cards)
(register-msg :extra-turns-remaining $username $turns)

(register-msg :tie)
(register-msg :win $username)
(register-msg :concede $username)
(register-msg :win-decked $username)
(register-msg :win-flatline $username)
(register-msg :clear-win $username)

(register-msg :mulligan-take $username)
(register-msg :mulligan-keep $username)

(register-msg :msg-draw-cards $username $count)
(register-msg :msg-forfeit-agenda $username $title)
(register-msg :msg-trash-card $username $card-str)
(register-msg :msg-trash-cards $username $count $titles)

(register-msg :msg-derez-card $username $card-str)
(register-msg :msg-rfg-n-cards-from-stack $username $count $card-strs)

(def MsgMap
  (m/schema
   `[:multi {:dispatch :effect/type}
     ~@@msg-registry]))
