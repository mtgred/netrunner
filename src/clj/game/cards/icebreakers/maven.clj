(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-maven
  {"Maven"
   {:abilities [(break-sub 2 1 "ICE")]
    :events (let [maven {:silent (req true)
                         :req (req (is-type? target "Program"))
                         :effect (effect (update-breaker-strength card))}]
              {:runner-install maven :trash maven :card-moved maven})
    :strength-bonus (req (count (filter #(is-type? % "Program") (all-active-installed state :runner))))}})