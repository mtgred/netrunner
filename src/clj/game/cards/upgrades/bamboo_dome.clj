(in-ns 'game.cards.upgrades)

(def card-definition-bamboo-dome
  {"Bamboo Dome"
   (letfn [(dome [dcard]
             {:prompt "Select a card to add to HQ"
              :async true
              :choices {:req #(and (= (:side %) "Corp")
                                   (= (:zone %) [:play-area]))}
              :msg "move a card to HQ"
              :effect (effect (move target :hand)
                              (continue-ability (put dcard) dcard nil))})
           (put [dcard]
             {:prompt "Select first card to put back onto R&D"
              :async true
              :choices {:req #(and (= (:side %) "Corp")
                                   (= (:zone %) [:play-area]))}
              :msg "move remaining cards back to R&D"
              :effect (effect (move target :deck {:front true})
                              (move (first (get-in @state [:corp :play-area])) :deck {:front true})
                              (clear-wait-prompt :runner)
                              (effect-completed eid))})]
    {:init {:root "R&D"}
     :install-req (req (filter #{"R&D"} targets))
     :abilities [{:cost [:click 1]
                  :req (req (>= (count (:deck corp)) 3))
                  :async true
                  :msg (msg (str "reveal " (join ", " (map :title (take 3 (:deck corp)))) " from R&D"))
                  :label "Reveal the top 3 cards of R&D. Secretly choose 1 to add to HQ. Return the others to the top of R&D, in any order."
                  :effect (req (doseq [c (take 3 (:deck corp))]
                                 (move state side c :play-area))
                            (show-wait-prompt state :runner "Corp to use Bamboo Dome")
                            (continue-ability state side (dome card) card nil))}]})})
