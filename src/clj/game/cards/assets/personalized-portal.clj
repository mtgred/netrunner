(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-personalized-portal
  {"Personalized Portal"
   {:events {:corp-turn-begins {:effect (req (draw state :runner 1)
                                             (let [cnt (count (get-in @state [:runner :hand]))
                                                   credits (quot cnt 2)]
                                               (gain state :corp :credit credits)
                                               (system-msg state :corp
                                                           (str "uses Personalized Portal to force the runner to draw "
                                                                "1 card and gains " credits " [Credits]"))))}}}})