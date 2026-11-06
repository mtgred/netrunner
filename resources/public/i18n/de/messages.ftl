### templates???

## Terms aka consistent names
# Terminologie nach den deutschen NetrunnerDB-Übersetzungen
# (netrunner-cards-json/translations/de).
# Fragmente unter "Ability fragments" werden in msg_use-card/msg_pay-use-card
# nach "um" eingesetzt und sind deshalb als Infinitiv mit "zu" formuliert.

-corp = {$case ->
    [genitive] Konzern
    [accusative] den Konzern
    [dative] dem Konzern
    [possessive] des Konzerns
    *[nominative] der Konzern
}

-runner = {$case ->
    [genitive] Runner
    [accusative] den Runner
    [dative] dem Runner
    [possessive] des Runners
    *[nominative] der Runner
}

-archives = Archiv
-hq = HQ
-rd = F&E

-grip = den Griff
-heap = die Ablage
-stack = den Stapel

-credit = [Credits]
-click = [Click]
-bad-publicity = negative Publicity
-tag = Markierung

-credit-pool = Credit-Vorrat

## card types

-agenda = Agenda
-agenda-plural = Agenden
-asset = Aktivposten
-asset-plural = Aktivposten
-event = Ereignis
-event-plural = Ereignisse
-hardware = Hardware
-hardware-plural = Hardware
-ice = Ice
-ice-plural = Ice
-ice-type = ICE
-identity = Identität
-identity-plural = Identitäten
-operation = Operation
-operation-plural = Operationen
-program = Programm
-program-plural = Programme
-resource = Ressource
-resource-plural = Ressourcen
-upgrade = Upgrade
-upgrade-plural = Upgrades

## damage types

damage-name = {$damage ->
    [meat] Körper-Schaden
    [net] Netz-Schaden
    [core] Substanz-Schaden
    *[other] unbekannten Schaden
}

## locations

server-name = {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] System {$server}
}

## Logical templates

msg_play-card = {$username} spielt {$title}.
msg_pay-play-card = {$username} {$payment}, um {$title} zu spielen.
msg_use-card = {$username} verwendet {$title}, um {$do-ability}.
msg_pay-use-card = {$username} {$payment} und verwendet {$title}, um {$do-ability}.
msg_satisfy-card = {$username} {$payment}, um {$title} zu erfüllen.

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}und{" "}
join-list = ,{" "}

## Ability fragments

# generic

do-nothing = nichts zu tun
play-card = {$title} zu spielen
play-card-no-additional-costs = {$title} zu spielen und dabei zusätzliche Kosten zu ignorieren
select = {$card-str} auszuwählen

# trashing cards

trash-self = sich selbst zu zerstören
trash-card = {$card-str} zu zerstören
trash-card-at-no-cost = {$card-str} kostenlos zu zerstören
trash-n-cards = {$count ->
    [zero] keine Karten
    [one] 1 Karte
    *[other] {$count} Karten
} zu zerstören
trash-cards = {$count ->
    [zero] keine Karten
    [one] 1 Karte ({$card-strs})
    *[other] {$count} Karten ({$card-strs})
} zu zerstören
trash-n-cards-from-hq = {$count ->
    [zero] keine Karten
    [one] 1 Karte
    *[other] {$count} Karten
} aus dem {-hq} zu zerstören
trash-cards-from-hq = {$count ->
    [zero] keine Karten
    [one] 1 Karte ({$card-strs})
    *[other] {$count} Karten ({$card-strs})
} aus dem {-hq} zu zerstören

trash-accessed-card = die Karte, auf die zugegriffen wird ({$title}), zu zerstören
trash-all-cards-in-grip = alle Karten im Griff zu zerstören
trash-all-agendas-by-type = alle {-agenda-plural} zu zerstören
trash-all-assets-by-type = alle {-asset-plural} zu zerstören
trash-all-events-by-type = alle {-event-plural} zu zerstören
trash-all-hardware-by-type = alle {-hardware-plural} zu zerstören
trash-all-ice-by-type = alle {-ice-type} zu zerstören
trash-all-operations-by-type = alle {-operation-plural} zu zerstören
trash-all-resource-by-type = alle {-resource-plural} zu zerstören
trash-all-upgrade-by-type = alle {-upgrade-plural} zu zerstören

trash-all-cards-in-server-at-no-cost = alle Karten in {$server} kostenlos zu zerstören

# credits

gain-credits = {$count} {-credit} zu erhalten
corp-gains-credits = {-corp(case: "accusative")} {$count} {-credit} erhalten zu lassen
runner-gains-credits = {-runner(case: "accusative")} {$count} {-credit} erhalten zu lassen

# drawing cards

draw-cards = {$count ->
    [one] 1 Karte
    *[other] {$count} Karten
} zu ziehen

# clicks

gain-clicks = {$count ->
    [one] [Click]
    *[other] {$count} [Click]
} zu erhalten

lose-clicks = {$count ->
    [one] [Click]
    *[other] {$count} [Click]
} zu verlieren

# tags

avoid-tags = {$count ->
    [one] 1 Markierung
    *[other] {$count} Markierungen
} zu vermeiden
give-tags = {-runner(case: "dative")} {$count ->
    [one] 1 Markierung
    *[other] {$count} Markierungen
} zu verpassen
take-tags = {$count ->
    [one] 1 Markierung
    *[other] {$count} Markierungen
} zu erhalten
remove-tags = {$count ->
    [one] 1 Markierung
    *[other] {$count} Markierungen
} zu entfernen
remove-all-tags = alle Markierungen ({$count}) zu entfernen

# runner shuffling

shuffle-grip-into-stack = { -grip } in { -stack } zu mischen
shuffle-grip-and-heap-into-stack = { -grip } und { -heap } in { -stack } zu mischen
shuffle-self-into-stack = sich selbst in {-stack} zu mischen
shuffle-cards-into-stack = {$count ->
    [zero] 0 Karten
    [one] {$titles}
    *[other] {$count} Karten ({$titles})
} in {-stack} zu mischen
shuffle-stack = {-stack} zu mischen

# corp shuffling

shuffle-cards-in-server-into-rd = alle Karten in {$server} in {-rd} zu mischen
shuffle-cards-into-rd = {$count ->
    [zero] 0 Karten
    [one] {$titles}
    *[other] {$count} Karten ({$titles})
} in {-rd} zu mischen

# score area stuff

forfeit = {$title} aufzugeben
add-self-to-score-area = sich selbst als Agenda mit {$value ->
    [one] 1 Agendapunkt
    *[other] {$value} Agendapunkten
} dem Punktebereich hinzuzufügen

give-bad-publicity = {-corp(case: "dative")} {$count} { -bad-publicity } zu verpassen

# moving cards

add-self-to-grip = sich selbst dem Griff hinzuzufügen
add-card-to-grip = {$title} dem Griff hinzuzufügen
add-card-to-hq = {$card-str} dem {-hq} hinzuzufügen

add-card-from-stack-to-grip = {$card-str} vom Stapel dem Griff hinzuzufügen
add-card-to-top-of-stack = {$card-str} oben auf den Stapel zu legen
add-card-to-bottom-of-stack = {$card-str} unter den Stapel zu legen

add-card-to-top-of-rd = {$title} oben auf {-rd} zu legen
add-card-to-bottom-of-rd = {$title} unter {-rd} zu legen

add-cards-from-heap-to-grip = {$titles} aus der Ablage dem Griff hinzuzufügen

move-seen-unseen-into-grip = {$seen} und {$unseen-cnt ->
    [one] 1 ungesehene Karte
    *[other] {$unseen-cnt} ungesehene Karten
} in den Griff zu verschieben
move-seen-into-grip = {$seen} in den Griff zu verschieben
move-unseen-into-grip = {$unseen-cnt ->
    [one] 1 ungesehene Karte
    *[other] {$unseen-cnt} ungesehene Karten
} in den Griff zu verschieben

move-seen-unseen-into-hq = {$seen} und {$unseen-cnt ->
    [one] 1 ungesehene Karte
    *[other] {$unseen-cnt} ungesehene Karten
} ins {-hq} zu verschieben
move-seen-into-hq = {$seen} ins {-hq} zu verschieben
move-unseen-into-hq = {$unseen-cnt ->
    [one] 1 ungesehene Karte
    *[other] {$unseen-cnt} ungesehene Karten
} ins {-hq} zu verschieben

# remove from the game (rfg)

rfg-card = {$title} aus dem Spiel zu entfernen

# reveal

expose-card = {$title} zu enthüllen
reveal-n-cards-in-hq = {$count ->
    [one] 1 Karte
    *[other] {$count} Karten
} aus dem {-hq} aufzudecken
reveal-cards-in-hq = {$count ->
    [one] {$titles}
    *[other] {$count} Karten ({$titles})
} aus dem {-hq} aufzudecken
reveal-cards-in-grip = {$count ->
    [one] {$titles}
    *[other] {$count} Karten ({$titles})
} aus dem Griff aufzudecken
reveal-top-of-stack = {$title} oben vom Stapel aufzudecken

disable-corp-id = die Identität {-corp(case:"possessive")} zu deaktivieren
disable-runner-id = die Identität {-runner(case:"possessive")} zu deaktivieren

# turns

take-additional-turn = nach diesem Zug einen zusätzlichen Zug zu machen
reduce-corp-max-hand-size-bad-publicity = das Handkartenlimit {-corp(case:"possessive")} für jede {-bad-publicity} um 1 zu senken
reduce-corp-click-next-turn = {-corp(case:"dative")} im nächsten Zug {$count} {-click} weniger zur Verfügung zu stellen

# rearrange stuff

rearrange-installed-ice = beliebig viele Ice, die Systeme schützen, neu anzuordnen
rearrange-top-n-cards-rd = {$count ->
    [one] die oberste Karte
    *[other] die obersten {$count} Karten
} von {-rd} neu anzuordnen
trash-or-rearrange-top-of-stack = {$count ->
    [one] die oberste Karte
    *[other] die obersten {$count} Karten
} des Stapels anzusehen und zu zerstören oder neu anzuordnen

swap-two-ice-positions = die Positionen von {$card-str} und {$card-str2} zu vertauschen

# advancement counters

advance = {$card-str} zu entwickeln
place-n-advancement-counters = {$count ->
    [zero] keine Entwicklungsmarker
    [one] 1 Entwicklungsmarker
    *[other] {$count} Entwicklungsmarker
} auf {$card-str} zu legen

remove-advancement-counters = {$count ->
    [zero] keine Entwicklungsmarker
    [one] 1 Entwicklungsmarker
    *[other] {$count} Entwicklungsmarker
} von {$card-str} zu entfernen

place-virus-counters = {$count ->
    [one] 1 Viruszähler
    *[other] {$count} Viruszähler
} auf {$title} zu legen

purge-virus-counters = alle Viruszähler zu löschen

charge-card = {$card-str} {$count ->
    [one] 1 Mal
    *[other] {$count} Mal
} aufzuladen

place-credits-on-self = {$credits} {-credit} auf sich selbst zu legen
place-credits-on-self-for-trash-costs = {$credits} {-credit} zum Bezahlen von Zerstörungskosten darauf zu legen

look-at-top-cards-add-to-grip = {$top-count ->
    [one] die oberste Karte
    *[other] die obersten {$top-count} Karten
} des Stapels anzusehen und {$add-count} davon dem Griff hinzuzufügen

guess = {$choice} zu raten

reveal-copies-of-self = {$count ->
    [one] 1 Kopie
    *[other] {$count} Kopien
} von sich selbst aufzudecken

# forcing

force-take-bad-publicity = {-corp(case: "accusative")} zu zwingen, {$count} {-bad-publicity} zu erhalten
force-trash-installed-ice = {-corp(case: "accusative")} zu zwingen, ein {-ice} zu zerstören, das {$server} schützt
force-corp-trash-top-of-rd = {-corp(case: "accusative")} zu zwingen, {$count ->
    [one] die oberste Karte
    *[other] die obersten {$count} Karten
} von {-rd} zu zerstören
force-corp-trash-additional-top-of-rd = {-corp(case: "accusative")} zu zwingen, {$count ->
    [one] eine weitere Karte
    *[other] {$count} weitere Karten
} oben von {-rd} zu zerstören
force-corp-rez = {-corp(case: "accusative")} zu zwingen, {$title} zu laden
force-corp-trash = {-corp(case: "accusative")} zu zwingen, {$title} zu zerstören
force-add-all-hq-cards-to-top-of-rd = {-corp(case: "accusative")} zu zwingen, alle Karten im {-hq} oben auf {-rd} zu legen
force-corp-pay-credits = {-corp(case: "accusative")} zu zwingen, {$credits} {-credit} zu zahlen
force-corp-lose-credits = {-corp(case: "accusative")} zu zwingen, {$credits} {-credit} zu verlieren
force-corp-draw-cards = {-corp(case: "accusative")} zu zwingen, {$count ->
    [one] 1 Karte
    *[other] {$count} Karten
} zu ziehen
force-corp-discard-from-hq = {-corp(case: "accusative")} zu zwingen, {$count ->
    [one] 1 Karte
    *[other] {$count} Karten
} aus dem {-hq} abzulegen
force-corp-random-discard-from-hq = {-corp(case: "accusative")} zu zwingen, {$count ->
    [one] 1 zufällige Karte
    *[other] {$count} zufällige Karten
} aus dem {-hq} abzulegen

force-runner-draw-cards = {-runner(case: "accusative")} zu zwingen, {$count ->
    [one] 1 Karte
    *[other] {$count} Karten
} zu ziehen

force-runner-gain-credits = {-runner(case: "accusative")} zu zwingen, {$credits} {-credit} zu erhalten

force-runner-lose-credits = {-runner(case: "accusative")} zu zwingen, {$credits} {-credit} zu verlieren

each-player-draws-cards = jeden Spieler {$count ->
    [one] 1 Karte
    *[other] {$count} Karten
} ziehen zu lassen

# all installs

# runner installs

runner-install-card = {$title} zu installieren

install-with-discount = {$title} zu installieren und dabei die Kosten um {$discount} {-credit} zu senken
install-from-grip = {$title} aus dem Griff zu installieren
install-from-grip-with-discount = {$title} aus dem Griff zu installieren und dabei die Kosten um {$discount} zu senken

install-from-stack = {$title} vom Stapel zu installieren
install-from-stack-with-discount = {$title} vom Stapel zu installieren und dabei die Kosten um {$discount} zu senken
install-program-from-heap = ein {-program} aus der Ablage zu installieren
install-program-from-stack = ein {-program} vom Stapel zu installieren

# hosting

host-self-as-condition-counter = sich selbst als gehosteten Bedingungsmarker auf {$card-str} zu hosten
host-card-on-card = {$title} auf {$card-str} zu hosten

# rezzing

rez-card = {$card-str} zu laden
derez-card = {$card-str} zu entladen
derez-cards = {$card-strs} zu entladen

# make a run

make-a-run = einen Run durchzuführen
make-a-run-on = einen Run auf {$server} durchzuführen
target-server = {$server} als Ziel zu wählen
target-no-server = kein System zu wählen
make-a-run-on-preventing-all-damage = einen Run auf {$server} durchzuführen und dabei allen Schaden zu verhindern
run-on-with-no-rezzed-ice = einen Run auf {$server} durchzuführen, während dem kein Ice geladen werden kann
rfg-to-make-a-run-on = {$title} aus dem Spiel zu entfernen, um einen Run auf {$server} durchzuführen

# redirect run

redirect-run-to-archives = das angegriffene System auf {-archives} zu ändern
redirect-run-to-hq = das angegriffene System auf {-hq} zu ändern
redirect-run-to-rd = das angegriffene System auf {-rd} zu ändern

# end run

end-run = den Run zu beenden

# icebreaker strength

give-strength-to-icebreaker-during-run = {$title} während des Runs +{$bonus} Stärke zu geben
give-strength-to-icebreaker-remainder-of-run = {$card-str} während des Runs +{$bonus} Stärke zu geben
give-strength-all-icebreakers-during-run = allen Ice-Brechern während des Runs +{$bonus} Stärke zu geben

# ice

ice-gains-barrier-code-gate-sentry-end-of-turn = {$card-str} bis zum Ende des Zuges Barriere, Code-Zugang und Wächter erhalten zu lassen

bypass-ice = {$card-str} zu umgehen

prevent-run-ending = das Ende des Runs zu verhindern
prevent-ice-rezzed-during-run = zu verhindern, dass während des Runs {-ice-type} geladen wird
prevent-corp-rez-card-during-turn = zu verhindern, dass {-corp(case:"nominative")} {$card-str} für den Rest des Zuges lädt
prevent-corp-rez-non-ice-on-runner-turn = zu verhindern, dass {-corp(case:"nominative")} während des Zuges {-runner(case:"possessive")} Karten lädt, die kein Ice sind

increase-rez-cost-first-unrezzed-approached-ice = die Ladekosten des ersten ungeladenen {-ice}, dem sich genähert wird, um {$credits} {-credit} zu erhöhen

# prevention

prevent-core-damage = {$count} Substanz-Schaden zu erleiden
prevent-meat-damage = {$count} Körper-Schaden zu verhindern
prevent-net-damage = {$count} Netz-Schaden zu erleiden

prevent-corp-advancing-cards-next-turn = zu verhindern, dass {-corp(case: "nominative")} im nächsten Zug Karten entwickelt

# Damage

do-meat-damage = {$value} Körper-Schaden zu verursachen
do-net-damage = {$value} Netz-Schaden zu verursachen
do-brain-damage = {$value} Substanz-Schaden zu verursachen
do-core-damage = {$value} Substanz-Schaden zu verursachen

suffer-meat-damage = {$value} Körper-Schaden zu erleiden
suffer-net-damage = {$value} Netz-Schaden zu erleiden
suffer-brain-damage = {$value} Substanz-Schaden zu erleiden
suffer-core-damage = {$value} Substanz-Schaden zu erleiden

prevent-damage-until-next-turn = bis zum nächsten eigenen Zug allen Schaden zu verhindern

# Access

access-another-card = auf eine weitere Karte zuzugreifen
access-card = auf {$card-str} zuzugreifen
access-additional-in-hq = auf {$count ->
    [one] 1 zusätzliche Karte
    *[other] {$count} zusätzliche Karten
} im {-hq} zuzugreifen
access-additional-in-rd = auf {$count ->
    [one] 1 zusätzliche Karte
    *[other] {$count} zusätzliche Karten
} in {-rd} zuzugreifen
access-from-bottom-of-rd = auf Karten von unten aus {-rd} zuzugreifen

# searching

search-stack-for-connection-resource = den Stapel nach einer Kontakt-Ressource zu durchsuchen
search-stack-for-run-event = den Stapel nach einem Run-Ereignis zu durchsuchen
search-stack-for-virtual-resource = den Stapel nach einer virtuellen Ressource zu durchsuchen

## Specific card abilities

# Apocalypse
trash-all-installed-corp = alle installierten { -corp(case: "genitive") }-Karten zu zerstören

# Apocalypse
turn-all-installed-runner-facedown = alle installierten { -runner(case: "genitive") }-Karten verdeckt umzudrehen

# Rebirth

change-identity = die Identität zu {$title} zu wechseln

## Payments

payment-click = gibt {$value ->
    [one] {-click}
    *[other] {$value} {-click}
} aus
payment-credit = zahlt {$value} {-credit}
payment-x-credit = zahlt {$value} {-credit}
payment-credit-pool = zahlt {$value} {-credit} aus dem {-credit-pool}
payment-hosted-credit = zahlt {$value} {-credit} von {$title}
payment-bad-publicity = zahlt {$value} {-credit} aus {-bad-publicity}

payment-extend = zerstört {$title} aus dem {-hq}
payment-trash-can = [trash]
payment-trash-self = zerstört {$title}
payment-forfeit = gibt {$count ->
    [zero] keine Agenden
    [one] 1 Agenda ({$titles})
    *[other] {$count} Agenden ({$titles})
} auf
payment-forfeit-self = gibt {$title} auf

payment-gain-tags = erhält {$count ->
    [zero] keine Markierungen
    [one] 1 Markierung
    *[other] {$count} Markierungen
}
payment-tag = entfernt {$count ->
    [zero] keine Markierungen
    [one] 1 Markierung
    *[other] {$count} Markierungen
}
payment-gain-bad-publicity = erhält {$count} negative Publicity
payment-return-to-grip = nimmt {$title} zurück auf die Hand
payment-return-to-hq = nimmt {$title} zurück ins {-hq}

payment-remove-from-game = entfernt {$title} aus dem Spiel
payment-rfg-program = entfernt {$count ->
    [zero] keine installierten Programme
    [one] 1 installiertes Programm ({$titles})
    *[other] {$count} installierte Programme ({$titles})
} aus dem Spiel
payment-trash-installed = zerstört {$count ->
    [zero] keine installierten Karten
    [one] 1 installierte Karte ({$titles})
    *[other] {$count} installierte Karten ({$titles})
}
payment-trash-hardware = zerstört {$count ->
    [zero] keine installierte Hardware
    [one] 1 installierte Hardware ({$titles})
    *[other] {$count} installierte Hardware ({$titles})
}
payment-trash-program = zerstört {$count ->
    [zero] keine installierten Programme
    [one] 1 installiertes Programm ({$titles})
    *[other] {$count} installierte Programme ({$titles})
}
payment-trash-resource = zerstört {$count ->
    [zero] keine installierten Ressourcen
    [one] 1 installierte Ressource ({$titles})
    *[other] {$count} installierte Ressourcen ({$titles})
}
payment-trash-connection = zerstört {$count ->
    [zero] keine installierten Kontakt-Ressourcen
    [one] 1 installierte Kontakt-Ressource ({$titles})
    *[other] {$count} installierte Kontakt-Ressourcen ({$titles})
}
payment-trash-ice = zerstört {$count ->
    [zero] kein installiertes geladenes {-ice-type}
    [one] 1 installiertes geladenes {-ice-type} ({$titles})
    *[other] {$count} installierte geladene {-ice-type} ({$titles})
}
payment-trash-bioroid = zerstört {$count ->
    [zero] keine installierten geladenen Bioroiden
    [one] 1 installierten geladenen Bioroiden ({$titles})
    *[other] {$count} installierte geladene Bioroiden ({$titles})
}

payment-trash-from-stack = zerstört {$count ->
    [zero] keine Karten
    [one] 1 Karte
    *[other] {$count} Karten
} oben vom Stapel
payment-trash-from-rd = zerstört {$count ->
    [zero] keine Karten
    [one] 1 Karte
    *[other] {$count} Karten
} oben von {-rd}
payment-trash-from-grip = zerstört {$count ->
    [zero] keine Karten
    [one] 1 Karte ({$titles})
    *[other] {$count} Karten ({$titles})
} aus dem Griff
payment-trash-from-hq = zerstört {$count ->
    [zero] keine Karten
    [one] 1 Karte
    *[other] {$count} Karten
} aus dem {-hq}
payment-reveal-trash-from-grip = deckt {$count ->
    [zero] keine Karten
    [one] 1 Karte ({$titles})
    *[other] {$count} Karten ({$titles})
} aus dem Griff auf und zerstört sie
payment-reveal-trash-from-hq = deckt {$count ->
    [zero] keine Karten
    [one] 1 Karte ({$titles})
    *[other] {$count} Karten ({$titles})
} aus dem {-hq} auf und zerstört sie
payment-randomly-trash-from-grip = zerstört zufällig {$count ->
    [zero] keine Karten
    [one] 1 Karte ({$titles})
    *[other] {$count} Karten ({$titles})
} aus dem Griff
payment-randomly-trash-from-hq = zerstört zufällig {$count ->
    [zero] keine Karten
    [one] 1 Karte
    *[other] {$count} Karten
} aus dem {-hq}
payment-randomly-reveal-trash-from-grip = deckt {$count ->
    [zero] keine Karten
    [one] 1 zufällige Karte ({$titles})
    *[other] {$count} zufällige Karten ({$titles})
} aus dem Griff auf und zerstört sie
payment-randomly-reveal-trash-from-hq = deckt {$count ->
    [zero] keine Karten
    [one] 1 zufällige Karte ({$titles})
    *[other] {$count} zufällige Karten ({$titles})
} aus dem {-hq} auf und zerstört sie
payment-trash-all-cards-in-hq = zerstört alle Karten ({$count} insgesamt) im {-hq}
payment-trash-all-cards-in-grip = zerstört alle Karten ({$count} insgesamt) im Griff ({$titles})

payment-trash-hardware-in-grip = zerstört {$count ->
    [one] 1 Hardware
    *[other] {$count} Hardware
} aus dem Griff ({$titles})
payment-trash-program-in-grip = zerstört {$count ->
    [one] 1 Programm
    *[other] {$count} Programme
} aus dem Griff ({$titles})
payment-trash-resource-in-grip = zerstört {$count ->
    [one] 1 Ressource
    *[other] {$count} Ressourcen
} aus dem Griff ({$titles})

payment-meat = erleidet {$value} Körper-Schaden
payment-net = erleidet {$value} Netz-Schaden
payment-core = erleidet {$value} Substanz-Schaden

payment-shuffle-installed-into-stack = mischt {$count ->
    [one] 1 Karte ({$titles})
    *[other] {$count} Karten ({$titles})
} in den Stapel
payment-shuffle-installed-into-rd = mischt {$count ->
    [one] 1 Karte
    *[other] {$count} Karten
} in {-rd}
payment-add-installed-bottom-stack = legt {$count ->
    [one] 1 installierte Karte ({$titles})
    *[other] {$count} installierte Karten ({$titles})
} unter den Stapel
payment-add-installed-bottom-rd = legt {$count ->
    [one] 1 installierte Karte ({$titles})
    *[other] {$count} installierte Karten ({$titles})
} unter {-rd}

payment-turn-hosted-matryoshka-facedown = dreht {$count ->
    [one] 1 gehostete Kopie
    *[other] {$count} gehostete Kopien
} von Matryoshka verdeckt um

payment-add-random-from-hand-to-bottom-of-stack = legt {$count ->
    [one] 1 zufällige Karte
    *[other] {$count} zufällige Karten
} unter den Stapel
payment-add-random-from-hand-to-bottom-of-rd = legt {$count ->
    [one] 1 zufällige Karte
    *[other] {$count} zufällige Karten
} unter {-rd}

payment-hosted-to-hq = fügt {$count ->
    [one] 1 gehostete Karte
    *[other] {$count} gehostete Karten
} dem {-hq} hinzu ({$titles})

payment-advancement-counter = gibt {$count ->
    [one] 1 gehosteten Entwicklungsmarker
    *[other] {$count} gehostete Entwicklungsmarker
} von {$title} aus
payment-agenda-counter = gibt {$count ->
    [one] 1 gehosteten Agendazähler
    *[other] {$count} gehostete Agendazähler
} von {$title} aus
payment-power-counter = gibt {$count ->
    [one] 1 gehosteten Wirkungszähler
    *[other] {$count} gehostete Wirkungszähler
} von {$title} aus
payment-virus-counter = gibt {$count ->
    [one] 1 gehosteten Viruszähler
    *[other] {$count} gehostete Viruszähler
} von {$title} aus

payment-derez-harmonic = entlädt {$count} Harmonie-{-ice-type} ({$titles})

## game.core.to-string/card-str

# it's one thing to say "uses Corroder", it's another to say "this specific Ice
# Wall on the table".

# "Corroder"
card-str-runner-seen = {$title}

# "installed Corroder"
card-str-runner-installed-seen = ein installiertes {$title}

# "a card in the grip"
card-str-runner-grip = eine Karte im Griff

# "Corroder in the heap"
card-str-runner-discard = {$title} in der Ablage

# "a facedown card"
card-str-runner-unknown = eine verdeckte Karte

# "hosted Corroder"
card-str-runner-hosted-seen = gehostetes {$title}

# "hosted facedown card"
card-str-runner-hosted-unknown = gehostete verdeckte Karte

# "Merger in the Runner's score area"
card-str-runner-scored = {$title} im Punktebereich {-runner(case: "possessive")}

# "Merger in the Corp's score area"
card-str-corp-scored = {$title} im Punktebereich {-corp(case: "possessive")}
card-str-corp-rfg = {$title} (aus dem Spiel entfernt)
card-str-corp-play-area = {$title} im Spielbereich {-corp(case: "possessive")}
card-str-corp-destroyed = zerstörtes {$title}

# "hosted Ice Wall"
card-str-corp-hosted-seen = gehostetes {$title}
card-str-corp-hosted-known = gehostetes verdecktes {$title}
card-str-corp-hosted-unknown = gehostete Karte{$server ->
    [archives] {" "}im Stamm von {-archives}
    [hq] {" "}im Stamm von {-hq}
    [rd] {" "}im Stamm von {-rd}
    *[other] {" "}in System {$server-n}
}

# "NGO Front in Server 5"
card-str-corp-installed-remote-seen = {$title} in System {$server-n}

# "facedown Obokata Protocol in Server 2"
card-str-corp-installed-remote-known = verdecktes {$title} in System {$server-n}

# "a card in Server 1"
card-str-corp-installed-remote-unknown = eine Karte in System {$server-n}

# "Prisec in root of HQ"
card-str-corp-installed-central-seen = {$server ->
    [archives] {$title} im Stamm von {-archives}
    [hq] {$title} im Stamm von {-hq}
    [rd] {$title} im Stamm von {-rd}
    *[other] unbekanntes zentrales System ({$server})
}

# "facedown Ryon Knight in root of R&D"
card-str-corp-installed-central-known = verdecktes {$title} im Stamm von {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] unbekanntes zentrales System ({$server})
}

# "a card in root of Archives"
card-str-corp-installed-central-unknown = eine Karte im Stamm von {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] unbekanntes zentrales System ({$server})
}

# "Ice Wall protecting HQ at position 1"
card-str-corp-installed-ice-seen = {$title}, das {$server ->
    [archives] {-archives} an Position {$position}
    [hq] {-hq} an Position {$position}
    [rd] {-rd} an Position {$position}
    *[other] System {$server-n} an Position {$position}
} schützt

# "facedown ICE protecting HQ at position 1"
card-str-corp-installed-ice-known = verdecktes {$title}, das {$server ->
    [archives] {-archives} an Position {$position}
    [hq] {-hq} an Position {$position}
    [rd] {-rd} an Position {$position}
    *[other] System {$server-n} an Position {$position}
} schützt

# "ICE protecting HQ at position 1"
card-str-corp-installed-ice-unknown = {-ice-type}, das {$server ->
    [archives] {-archives} an Position {$position}
    [hq] {-hq} an Position {$position}
    [rd] {-rd} an Position {$position}
    *[other] System {$server-n} an Position {$position}
} schützt

### Standalone Messages

## Game End

msg_tie = Das Spiel endet unentschieden.
msg_win = {$username} gewinnt das Spiel.
msg_concede = {$username} gibt auf.
msg_win-decked = {$username} hat keine Karten mehr im Deck.
msg_win-flatline = {$username} ist flatlined.
msg_clear-win = {$username} hat die Siegbedingung entfernt.

## Mulligan

msg_mulligan-take = {$username} nimmt einen Mulligan.
msg_mulligan-keep = {$username} behält die Starthand.

## Moving

msg_draw-cards = {$username} zieht {$count ->
    [zero] keine Karten.
    [one] 1 Karte.
    *[other] {$count} Karten.
}

msg_forfeit-agenda = {$username} gibt {$title} auf.

msg_trash-card = {$username} zerstört {$card-str}.

msg_trash-cards = {$username} zerstört {$count ->
    [zero] keine Karten.
    [one] 1 Karte ({$titles}).
    *[other] {$count} Karten ({$titles}).
}

msg_derez-card = {$username} entlädt {$card-str}.
msg_derez-cards = {$username} entlädt {$card-strs}.

msg_rfg-n-cards-from-stack = {$username} entfernt {$count ->
    [one] die oberste Karte des Stapels aus dem Spiel.
    *[other] die obersten {$count} Karten des Stapels aus dem Spiel.
}

## Runs

# TODO payment may be empty, not sure what conditional is correct to check that. needed for make/makes wording choice
msg_make-a-run-on = {$username} {$payment}, um einen Run auf {$server} durchzuführen.
msg_make-a-run-on-ignoring-costs = {$username} {$payment}, um einen Run auf {$server} durchzuführen, und ignoriert dabei alle Kosten.
msg_bypass-ice = {$username} umgeht {$title}.
msg_approach-ice = {$username} nähert sich {$card-str}.
msg_encounter-ice = {$username} trifft auf {$card-str}.
msg_pass-ice = {$username} passiert {$card-str}.
msg_continue = {$username} setzt den Run fort.
msg_jack-out = {$username} stöpselt sich aus.
msg_approach-server = {$username} nähert sich {$server}.
msg_breach-server = {$username} dringt in {$server} ein.

## Accesses

msg_access-card = {$username} greift auf {$title} aus {$server} zu.
msg_access-card-unseen = {$username} greift auf eine ungesehene Karte aus {$server} zu.
msg_pay-access-card = {$username} {$payment}, um auf {$title} aus {$server} zuzugreifen.
msg_pay-access-card-unseen = {$username} {$payment}, um auf eine ungesehene Karte aus {$server} zuzugreifen.
msg_access-archives = {$username} greift auf alles Übrige im {-archives} zu.
msg_steal-agenda = {$username} stiehlt {$title} und erhält {$value ->
    [one] 1 Agendapunkt.
    *[other] {$value} Agendapunkte.
}

## Scoring

msg_advance = {$username} {$payment}, um {$card-str} zu entwickeln.
msg_score-agenda = {$username} erzielt die Punkte für {$title} und erhält {$value ->
    [one] 1 Agendapunkt.
    *[other] {$value} Agendapunkte.
}
msg_pay-score-agenda = {$username} {$payment}, um die Punkte für {$title} zu erzielen.

## Damage

msg_trash-damage = {$username} zerstört {$titles} durch {$damage}.

## Traces

msg_increase-trace-strength = {$username} {$payment}, um die Aufspürstärke auf {$value} zu erhöhen.

## Turn messages

msg_corp-start-of-turn = {$username} beginnt Zug {$turn} mit {$credits} {-credit} und {$cards ->
    [one] 1 Karte im { -hq }.
    *[other] {$cards} Karten im { -hq }.
}
msg_corp-end-of-turn = {$username} beendet Zug {$turn} mit {$credits} {-credit} und {$cards ->
    [one] 1 Karte im { -hq }.
    *[other] {$cards} Karten im { -hq }.
}

msg_runner-start-of-turn = {$username} beginnt Zug {$turn} mit {$credits} {-credit} und {$cards ->
    [one] 1 Karte im Griff.
    *[other] {$cards} Karten im Griff.
}
msg_runner-end-of-turn = {$username} beendet Zug {$turn} mit {$credits} {-credit} und {$cards ->
    [one] 1 Karte im Griff.
    *[other] {$cards} Karten im Griff.
}

msg_mandatory-start-of-turn-draw = {$username} zieht die verpflichtende Karte zu Beginn des Zuges.

msg_no-further-actions = {$username} hat keine weiteren Aktionen.

msg_skip-discard-step = {$username} überspringt in diesem Zug den Abwurfschritt.

msg_corp-discard-cards-from-hand-eot = {$username} legt am Ende des Zuges {$cards ->
    [one] 1 Karte aus dem { -hq } ab.
    *[other] {$cards} aus dem { -hq } ab.
}
msg_runner-discard-cards-from-hand-eot = {$username} legt am Ende des Zuges {$cards} aus dem Griff ab.

msg_extra-turns-remaining = {$username} hat noch {$turns ->
    [one] 1 zusätzlichen Zug.
    *[other] {$turns} zusätzliche Züge.
}

## Waiting Prompts

waiting-corp-default = Warte auf eine Entscheidung {-corp(case: "possessive")}.
waiting-runner-default = Warte auf eine Entscheidung {-runner(case: "possessive")}.
waiting-trash-prevention-triggers = Warte, bis dein Gegner Auslöser zum Verhindern des Zerstörens abgehandelt hat.
waiting-pre-damage-triggers = Warte, bis dein Gegner Auslöser vor dem Schaden abgehandelt hat.
waiting-damage-triggers = Warte, bis dein Gegner Schadensauslöser abgehandelt hat.
waiting-prevent-when-encountered = Warte, bis dein Gegner eine „Wenn du auf ... triffst“-Fähigkeit verhindert.
waiting-prevent-run-ending = Warte, bis dein Gegner das Ende des Runs verhindert.
waiting-prevent-jacking-out = Warte, bis dein Gegner verhindert, dass du dich ausstöpselst.
waiting-prevent-expose = Warte, bis dein Gegner ein Enthüllen verhindert.
waiting-prevent-bad-publicity = Warte, bis dein Gegner {-bad-publicity} verhindert.
waiting-prevent-tags = Warte, bis dein Gegner Markierungen verhindert.

## Prompts

prompt_access-additional-card = Auf 1 zusätzliche Karte zugreifen?

prompt_choose-a-card = Wähle eine Karte

prompt_choose-a-card-in-archives = Wähle eine Karte im {-archives}

prompt_choose-a-card-in-grip = Wähle eine Karte im Griff

prompt_choose-a-card-in-heap = Wähle eine Karte in der Ablage

prompt_choose-a-card-in-hq = Wähle eine Karte im {-hq}

prompt_choose-a-card-in-hq-to-add-to-the-bottom-of-rd = Wähle eine Karte im {-hq}, die unter {-rd} gelegt wird

prompt_choose-a-card-in-hq-to-add-to-the-top-of-rd = Wähle eine Karte im {-hq}, die oben auf {-rd} gelegt wird

prompt_choose-a-card-in-hq-to-install = Wähle eine Karte im {-hq} zum Installieren

prompt_choose-a-card-in-hq-to-trash = Wähle eine Karte im {-hq} zum Zerstören

prompt_choose-a-card-in-this-server = Wähle eine Karte in diesem System

prompt_choose-a-card-that-can-be-advanced = Wähle eine Karte, die entwickelt werden kann

prompt_choose-a-card-that-can-be-advanced-to-place-advancement-counter-on = Wähle eine Karte, die entwickelt werden kann, um 1 Entwicklungsmarker darauf zu legen

prompt_choose-a-card-to-access = Wähle eine Karte, auf die zugegriffen wird

prompt_choose-a-card-to-access-you-must-access-all-cards = Wähle eine Karte, auf die zugegriffen wird. Du musst auf alle Karten zugreifen

prompt_choose-a-card-to-add-to-grip = Wähle eine Karte, die dem Griff hinzugefügt wird

prompt_choose-a-card-to-add-to-hq = Wähle eine Karte, die dem {-hq} hinzugefügt wird

prompt_choose-a-card-to-add-to-the-bottom-of-stack = Wähle eine Karte, die unter den Stapel gelegt wird

prompt_choose-a-card-to-add-to-the-top-of-rd = Wähle eine Karte, die oben auf {-rd} gelegt wird

prompt_choose-a-card-to-derez = Wähle eine Karte zum Entladen

prompt_choose-a-card-to-install = Wähle eine Karte zum Installieren

prompt_choose-a-card-to-install-facedown = Wähle eine Karte, die verdeckt installiert wird

prompt_choose-a-card-to-install-from-archives = Wähle eine Karte aus dem {-archives} zum Installieren

prompt_choose-a-card-to-install-from-hq = Wähle eine Karte aus dem {-hq} zum Installieren

prompt_choose-a-card-to-move-next-onto-rd = Wähle die nächste Karte, die auf {-rd} gelegt wird

prompt_choose-a-card-to-trash = Wähle eine Karte zum Zerstören

prompt_choose-a-card-type = Wähle eine Kartenart

prompt_choose-a-faceup-card = Wähle eine offene Karte

prompt_choose-a-hosted-card = Wähle eine gehostete Karte

prompt_choose-a-location-to-install = Wähle einen Ort, um {$title} zu installieren

prompt_choose-a-piece-of-hardware = Wähle eine Hardware

prompt_choose-a-piece-of-hardware-to-trash = Wähle eine Hardware zum Zerstören

prompt_choose-a-piece-of-ice = Wähle ein Ice

prompt_choose-a-piece-of-ice-protecting-this-server = Wähle ein Ice, das dieses System schützt

prompt_choose-a-piece-of-ice-to-install = Wähle ein Ice zum Installieren

prompt_choose-a-piece-of-ice-to-install-from-hq = Wähle ein Ice aus dem {-hq} zum Installieren

prompt_choose-a-piece-of-ice-to-place-advancement-counter-on = Wähle ein Ice, um 1 Entwicklungsmarker darauf zu legen

prompt_choose-a-piece-of-ice-to-trash = Wähle ein Ice zum Zerstören

prompt_choose-a-program-or-piece-of-hardware-to-install = Wähle ein Programm oder eine Hardware zum Installieren

prompt_choose-a-program-to-host = Wähle ein Programm zum Hosten

prompt_choose-a-program-to-install = Wähle ein Programm zum Installieren

prompt_choose-a-program-to-trash = Wähle ein Programm zum Zerstören

prompt_choose-a-remote-server = Wähle ein ausgelagertes System

prompt_choose-a-remote-server-to-run = Wähle ein ausgelagertes System für einen Run

prompt_choose-a-resource-to-trash = Wähle eine Ressource zum Zerstören

prompt_choose-a-server = Wähle ein System

prompt_choose-a-server-to-install-on = Wähle ein System, in dem {$title} installiert wird

prompt_choose-a-subroutine-to-resolve = Wähle eine Subroutine zum Abhandeln

prompt_choose-a-trigger-to-resolve = Wähle einen Auslöser zum Abhandeln

prompt_choose-an-ability-to-resolve = Wähle eine Fähigkeit zum Abhandeln

prompt_choose-an-ability-to-resolve-remaining = Wähle eine Fähigkeit zum Abhandeln (noch {$count})

prompt_choose-an-agenda = Wähle eine Agenda

prompt_choose-an-agenda-to-add-to-the-bottom-of-rd = Wähle eine Agenda, die unter {-rd} gelegt wird

prompt_choose-an-agenda-to-forfeit = Wähle eine Agenda zum Aufgeben

prompt_choose-an-event-to-play = Wähle ein Ereignis zum Spielen

prompt_choose-an-ice-subtype = Wähle eine Ice-Unterart

prompt_choose-an-icebreaker = Wähle einen Ice-Brecher

prompt_choose-an-iced-server = Wähle ein System mit Ice

prompt_choose-an-installed-card = Wähle eine installierte Karte

prompt_choose-an-installed-card-to-place-advancement-counters-on-remaining = Wähle eine installierte Karte, um Entwicklungsmarker darauf zu legen (noch {$count})

prompt_choose-an-installed-card-to-trash = Wähle eine installierte Karte zum Zerstören

prompt_choose-another-server-and-redirect-the-run = Wähle ein anderes System und leite den Run an dessen äußerste Position um

prompt_choose-any-number-of-cards-in-hq-to-trash = Wähle beliebig viele Karten im {-hq} zum Zerstören

prompt_choose-any-number-of-rezzed-cards-to-trash = Wähle beliebig viele geladene Karten zum Zerstören

prompt_choose-installed-card-to-trash = Wähle {$count ->
    [one] {$count} installierte Karte
    *[other] {$count} installierte Karten
} zum Zerstören

prompt_choose-installed-runner-cards = {$count ->
    [one] Wähle eine installierte {-runner(case: "genitive")}-Karte
    *[other] Wähle {$count} installierte {-runner(case: "genitive")}-Karten
}

prompt_choose-one = Wähle eine Option

prompt_choose-one-choice-of = Wähle eine Option. Auswahl {$current} von {$total}

prompt_choose-one-remaining = Wähle eine Option (noch {$count})

prompt_choose-pieces-of-ice-to-swap = Wähle 2 Ice zum Vertauschen

prompt_choose-target = Wähle ein Ziel für {$title}

prompt_choose-targets-at-least = {$count ->
    [one] Wähle mindestens {$count} Ziel für {$title}
    *[other] Wähle mindestens {$count} Ziele für {$title}
}

prompt_choose-targets-between = Wähle mindestens {$min} und bis zu {$max} Ziele für {$title}

prompt_choose-targets-exactly = {$count ->
    [one] Wähle {$count} Ziel für {$title}
    *[other] Wähle {$count} Ziele für {$title}
}

prompt_choose-targets-up-to = {$count ->
    [one] Wähle bis zu {$count} Ziel für {$title}
    *[other] Wähle bis zu {$count} Ziele für {$title}
}

prompt_choose-up-to-cards-in-hq-to-trash = Wähle bis zu {$count} {$count ->
    [one] Karte
    *[other] Karten
} im {-hq} zum Zerstören

prompt_do-core-damage = {$count} Substanz-Schaden verursachen?

prompt_do-meat-damage = {$count} Körper-Schaden verursachen?

prompt_draw-cards = {$count ->
    [one] {$count} Karte ziehen?
    *[other] {$count} Karten ziehen?
}

prompt_gain-click = [Click] erhalten?

prompt_gain-credits = {$credits} [Credits] erhalten?

prompt_give-the-runner-tag = {-runner(case: "dative")} 1 Markierung verpassen?

prompt_how-many-advancement-counters-do-you-want-to-move = Wie viele Entwicklungsmarker möchtest du verschieben?

prompt_how-many-advancement-counters-do-you-want-to-place = Wie viele Entwicklungsmarker möchtest du legen?

prompt_how-many-cards-do-you-want-to-draw = Wie viele Karten möchtest du ziehen?

prompt_how-many-copies-do-you-want-to-reveal = Wie viele Kopien von {$title} möchtest du aufdecken?

prompt_how-many-credits-do-you-want-to-move = Wie viele Credits möchtest du verschieben?

prompt_how-many-credits-do-you-want-to-pay = Wie viele Credits möchtest du zahlen?

prompt_how-many-credits-do-you-want-to-spend = Wie viele Credits möchtest du ausgeben?

prompt_how-many-credits-do-you-want-to-store = Wie viele Credits möchtest du lagern?

prompt_how-many-hosted-credits-do-you-want-to-take = Wie viele gehostete Credits möchtest du nehmen?

prompt_how-many-power-counters-do-you-want-to-place = Wie viele Wirkungszähler möchtest du legen?

prompt_install = {$title} installieren?

prompt_install-a-program = Ein Programm installieren?

prompt_install-another-copy = Eine weitere Kopie von {$title} installieren?

prompt_let-the-runner-gain-credits = {-runner(case: "accusative")} 2 [Credits] erhalten lassen?

prompt_look-at-the-top-cards-of-rd = {$count ->
    [one] Die oberste Karte von {-rd} ansehen?
    *[other] Die obersten {$count} Karten von {-rd} ansehen?
}

prompt_make-another-run-on-rd = Einen weiteren Run auf {-rd} durchführen?

prompt_move-to-the-bottom-of-rd = {$title} unter {-rd} legen?

prompt_move-where = Wohin soll {$title} verschoben werden?

prompt_name-a-runner-card = Nenne eine {-runner(case: "genitive")}-Karte

prompt_pay-credits-to-access-additional-card = {$credits} [Credits] zahlen, um auf 1 zusätzliche Karte zuzugreifen?

prompt_pay-credits-to-gain-click = {$credits} [Credits] zahlen, um [Click] zu erhalten?

prompt_pay-credits-to-use-ability = {$credits} [Credits] zahlen, um die Fähigkeit von {$title} zu verwenden?

prompt_place-power-counter-on = 1 Wirkungszähler auf {$title} legen?

prompt_place-virus-counter = 1 Viruszähler legen?

prompt_place-virus-counter-on = 1 Viruszähler auf {$title} legen?

prompt_purge-virus-counters = Viruszähler löschen?

prompt_remove-how-many-tags = Wie viele Markierungen entfernen?

prompt_search-for-a-card = Nach einer Karte suchen?

prompt_search-rd-for-a-piece-of-ice = {-rd} nach einem Ice durchsuchen?

prompt_search-rd-for-ice-to-install-protecting-a-remote-server = {-rd} nach einem Ice durchsuchen, das ein ausgelagertes System schützend installiert wird?

prompt_shuffle-a-copy-back-into-stack = Eine Kopie von {$title} zurück in den Stapel mischen?

prompt_spend-hosted-power-counter-to-access-additional-card = 1 gehosteten Wirkungszähler ausgeben, um auf 1 zusätzliche Karte zuzugreifen?

prompt_swap-pieces-of-ice = 2 Ice vertauschen?

prompt_swap-with-another-ice = {$card} mit einem anderen Ice vertauschen?

prompt_take-bad-publicity = {$count} {-bad-publicity} erhalten?

prompt_the-top-card-of-rd-is = Die oberste Karte von {-rd} ist {$title}

prompt_the-top-card-of-the-stack-is = Die oberste Karte des Stapels ist {$title}

prompt_top-cards-of-rd = {$count ->
    [one] Die oberste Karte von {-rd} ist {$titles}
    *[other] Die obersten Karten von {-rd} sind (oben->unten): {$titles}
}

prompt_top-cards-of-stack = {$count ->
    [one] Die oberste Karte des Stapels ist {$titles}
    *[other] Die obersten Karten des Stapels sind (oben->unten): {$titles}
}

prompt_trash = {$title} zerstören?

prompt_trash-to-force-the-corp-to-trace = {$title} zerstören, um {-corp(case: "accusative")} zum Aufspüren zu zwingen

prompt_trash-to-reduce-the-base-trace-strength = {$title} zerstören, um die Basis-Aufspürstärke auf 0 zu senken?

prompt_which-position-to-install-in = In welcher Position installieren? (0 ist die innerste)

prompt_you-have-no-ice-in-rd = Du hast kein Ice in {-rd}

## Prompt Choices

choice_add-to-score-area = {$title} dem Punktebereich hinzufügen

choice_bottom-of-rd = Unter {-rd}

choice_bottom-of-stack = Unter den Stapel

choice_cancel = Abbrechen

choice_card-from-deck = Karte aus dem Deck

choice_card-from-hand = Karte von der Hand

choice_carry-on = Weiter so!

choice_done = Fertig

choice_draw-cards = {$count ->
    [one] {$count} Karte ziehen
    *[other] {$count} Karten ziehen
}

choice_end-the-run = Beende den Run

choice_everything-else = Alles Übrige

choice_facedown-card-in-archives = Verdeckte Karte im {-archives}

choice_gain-credits = {$credits} [Credits] erhalten

choice_gain-credits-and-draw-cards = {$credits} [Credits] erhalten und {$count ->
    [one] {$count} Karte ziehen
    *[other] {$count} Karten ziehen
}

choice_i-want-to-start-over = Ich möchte von vorne beginnen

choice_install = {$title} installieren

choice_jack-out = Ausstöpseln

choice_no = Nein

choice_no-action = Keine Aktion

choice_no-server = Kein System

choice_none = Keine

choice_ok = OK

choice_pass-priority = Priorität abgeben

choice_pay-credits = {$credits} [Credits] zahlen

choice_pay-to-access = Zahlen, um zuzugreifen

choice_pay-to-steal = Zahlen, um zu stehlen

choice_pay-to-trash = {$count} [Credits] zahlen, um zu zerstören

choice_remove-tags = {$count ->
    [one] 1 Markierung entfernen
    *[other] {$count} Markierungen entfernen
}

choice_roll-d6 = Einen W6 würfeln

choice_spend-click = [Click] ausgeben

choice_start-over = Von vorne beginnen

choice_steal = Stehlen

choice_suffer-core-damage = {$count} Substanz-Schaden erleiden

choice_suffer-meat-damage = {$count} Körper-Schaden erleiden

choice_suffer-net-damage = {$count} Netz-Schaden erleiden

choice_take-tags = {$count ->
    [one] {$count} Markierung erhalten
    *[other] {$count} Markierungen erhalten
}

choice_the-corp-trashes-a-program = Der Konzern zerstört ein Programm

choice_top-of-rd = Oben auf {-rd}

choice_top-of-stack = Oben auf den Stapel

choice_trash = {$card} zerstören

choice_trash-an-installed-card = Eine installierte Karte zerstören

choice_unrezzed-upgrade = Ungeladenes Upgrade

choice_yes = Ja
