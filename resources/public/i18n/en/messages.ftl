### templates???

## Terms aka consistent names

-corp = {$case ->
    [genitive] Corp
    [accusative] the Corp
    [possessive] the Corp's
    *[nominative] the Corp
}

-runner = {$case ->
    [genitive] Runner
    [accusative] the Runner
    *[nominative] the Runner
}

-archives = Archives
-hq = HQ
-rd = R&D

-grip = the grip
-heap = the heap
-stack = the stack

-credit = [Credit]
-click = [Click]
-bad-publicity = bad publicity
-tag = tag

-credit-pool = credit pool

## card types

-agenda = agenda
-agenda-plural = agendas
-asset = asset
-asset-plural = assets
-event = event
-event-plural = events
-hardware = piece of hardware
-hardware-plural = pieces of hardware
-ice = piece of ice
-ice-plural = pieces of ice
-ice-type = ICE
-identity = identity
-identity-plural = identities
-operation = operation
-operation-plural = operations
-program = program
-program-plural = programs
-resource = resource
-resource-plural = resources
-upgrade = upgrade
-upgrade-plural = upgrades

## locations

server-name = {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] Server {$server}
}

## Logical templates

use-card = {$username} uses {$title} to {$do-ability}.
pay-use-card = {$username} {$payment} to use {$title} to {$do-ability}.
satisfy-card = {$username} {$payment} to satisfy {$title}.

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}and{" "}
join-list = ,{" "}

## Ability fragments

# generic

do-nothing = do nothing
play-card = play {$title}
play-card-no-additional-costs = play {$title}, ignoring additional costs
select = select {$card-str}

# trashing cards

trash-self = trash itself
trash-card = trash {$card-str}
trash-card-at-no-cost = trash {$card-str} at no cost
trash-n-cards = trash {$count ->
    [zero] no cards
    [one] 1 card
    *[other] {$count} cards
}
trash-cards = trash {$count ->
    [zero] no cards
    [one] 1 card ({$card-strs})
    *[other] {$count} cards ({$card-strs})
}
trash-accessed-card = trash the accessed card ({$title})
trash-all-cards-in-grip = trash all cards in {-grip}
trash-all-agendas-by-type = trash all {-agenda-plural}
trash-all-assets-by-type = trash all {-asset-plural}
trash-all-events-by-type = trash all {-event-plural}
trash-all-hardware-by-type = trash all {-hardware-plural}
trash-all-ice-by-type = trash all {-ice-type}
trash-all-operations-by-type = trash all {-operation-plural}
trash-all-resource-by-type = trash all {-resource-plural}
trash-all-upgrade-by-type = trash all {-upgrade-plural}

trash-all-cards-in-server-at-no-cost = trash all cards in {$server} at no cost

# credits

gain-credits = gain {$count} {-credit}
corp-gains-credits = make {-corp(case: "accusative")} gain {$count} {-credit}
runner-gains-credits = make {-runner(case: "accusative")} gain {$count} {-credit}

# drawing cards

draw-cards = draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}

# clicks

gain-clicks = gain {$count ->
    [one] [Click]
    *[other] {$count} [Click]
}

lose-clicks = lose {$count ->
    [one] [Click]
    *[other] {$count} [Click]
}

# tags

avoid-tags = avoid {$count ->
    [one] 1 tag
    *[other] {$count} tags
}
take-tags = take {$count ->
    [one] 1 tag
    *[other] {$count} tags
}
remove-tags = remove {$count ->
    [one] 1 tag
    *[other] {$count} tags
}
remove-all-tags = remove all tags ({$count})

# runner shuffling

shuffle-grip-into-stack = shuffle { -grip } into { -stack }
shuffle-grip-and-heap-into-stack = shuffle { -grip } and { -heap } into { -stack }
shuffle-self-into-stack = shuffle itself into {-stack}
shuffle-cards-into-stack = shuffle {$count ->
    [zero] 0 cards into {-stack}
    [one] {$titles} into {-stack}
    *[other] {$count} cards ({$titles}) into {-stack}
}
shuffle-stack = shuffle {-stack}

# corp shuffling

shuffle-cards-in-server-into-rd = shuffle all cards in {$server} into {-rd}
shuffle-cards-into-rd = shuffle {$count ->
    [zero] 0 cards into {-rd}
    [one] {$titles} into {-rd}
    *[other] {$count} cards ({$titles}) into {-rd}
}

# score area stuff

forfeit = forfeits {$title}
add-self-to-score-area = add itself to [their] score area as an agenda worth {$value ->
    [one] 1 agenda point
    *[other] {$value} agenda points
}

give-bad-publicity = give {-corp(case: "accusative")} {$count} { -bad-publicity }

# moving cards

add-self-to-grip = add itself to {-grip}
add-card-to-grip = add {$title} to {-grip}
add-card-to-hq = add {$card-str} to {-hq}

add-card-from-stack-to-grip = add {$card-str} from {-stack} to {-grip}
add-card-to-top-of-stack = add {$card-str} to the top of {-stack}
add-card-to-bottom-of-stack = add {$card-str} to the bottom of {-stack}

add-card-to-top-of-rd = add {$title} to the top of {-rd}
add-card-to-bottom-of-rd = add {$title} to the bottom of {-rd}

add-cards-from-heap-to-grip = add {$titles} from {-heap} to {-grip}

move-seen-unseen-into-grip = move {$seen} and {$unseen-cnt ->
    [one] 1 unseen card into {-grip}
    *[other] {$unseen-cnt} unseen cards into {-grip}
}
move-seen-into-grip = move {$seen} into {-grip}
move-unseen-into-grip = move {$unseen-cnt ->
    [one] 1 unseen card into {-grip}
    *[other] {$unseen-cnt} unseen cards into {-grip}
}

move-seen-unseen-into-hq = move {$seen} and {$unseen-cnt ->
    [one] 1 unseen card into {-hq}
    *[other] {$unseen-cnt} unseen cards into {-hq}
}
move-seen-into-hq = move {$seen} into {-hq}
move-unseen-into-hq = move {$unseen-cnt ->
    [one] 1 unseen card into {-hq}
    *[other] {$unseen-cnt} unseen cards into {-hq}
}

# remove from the game (rfg)

rfg-card = remove {$title} from the game

# reveal

expose-card = expose {$title}
reveal-n-cards-in-hq = reveal {$count ->
    [one] 1 card from {-hq}
    *[other] {$count} cards from {-hq}
}
reveal-cards-in-hq = reveal {$count ->
    [one] {$titles} from {-hq}
    *[other] {$count} cards ({$titles}) from {-hq}
}
reveal-cards-in-grip = reveal {$count ->
    [one] {$titles} from {-grip}
    *[other] {$count} cards ({$titles}) from {-grip}
}
reveal-top-of-stack = reveal {$title} from the top of {-stack}

disable-corp-id = disable {-corp(case:"nominative")} identity
disable-runner-id = disable {-runner(case:"nominative")} identity

# turns

take-additional-turn = take an additional turn after this one
reduce-corp-max-hand-size-bad-publicity = reduce {-corp(case:"possessive")} maximum hand size by 1 for each {-bad-publicity}
reduce-corp-click-next-turn = give {-corp(case:"nominative")} {$count} fewer {-click} to spend on [corp-pronoun] next turn

# rearrange stuff

rearrange-installed-ice = rearrange any number of ice protecting all servers
rearrange-top-n-cards-rd = rearrange the top {$count ->
    [one] 1 card of {-rd}
    *[other] {$count} cards of {-rd}
}
trash-or-rearrange-top-of-stack = look at and trash or rearrange the top {$count ->
    [one] 1 card of {-stack}
    *[other] {$count} cards of {-stack}
}

swap-two-ice-positions = swap the positions of {$card-str} and {$card-str2}

# advancement counters

place-n-advancement-counters = place {$count ->
    [zero] no advancement counters on {$card-str}
    [one] 1 advancement counter on {$card-str}
    *[other] {$count} advancement counters on {$card-str}
}

remove-advancement-counters = remove {$count ->
    [zero] no advancement counters from {$card-str}
    [one] 1 advancement counter from {$card-str}
    *[other] {$count} advancement counters from {$card-str}
}

place-virus-counters = place {$count ->
    [one] 1 virus counter on {$title}
    *[other] {$count} virus counters on {$title}
}

charge-card = trash {$card-str} {$count ->
    [one] 1 time
    *[other] {$count} times
}

place-credits-on-self = place {$credits} {-credit} on itself
place-credits-on-self-for-trash-costs = place {$credits} {-credit} for paying trash costs

look-at-top-cards-add-to-grip = look at the top {$top-count ->
    [one] card of the stack and add {$add-count} of them to the grip
    *[other] {$top-count} cards of the stack and add {$add-count} of them to the grip
}

guess = guess {$choice}

reveal-copies-of-self = reveal {$count ->
    [one] 1 copy of itself
    *[other] {$count} copies of itself
}

# forcing

force-take-bad-publicity = force {-corp(case: "nominative")} to take {$count} {-bad-publicity}
force-trash-installed-ice = force {-corp(case: "nominative")} to trash a {-ice} protecting {$server}
force-corp-trash-top-of-rd = force the Corp to trash the top {$count ->
    [one] card of R&D
    *[other] {$count} cards of R&D
}
force-corp-trash-additional-top-of-rd = force the Corp to trash an additional {$count ->
    [one] card from the top of R&D
    *[other] {$count} cards from the top of R&D
}
force-corp-rez = force the Corp to rez {$title}
force-corp-trash = force the Corp to trash {$title}
force-add-all-hq-cards-to-top-of-rd = force {-corp(case: "nominative")} to add all cards in {-hq} to the top of {-rd}
force-corp-pay-credits = force {-corp(case: "nominative")} to pay {$credits} {-credit}
force-corp-lose-credits = force {-corp(case: "nominative")} to lose {$credits} {-credit}
force-corp-draw-cards = force {-corp(case: "nominative")} to draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}
force-corp-discard-from-hq = force {-corp(case: "nominative")} to discard {$count ->
    [one] 1 card from {-hq}
    *[other] {$count} cards from {-hq}
}
force-corp-random-discard-from-hq = force {-corp(case: "nominative")} to discard {$count ->
    [one] 1 card from {-hq} at random
    *[other] {$count} cards from {-hq} at random
}

force-runner-draw-cards = force {-runner(case: "nominative")} to draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}

force-runner-gain-credits = force {-runner(case: "nominative")} to gain {$credits} {-credit}

each-player-draws-cards = make each player draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}

# all installs

# runner installs

runner-install-card = install {$title}

install-with-discount = install {$title}, lowering the cost by {$discount} {-credit}
install-from-grip = install {$title} from {-grip}
install-from-grip-with-discount = install {$title} from {-grip}, lowering the cost by {$discount}

install-from-stack = install {$title} from {-stack}
install-from-stack-with-discount = install {$title} from {-stack}, lowering the cost by {$discount}
install-program-from-heap = install a {-program} from {-heap}
install-program-from-stack = install a {-program} from {-stack}

# hosting

host-self-as-condition-counter = host itself on {$card-str} as a hosted condition counter
host-card-on-card = host {$title} on {$card-str}

# rezzing

rez-card = rez {$card-str}
derez-card = derez {$card-str}
derez-cards = derez {$card-strs}

# make a run

make-a-run = make a run
make-a-run-on = make a run on {$server}
make-a-run-on-preventing-all-damage = make a run on {$server}, preventing all damage
run-on-with-no-rezzed-ice = make a run on {$server} during which no ice can be rezzed
rfg-to-make-a-run-on = remove {$title} from the game to make a run on {$server}

# redirect run

redirect-run-to-archives = change the attacked server to {-archives}
redirect-run-to-hq = change the attacked server to {-hq}
redirect-run-to-rd = change the attacked server to {-rd}

# icebreaker strength

give-strength-to-icebreaker-during-run = give +{$bonus} strength to {$title} during the run
give-strength-to-icebreaker-remainder-of-run = give +{$bonus} strength to {$card-str} during the run
give-strength-all-icebreakers-during-run = give +{$bonus} strength to all icebreakers during the run

# ice

ice-gains-barrier-code-gate-sentry-end-of-turn = {$card-str} gains barrier, code gate, and sentry until end of turn

bypass-ice = bypass {$card-str}

prevent-run-ending = prevent the run from ending
prevent-ice-rezzed-during-run = prevent {-ice-type} from being rezzed during the run
prevent-corp-rez-card-during-turn = prevent {-corp(case:"nominative")} from rezzing {$card-str} for the rest of the turn
prevent-corp-rez-non-ice-on-runner-turn = prevent {-corp(case:"nominative")} from rezzing non-ice cards during {-runner(case:"possessive")} turn

increase-rez-cost-first-unrezzed-approached-ice = increase the rez cost of the first unrezzed {-ice} approached by {$credits} {-credit}

# prevention

prevent-core-damage = suffer {$count} core damage
prevent-meat-damage = prevent {$count} meat damage
prevent-net-damage = suffer {$count} net damage

prevent-corp-advancing-cards-next-turn = prevent {-corp(case: "nominative")} from advancing cards during [their] next turn

# Damage

suffer-meat-damage = suffer {$value} meat damage
suffer-net-damage = suffer {$value} net damage
suffer-brain-damage = suffer {$value} core damage
suffer-core-damage = suffer {$value} core damage

prevent-damage-until-next-turn = prevent all damage until your next turn

# Access

access-another-card = access another card
access-card = access {$card-str}
access-additional-in-hq = access {$count ->
    [one] 1 additional card in {-hq}
    *[other] {$count} additional cards in {-hq}
}
access-additional-in-rd = access {$count ->
    [one] 1 additional card in {-rd}
    *[other] {$count} additional cards in {-rd}
}
access-from-bottom-of-rd = access cards from the bottom of {-rd}

# searching

search-stack-for-connection-resource = search {-stack} for a connection resource
search-stack-for-run-event = search {-stack} for a run event
search-stack-for-virtual-resource = search {-stack} for a virtual resource

## Specific card abilities

# Apocalypse
trash-all-installed-corp = trash all installed { -corp(case: "genitive") } cards

# Apocalypse
turn-all-installed-runner-facedown = turn all installed { -runner(case: "genitive") } cards facedown

# Rebirth

change-identity = change identity to {$title}

## Payments 

payment-click = spends {$value} {-click}
payment-credit = pays {$value} {-credit}
payment-x-credit = pays {$value} {-credit}
payment-credit-pool = pays {$value} {-credit} from [their] {-credit-pool}
payment-hosted-credit = pays {$value} {-credit} from {$title}
payment-bad-publicity = pays {$value} {-credit} from {-bad-publicity}

payment-extend = trashes {$title} from {-hq}
payment-trash-can = [trash]
payment-trash-self = trashes {$title}
payment-forfeit = forfeits {$count ->
    [zero] no agendas
    [one] 1 agenda ({$titles})
    *[other] {$count} agendas ({$titles})
}
payment-forfeit-self = forfeits {$title}

payment-gain-tags = takes {$count ->
    [zero] no tags
    [one] 1 tag
    *[other] {$count} tags
}
payment-tag = removes {$count ->
    [zero] no tags
    [one] 1 tag
    *[other] {$count} tags
}
payment-gain-bad-publicity = gains {$count} bad publicity
payment-return-to-grip = returns {$title} to {-grip}
payment-return-to-hq = returns {$title} to {-hq}

payment-remove-from-game = removes {$title} from the game
payment-rfg-program = removes {$count ->
    [zero] no installed programs from the game
    [one] 1 installed program from the game ({$titles})
    *[other] {$count} installed programs from the game ({$titles})
}
payment-trash-installed = trashes {$count ->
    [zero] no installed cards
    [one] 1 installed card ({$titles})
    *[other] {$count} installed cards ({$titles})
}
payment-trash-hardware = trashes {$count ->
    [zero] no installed pieces of hardwares
    [one] 1 installed piece of hardware ({$titles})
    *[other] {$count} installed pieces of hardware ({$titles})
}
payment-trash-program = trashes {$count ->
    [zero] no installed programs
    [one] 1 installed program ({$titles})
    *[other] {$count} installed programs ({$titles})
}
payment-trash-resource = trashes {$count ->
    [zero] no installed resources
    [one] 1 installed resource ({$titles})
    *[other] {$count} installed resources ({$titles})
}
payment-trash-connection = trashes {$count ->
    [zero] no installed connection resources
    [one] 1 installed connection resource ({$titles})
    *[other] {$count} installed connection resources ({$titles})
}
payment-trash-ice = trashes {$count ->
    [zero] no installed rezzed {-ice-type}
    [one] 1 installed rezzed {-ice-type} ({$titles})
    *[other] {$count} installed rezzed {-ice-type} ({$titles})
}
payment-trash-bioroid = trashes {$count ->
    [zero] no installed rezzed Bioroids
    [one] 1 installed rezzed Bioroid ({$titles})
    *[other] {$count} installed rezzed Bioroids ({$titles})
}

payment-trash-from-stack = trashes {$count ->
    [zero] no cards from the top of {-stack}
    [one] 1 card from the top of {-stack}
    *[other] {$count} cards from the top of {-stack}
}
payment-trash-from-rd = trashes {$count ->
    [zero] no cards from the top of {-rd}
    [one] 1 card from the top of {-rd}
    *[other] {$count} cards from the top of {-rd}
}
payment-trash-from-grip = trashes {$count ->
    [zero] no cards from {-grip}
    [one] 1 card ({$titles}) from {-grip}
    *[other] {$count} cards ({$titles}) from {-grip}
}
payment-trash-from-hq = trashes {$count ->
    [zero] no cards from {-hq}
    [one] 1 card from {-hq}
    *[other] {$count} cards from {-hq}
}
payment-reveal-trash-from-grip = reveals and trashes {$count ->
    [zero] no cards from {-grip}
    [one] 1 card ({$titles}) from {-grip}
    *[other] {$count} cards ({$titles}) from {-grip}
}
payment-reveal-trash-from-hq = reveals and trashes {$count ->
    [zero] no cards from {-hq}
    [one] 1 card ({$titles}) from {-hq}
    *[other] {$count} cards ({$titles}) from {-hq}
}
payment-randomly-trash-from-grip = randomly trashes {$count ->
    [zero] no cards from {-grip}
    [one] 1 card ({$titles}) from {-grip}
    *[other] {$count} cards ({$titles}) from {-grip}
}
payment-randomly-trash-from-hq = randomly trashes {$count ->
    [zero] no cards from {-hq}
    [one] 1 card from {-hq}
    *[other] {$count} cards from {-hq}
}
payment-randomly-reveal-trash-from-grip = reveals and trashes {$count ->
    [zero] no cards from {-grip}
    [one] 1 random card ({$titles}) from {-grip}
    *[other] {$count} random cards ({$titles}) from {-grip}
}
payment-randomly-reveal-trash-from-hq = reveals and trashes {$count ->
    [zero] no cards from {-hq}
    [one] 1 random card ({$titles}) from {-hq}
    *[other] {$count} random cards ({$titles}) from {-hq}
}
payment-trash-all-cards-in-hq = trashes all cards ({$count} total) in {-hq}
payment-trash-all-cards-in-grip = trashes all cards ({$count} total) in {-grip} ({$titles})

payment-trash-hardware-in-grip = trashes {$count ->
    [one] 1 piece of hardware from {-grip} ({$titles})
    *[other] {$count} pieces of hardware from {-grip} ({$titles})
}
payment-trash-program-in-grip = trashes {$count ->
    [one] 1 programs from {-grip} ({$titles})
    *[other] {$count} programs from {-grip} ({$titles})
}
payment-trash-resource-in-grip = trashes {$count ->
    [one] 1 resource from {-grip} ({$titles})
    *[other] {$count} resources from {-grip} ({$titles})
}

payment-meat = suffer {$value} meat damage
payment-net = suffer {$value} net damage
payment-core = suffer {$value} core damage

payment-shuffle-installed-into-stack = shuffles {$count ->
    [one] 1 card ({$titles}) into {-stack}
    *[other] {$count} cards ({$titles}) into {-stack}
}
payment-shuffle-installed-into-rd = shuffles {$count ->
    [one] 1 card into {-rd}
    *[other] {$count} cards into {-rd}
}
payment-add-installed-bottom-stack = adds {$count ->
    [one] 1 installed card ({$titles}) to the bottom of {-stack}
    *[other] {$count} installed cards ({$titles}) to the bottom of {-stack}
}
payment-add-installed-bottom-rd = adds {$count ->
    [one] 1 installed card ({$titles}) to the bottom of {-rd}
    *[other] {$count} installed cards ({$titles}) to the bottom of {-rd}
}

payment-turn-hosted-matryoshka-facedown = turns {$count ->
    [one] 1 hosted copy of Matryoshka facedown
    *[other] {$count} hosted copy of Matryoshka facedown
}

payment-add-random-from-hand-to-bottom-of-stack = adds {$count ->
    [one] 1 random card to the bottom of {-stack}
    *[other] {$count} random cards to the bottom of {-stack}
}
payment-add-random-from-hand-to-bottom-of-rd = adds {$count ->
    [one] 1 random card to the bottom of {-rd}
    *[other] {$count} random cards to the bottom of {-rd}
}

payment-hosted-to-hq = adds {$count ->
    [one] 1 hosted card to {-hq} ({$titles})
    *[other] {$count} hosted cards to {-hq} ({$titles})
}

payment-advancement-counter = spends {$count ->
    [one] 1 hosted advancement counter from {$title}
    *[one] {$count} hosted advancement counters from {$title}
}
payment-agenda-counter = spends {$count ->
    [one] 1 hosted agenda counter from {$title}
    *[one] {$count} hosted agenda counters from {$title}
}
payment-power-counter = spends {$count ->
    [one] 1 hosted power counter from {$title}
    *[one] {$count} hosted power counters from {$title}
}
payment-virus-counter = spends {$count ->
    [one] 1 hosted virus counter from {$title}
    *[one] {$count} hosted virus counters from {$title}
}

payment-derez-harmonic = derezzes {$count} Harmonic {-ice-type} ({$titles})

## game.core.to-string/card-str

# it's one thing to say "uses Corroder", it's another to say "this specific Ice
# Wall on the table".

# "Corroder"
card-str-runner-seen = {$title}

# "installed Corroder"
card-str-runner-installed-seen = an installed {$title}

# "a card in the grip"
card-str-runner-grip = a card in {-grip}

# "Corroder in the heap"
card-str-runner-discard = {$title} in {-heap}

# "a facedown card"
card-str-runner-unknown = a facedown card

# "hosted Corroder"
card-str-runner-hosted-seen = hosted {$title}

# "hosted facedown card"
card-str-runner-hosted-unknown = hosted facedown card

# "Merger in the Runner's score area"
card-str-runner-scored = {$title} in {-runner(case: "possessive")} score area

# "Merger in the Corp's score area"
card-str-corp-scored = {$title} in {-corp(case: "possessive")} score area
card-str-corp-rfg = {$title} removed from the game
card-str-corp-play-area = {$title} in {-corp(case: "possessive")} play area
card-str-corp-destroyed = destroyed {$title}

# "hosted Ice Wall"
card-str-corp-hosted-seen = hosted {$title}
card-str-corp-hosted-known = hosted facedown {$title}
card-str-corp-hosted-unknown = hosted card{$server ->
    [archives] {" "}in root of {-archives}
    [hq] {" "}in root of {-hq}
    [rd] {" "}in root of {-rd}
    *[other] {" "}in Server {$server-n}
}

# "NGO Front in Server 5"
card-str-corp-installed-remote-seen = {$title} in Server {$server-n}

# "facedown Obokata Protocol in Server 2"
card-str-corp-installed-remote-known = facedown {$title} in Server {$server-n}

# "a card in Server 1"
card-str-corp-installed-remote-unknown = a card in Server {$server-n}

# "Prisec in root of HQ"
card-str-corp-installed-central-seen = {$server ->
    [archives] {$title} in root of {-archives}
    [hq] {$title} in root of {-hq}
    [rd] {$title} in root of {-rd}
    *[other] unknown central ({$server})
}

# "facedown Ryon Knight in root of R&D"
card-str-corp-installed-central-known = facedown {$title} in root of {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] unknown central ({$server})
}

# "a card in root of Archives"
card-str-corp-installed-central-unknown = a card in root of {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] unknown central ({$server})
}

# "Ice Wall protecting HQ at position 1"
card-str-corp-installed-ice-seen = {$title} protecting {$server ->
    [archives] {-archives} at position {$position}
    [hq] {-hq} at position {$position}
    [rd] {-rd} at position {$position}
    *[other] Server {$server-n} at position {$position}
}

# "facedown ICE protecting HQ at position 1"
card-str-corp-installed-ice-known = facedown {$title} protecting {$server ->
    [archives] {-archives} at position {$position}
    [hq] {-hq} at position {$position}
    [rd] {-rd} at position {$position}
    *[other] Server {$server-n} at position {$position}
}

# "ICE protecting HQ at position 1"
card-str-corp-installed-ice-unknown = {-ice-type} protecting {$server ->
    [archives] {-archives} at position {$position}
    [hq] {-hq} at position {$position}
    [rd] {-rd} at position {$position}
    *[other] Server {$server-n} at position {$position}
}

### Standalone Messages

## Game End

tie = The game has ended in a tie.
win = {$username} wins the game.
concede = {$username} concedes.
win-decked = {$username} is decked.
win-flatline = {$username} is flatlined.
clear-win = {$username} cleared the win condition.

## Mulligan

mulligan-take = {$username} takes a mulligan.
mulligan-keep = {$username} keeps [their] hand.

## Moving

msg-draw-cards = {$username} draws {$count ->
    [zero] no cards.
    [one] 1 card.
    *[other] {$count} cards.
}

msg-forfeit-agenda = {$username} forfeits {$title}.

msg-trash-card = {$username} trashes {$card-str}.

msg-trash-cards = {$username} trashes {$count ->
    [zero] no cards.
    [one] 1 card ({$titles}).
    *[other] {$count} cards ({$titles}).
}

msg-derez-card = {$username} derezzes {$card-str}.

msg-rfg-n-cards-from-stack = {$username} removes {$count ->
    [one] the top 1 card of {-stack} from the game.
    *[other] the top {$count} cards of {-stack} from the game.
}

## Traces

increase-trace-strength = {$username} {$payment} to increase trace strength to {$value}.

## Turn messages

corp-start-of-turn = {$username} started [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}
corp-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}

runner-start-of-turn = {$username} started [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -grip }.
    *[other] {$cards} cards in { -grip }.
}
runner-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -grip }.
    *[other] {$cards} cards in { -grip }.
}

mandatory-start-of-turn-draw = {$username} makes [their] mandatory start of turn draw.

no-further-actions = {$username} has no further actions.

skip-discard-step = {$username} skips [their] discard step this turn.

corp-discard-cards-from-hand-eot = {$username} discards {$cards ->
    [one] 1 card from { -hq } at end of turn.
    *[other] {$cards} from { -hq } at end of turn.
}
runner-discard-cards-from-hand-eot = {$username} discards {$cards} from {-stack} at end of turn.

extra-turns-remaining = {$username} will have {$turns ->
    [one] 1 extra turn remaining.
    *[other] {$turns} extra turns remaning.
}

