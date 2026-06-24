### templates???

## Terms aka consistent names

-corp = {$case ->
    [genitive] Corp
    [accusative] the Corp
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

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}and{" "}
join-list = ,{" "}

## Ability fragments

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

gain-credits = gain {$count} {-credit}

draw-cards = draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}

gain-clicks = gain {$count ->
    [one] [Click]
    *[other] {$count} [Click]
}

lose-clicks = lose {$count ->
    [one] [Click]
    *[other] {$count} [Click]
}

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

# runner shuffling

shuffle-grip-into-stack = shuffle { -grip } into { -stack }
shuffle-grip-and-heap-into-stack = shuffle { -grip } and { -heap } into { -stack }
shuffle-self-into-stack = shuffle itself into {-stack}
shuffle-cards-into-stack = shuffle {$count ->
    [zero] 0 cards into {-stack}
    [one] 1 card ({$titles}) into {-stack}
    *[other] {$count} cards ({$titles}) into {-stack}
}
shuffle-stack = shuffle {-stack}

# corp shuffling

shuffle-cards-in-server-into-rd = shuffle all cards in {$server} into {-rd}

# score area stuff

forfeit = forfeits {$title}
add-self-to-score-area = add itself to [their] score area as an agenda worth {$value ->
    [one] 1 agenda point
    *[other] {$value} agenda points
}

give-bad-publicity = give {-corp(case: "accusative")} {$count} { -bad-publicity }


add-card-to-grip = add {$title} to [their] {-grip}

add-card-from-stack-to-grip = add {$card-str} from {-stack} to [their] {-grip}
add-card-to-top-of-stack = add {$card-str} to the top of {-stack}
add-card-to-bottom-of-stack = add {$card-str} to the bottom of {-stack}

add-card-to-top-of-rd = add {$title} to the top of {-rd}
add-card-to-bottom-of-rd = add {$title} to the bottom of {-rd}

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

expose-card = expose {$title}
reveal-n-cards-in-hq = reveal {$count} cards from {-hq}
reveal-cards-in-hq = reveal {$count} cards ({$titles}) from {-hq}

disable-corp-id = disable {-corp(case:"nominative")} identity
disable-runner-id = disable {-runner(case:"nominative")} identity

do-nothing = do nothing

take-additional-turn = take an additional turn after this one

rearrange-installed-ice = rearrange any number of ice protecting all servers

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

place-credits-on-self-for-trash-costs = place {$count} [Credits] for paying trash costs

look-at-top-cards-add-to-grip = look at the top {$top-count ->
    [one] card of the stack and add {$add-count} of them to the grip
    *[other] {$top-count} cards of the stack and add {$add-count} of them to the grip
}

guess = guess {$choice}

reveal-copies-of-self = reveal {$count ->
    [one] 1 copy of itself
    *[other] {$count} copies of itself
}

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
force-add-all-hq-cards-to-top-of-rd = force {-corp(case:"accusative")} to add all cards in {-hq} to the top of {-rd}
force-trash-installed-ice = force {-corp(case: "accusative")} to trash a {-ice} protecting {$server}
force-corp-lose-credits = force the Corp to lose {$credits} {-credit}

each-player-draws-cards = make each player draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}

# all installs

# runner installs

runner-install-card = install {$title}

install-from-grip = install {$title} from {-grip}
install-from-grip-with-discount = install {$title} from {-grip}, lowering the cost by {$discount}

install-from-stack = install {$title} from {-stack}
install-from-stack-with-discount = install {$title} from {-stack}, lowering the cost by {$discount}

rez-card = rez {$card-str}
derez-card = derez {$card-str}
derez-cards = derez {$card-strs}

# Runs

make-a-run = make a run
make-a-run-on = make a run on {$server}
run-on-with-no-rezzed-ice = make a run on {$server} during which no ice can be rezzed
give-strength-to-icebreaker-during-run = give +{$bonus} strength to {$title} during the run
give-strength-to-icebreaker-remainder-of-run = give +{$bonus} strength to {$card-str} during the run
give-strength-all-icebreakers-during-run = give +{$bonus} strength to all icebreakers during the run

bypass-ice = bypass {$title}

prevent-ice-rezzed-during-run = prevent {-ice-type} from being rezzed during the run
prevent-corp-rez-card-during-turn = prevent {-corp(case:"nominative")} from rezzing {$card-str} for the rest of the turn

increase-rez-cost-first-unrezzed-approached-ice = increase the rez cost of the first unrezzed {-ice} approached by {$credits} {-credit}

suffer-meat-damage = suffer {$value} meat damage
suffer-net-damage = suffer {$value} net damage
suffer-brain-damage = suffer {$value} core damage
suffer-core-damage = suffer {$value} core damage

# Access

access-another-card = access another card
access-additional-in-hq = access {$count ->
    [one] 1 additional card in {-hq}
    *[other] {$count} additional cards in {-hq}
}
access-additional-in-rd = access {$count ->
    [one] 1 additional card in {-rd}
    *[other] {$count} additional cards in {-rd}
}

## Specific card abilities

# Apocalypse
trash-all-installed-corp = trash all installed { -corp(case: "genitive") } cards

# Apocalypse
turn-all-installed-runner-facedown = turn all installed { -runner(case: "genitive") } cards facedown

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
payment-random-trash-from-grip = randomly trashes {$count ->
    [zero] no cards from {-grip}
    [one] 1 card ({$titles}) from {-grip}
    *[other] {$count} cards ({$titles}) from {-grip}
}
payment-random-trash-from-hq = randomly trashes {$count ->
    [zero] no cards from {-hq}
    [one] 1 card from {-hq}
    *[other] {$count} cards from {-hq}
}
payment-random-reveal-trash-from-grip = reveals and trashes {$count ->
    [zero] no cards from {-grip}
    [one] 1 random card ({$titles}) from {-grip}
    *[other] {$count} random cards ({$titles}) from {-grip}
}
payment-random-reveal-trash-from-hq = reveals and trashes {$count ->
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

# "a facedown card"
card-str-runner-unknown = a facedown card

# "hosted Corroder"
card-str-runner-hosted-seen = hosted {$title}

# "hosted facedown card"
card-str-runner-hosted-unknown = hosted facedown card

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

msg-forfeit-agenda = {$username} forfeits {$title}.

msg-trash-card = {$username} trashes {$card-str}.

msg-trash-cards = {$username} trashes {$count ->
    [zero] no cards.
    [one] 1 card ({$titles}).
    *[other] {$count} cards ({$titles}).
}

msg-derez-card = {$username} derezzes {$card-str}.

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

