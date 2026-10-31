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

-credit = [Credits]
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

## damage types

damage-name = {$damage ->
    [meat] meat damage
    [net] net damage
    [core] core damage
    *[other] unknown damage
}

## locations

server-name = {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] Server {$server}
}

## Logical templates

msg_play-card = {$username} plays {$title}.
msg_pay-play-card = {$username} {$payment} to play {$title}.
msg_use-card = {$username} uses {$title} to {$do-ability}.
msg_pay-use-card = {$username} {$payment} to use {$title} to {$do-ability}.
msg_satisfy-card = {$username} {$payment} to satisfy {$title}.

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
trash-n-cards-from-hq = trash {$count ->
    [zero] no cards from {-hq}
    [one] 1 card from {-hq}
    *[other] {$count} cards from {-hq}
}
trash-cards-from-hq = trash {$count ->
    [zero] no cards from {-hq}
    [one] 1 card ({$card-strs}) from {-hq}
    *[other] {$count} cards ({$card-strs}) from {-hq}
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
give-tags = give {-runner(case: "nominative")} {$count ->
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

advance = advance {$card-str}
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

purge-virus-counters = purge all virus counters

charge-card = charge {$card-str} {$count ->
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

force-runner-lose-credits = force {-runner(case: "nominative")} to lose {$credits} {-credit}

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
target-server = target {$server}
target-no-server = choose no server
make-a-run-on-preventing-all-damage = make a run on {$server}, preventing all damage
run-on-with-no-rezzed-ice = make a run on {$server} during which no ice can be rezzed
rfg-to-make-a-run-on = remove {$title} from the game to make a run on {$server}

# redirect run

redirect-run-to-archives = change the attacked server to {-archives}
redirect-run-to-hq = change the attacked server to {-hq}
redirect-run-to-rd = change the attacked server to {-rd}

# end run

end-run = end the run

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

do-meat-damage = do {$value} meat damage
do-net-damage = do {$value} net damage
do-brain-damage = do {$value} core damage
do-core-damage = do {$value} core damage

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

payment-click = spends {$value ->
    [one] {-click}
    *[other] {$value} {-click}
}
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

msg_tie = The game has ended in a tie.
msg_win = {$username} wins the game.
msg_concede = {$username} concedes.
msg_win-decked = {$username} is decked.
msg_win-flatline = {$username} is flatlined.
msg_clear-win = {$username} cleared the win condition.

## Mulligan

msg_mulligan-take = {$username} takes a mulligan.
msg_mulligan-keep = {$username} keeps [their] hand.

## Moving

msg_draw-cards = {$username} draws {$count ->
    [zero] no cards.
    [one] 1 card.
    *[other] {$count} cards.
}

msg_forfeit-agenda = {$username} forfeits {$title}.

msg_trash-card = {$username} trashes {$card-str}.

msg_trash-cards = {$username} trashes {$count ->
    [zero] no cards.
    [one] 1 card ({$titles}).
    *[other] {$count} cards ({$titles}).
}

msg_derez-card = {$username} derezzes {$card-str}.
msg_derez-cards = {$username} derezzes {$card-strs}.

msg_rfg-n-cards-from-stack = {$username} removes {$count ->
    [one] the top 1 card of {-stack} from the game.
    *[other] the top {$count} cards of {-stack} from the game.
}

## Runs

# TODO payment may be empty, not sure what conditional is correct to check that. needed for make/makes wording choice
msg_make-a-run-on = {$username} {$payment} to make a run on {$server}.
msg_make-a-run-on-ignoring-costs = {$username} {$payment} to make a run on {$server}, ignoring all costs.
msg_bypass-ice = {$username} bypasses {$title}.
msg_approach-ice = {$username} approaches {$card-str}.
msg_encounter-ice = {$username} encounters {$card-str}.
msg_pass-ice = {$username} passes {$card-str}.
msg_continue = {$username} will continue the run.
msg_jack-out = {$username} jacks out.
msg_approach-server = {$username} approaches {$server}.
msg_breach-server = {$username} breaches {$server}.

## Accesses

msg_access-card = {$username} accesses {$title} from {$server}.
msg_access-card-unseen = {$username} accesses an unseen card from {$server}.
msg_pay-access-card = {$username} {$payment} to access {$title} from {$server}.
msg_pay-access-card-unseen = {$username} {$payment} to access an unseen card from {$server}.
msg_access-archives = {$username} accesses everything else in {-archives}.
msg_steal-agenda = {$username} steals {$title} and gains {$value ->
    [one] 1 agenda point.
    *[other] {$value} agenda points.
}

## Scoring

msg_advance = {$username} {$payment} to advance {$card-str}.
msg_score-agenda = {$username} scores {$title} and gains {$value ->
    [one] 1 agenda point.
    *[other] {$value} agenda points.
}
msg_pay-score-agenda = {$username} {$payment} to score {$title}.

## Damage

msg_trash-damage = {$username} trashes {$titles} due to {$damage}.

## Traces

msg_increase-trace-strength = {$username} {$payment} to increase trace strength to {$value}.

## Turn messages

msg_corp-start-of-turn = {$username} started [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}
msg_corp-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}

msg_runner-start-of-turn = {$username} started [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -grip }.
    *[other] {$cards} cards in { -grip }.
}
msg_runner-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -grip }.
    *[other] {$cards} cards in { -grip }.
}

msg_mandatory-start-of-turn-draw = {$username} makes [their] mandatory start of turn draw.

msg_no-further-actions = {$username} has no further action.

msg_skip-discard-step = {$username} skips [their] discard step this turn.

msg_corp-discard-cards-from-hand-eot = {$username} discards {$cards ->
    [one] 1 card from { -hq } at end of turn.
    *[other] {$cards} from { -hq } at end of turn.
}
msg_runner-discard-cards-from-hand-eot = {$username} discards {$cards} from {-stack} at end of turn.

msg_extra-turns-remaining = {$username} will have {$turns ->
    [one] 1 extra turn remaining.
    *[other] {$turns} extra turns remaning.
}

## Waiting Prompts

waiting-corp-default = Waiting for {-corp(case: "nominative")} to make a decision.
waiting-runner-default = Waiting for {-runner(case: "nominative")} to make a decision.
waiting-trash-prevention-triggers = Waiting for your opponent to resolve trash prevention triggers.
waiting-pre-damage-triggers = Waiting for your opponent to resolve pre-damage triggers.
waiting-damage-triggers = Waiting for your opponent to resolve damage triggers.
waiting-prevent-when-encountered = Waiting for your opponent to prevent a "when encountered" ability.
waiting-prevent-run-ending = Waiting for your opponent to prevent the run from ending.
waiting-prevent-jacking-out = Waiting for your opponent to prevent you from jacking out.
waiting-prevent-expose = Waiting for your opponent to prevent an expose.
waiting-prevent-bad-publicity = Waiting for your opponent to prevent {-bad-publicity}.
waiting-prevent-tags = Waiting for your opponent to prevent tags.

## Prompts

prompt_access-additional-card = Access 1 additional card?

prompt_add-a-card-to-the-top-of-rd = Add a card to the top of {-rd}

prompt_choose-a-card = Choose a card

prompt_choose-a-card-in-archives = Choose a card in {-archives}

prompt_choose-a-card-in-grip = Choose a card in {-grip}

prompt_choose-a-card-in-heap = Choose a card in {-heap}

prompt_choose-a-card-in-hq = Choose a card in {-hq}

prompt_choose-a-card-in-hq-to-add-to-the-bottom-of-rd = Choose a card in {-hq} to add to the bottom of {-rd}

prompt_choose-a-card-in-hq-to-add-to-the-top-of-rd = Choose a card in {-hq} to add to the top of {-rd}

prompt_choose-a-card-in-hq-to-install = Choose a card in {-hq} to install

prompt_choose-a-card-in-hq-to-move-to-the-top-of-rd = Choose a card in {-hq} to move to the top of {-rd}

prompt_choose-a-card-in-hq-to-trash = Choose a card in {-hq} to trash

prompt_choose-a-card-in-this-server = Choose a card in this server

prompt_choose-a-card-that-can-be-advanced = Choose a card that can be advanced

prompt_choose-a-card-that-can-be-advanced-to-place-advancement-counter-on = Choose a card that can be advanced to place 1 advancement counter on

prompt_choose-a-card-to-access = Choose a card to access

prompt_choose-a-card-to-access-you-must-access-all-cards = Choose a card to access. You must access all cards

prompt_choose-a-card-to-add-to-grip = Choose a card to add to {-grip}

prompt_choose-a-card-to-add-to-hq = Choose a card to add to {-hq}

prompt_choose-a-card-to-add-to-the-bottom-of-stack = Choose a card to add to the bottom of {-stack}

prompt_choose-a-card-to-add-to-the-top-of-rd = Choose a card to add to the top of {-rd}

prompt_choose-a-card-to-derez = Choose a card to derez

prompt_choose-a-card-to-install = Choose a card to install

prompt_choose-a-card-to-install-facedown = Choose a card to install facedown

prompt_choose-a-card-to-install-from-archives = Choose a card to install from {-archives}

prompt_choose-a-card-to-install-from-hq = Choose a card to install from {-hq}

prompt_choose-a-card-to-move-next-onto-rd = Choose a card to move next onto {-rd}

prompt_choose-a-card-to-trash = Choose a card to trash

prompt_choose-a-card-type = Choose a card type

prompt_choose-a-faceup-card = Choose a faceup card

prompt_choose-a-hosted-card = Choose a hosted card

prompt_choose-a-location-to-install = Choose a location to install {$title}

prompt_choose-a-piece-of-hardware = Choose a piece of hardware

prompt_choose-a-piece-of-hardware-to-trash = Choose a piece of hardware to trash

prompt_choose-a-piece-of-ice = Choose a piece of ice

prompt_choose-a-piece-of-ice-protecting-this-server = Choose a piece of ice protecting this server

prompt_choose-a-piece-of-ice-to-install = Choose a piece of ice to install

prompt_choose-a-piece-of-ice-to-install-from-hq = Choose a piece of ice to install from {-hq}

prompt_choose-a-piece-of-ice-to-place-advancement-counter-on = Choose a piece of ice to place 1 advancement counter on

prompt_choose-a-piece-of-ice-to-trash = Choose a piece of ice to trash

prompt_choose-a-program-or-piece-of-hardware-to-install = Choose a program or piece of hardware to install

prompt_choose-a-program-to-host = Choose a program to host

prompt_choose-a-program-to-install = Choose a program to install

prompt_choose-a-program-to-trash = Choose a program to trash

prompt_choose-a-remote-server = Choose a remote server

prompt_choose-a-remote-server-to-run = Choose a remote server to run

prompt_choose-a-resource-to-trash = Choose a resource to trash

prompt_choose-a-server = Choose a server

prompt_choose-a-server-to-install-on = Choose a server to install {$title} on

prompt_choose-a-subroutine-to-resolve = Choose a subroutine to resolve

prompt_choose-a-trigger-to-resolve = Choose a trigger to resolve

prompt_choose-an-ability-to-resolve = Choose an ability to resolve

prompt_choose-an-ability-to-resolve-remaining = Choose an ability to resolve ({$count} remaining)

prompt_choose-an-agenda = Choose an agenda

prompt_choose-an-agenda-to-add-to-the-bottom-of-rd = Choose an agenda to add to the bottom of {-rd}

prompt_choose-an-agenda-to-forfeit = Choose an agenda to forfeit

prompt_choose-an-event-to-play = Choose an event to play

prompt_choose-an-ice-subtype = Choose an ice subtype

prompt_choose-an-icebreaker = Choose an icebreaker

prompt_choose-an-iced-server = Choose an iced server

prompt_choose-an-installed-card = Choose an installed card

prompt_choose-an-installed-card-to-place-advancement-counters-on-remaining = Choose an installed card to place advancement counters on ({$count} remaining)

prompt_choose-an-installed-card-to-trash = Choose an installed card to trash

prompt_choose-an-installed-card-you-can-advance = Choose an installed card you can advance

prompt_choose-an-installed-runner-card = Choose an installed {-runner(case: "genitive")} card

prompt_choose-another-server-and-redirect-the-run = Choose another server and redirect the run to its outermost position

prompt_choose-any-number-of-cards-in-hq-to-trash = Choose any number of cards in {-hq} to trash

prompt_choose-any-number-of-rezzed-cards-to-trash = Choose any number of rezzed cards to trash

prompt_choose-installed-card-to-trash = Choose {$count ->
    [one] {$count} installed card
    *[other] {$count} installed cards
} to trash

prompt_choose-installed-runner-cards = Choose 2 installed {-runner(case: "genitive")} cards

prompt_choose-one = Choose one

prompt_choose-one-choice-of = Choose one. Choice {$current} of {$total}

prompt_choose-one-remaining = Choose one ({$count} remaining)

prompt_choose-pieces-of-ice-to-swap = Choose 2 pieces of ice to swap

prompt_choose-pieces-of-ice-to-swap-positions = Choose 2 pieces of ice to swap positions

prompt_choose-target = Choose a target for {$title}

prompt_choose-targets-at-least = {$count ->
    [one] Choose at least {$count} target for {$title}
    *[other] Choose at least {$count} targets for {$title}
}

prompt_choose-targets-between = Choose at least {$min} and up to {$max} targets for {$title}

prompt_choose-targets-exactly = {$count ->
    [one] Choose {$count} target for {$title}
    *[other] Choose {$count} targets for {$title}
}

prompt_choose-targets-up-to = {$count ->
    [one] Choose up to {$count} target for {$title}
    *[other] Choose up to {$count} targets for {$title}
}

prompt_choose-up-to-cards-in-hq-to-trash = Choose up to {$count} {$count ->
    [one] card
    *[other] cards
} in {-hq} to trash

prompt_do-core-damage = Do {$count} core damage?

prompt_do-meat-damage = Do {$count} meat damage?

prompt_draw-cards = Draw {$count ->
    [one] {$count} card?
    *[other] {$count} cards?
}

prompt_gain-click = Gain [Click]?

prompt_gain-credits = Gain {$credits} [Credits]?

prompt_give-the-runner-tag = Give {-runner(case: "accusative")} 1 tag?

prompt_how-many-advancement-counters-do-you-want-to-move = How many advancement counters do you want to move?

prompt_how-many-advancement-counters-do-you-want-to-place = How many advancement counters do you want to place?

prompt_how-many-cards-do-you-want-to-draw = How many cards do you want to draw?

prompt_how-many-copies-do-you-want-to-reveal = How many copies of {$title} do you want to reveal?

prompt_how-many-credits-do-you-want-to-move = How many credits do you want to move?

prompt_how-many-credits-do-you-want-to-pay = How many credits do you want to pay?

prompt_how-many-credits-do-you-want-to-spend = How many credits do you want to spend?

prompt_how-many-credits-do-you-want-to-store = How many credits do you want to store?

prompt_how-many-hosted-credits-do-you-want-to-take = How many hosted credits do you want to take?

prompt_how-many-power-counters-do-you-want-to-place = How many power counters do you want to place?

prompt_install = Install {$title}?

prompt_install-a-program = Install a program?

prompt_install-another-copy = Install another copy of {$title}?

prompt_let-the-runner-gain-credits = Let {-runner(case: "accusative")} gain 2 [Credits]?

prompt_look-at-the-top-cards-of-rd = {$count ->
    [one] Look at the top card of {-rd}?
    *[other] Look at the top {$count} cards of {-rd}?
}

prompt_make-another-run-on-rd = Make another run on {-rd}?

prompt_move-to-the-bottom-of-rd = Move {$title} to the bottom of {-rd}?

prompt_move-where = Move {$title} where?

prompt_name-a-runner-card = Name a {-runner(case: "genitive")} card

prompt_pay-credits-to-access-additional-card = Pay {$credits} [Credits] to access 1 additional card?

prompt_pay-credits-to-gain-click = Pay {$credits} [Credits] to gain [Click]?

prompt_pay-credits-to-use-ability = Pay {$credits} [Credits] to use {$title} ability?

prompt_place-power-counter-on = Place 1 power counter on {$title}?

prompt_place-virus-counter = Place 1 virus counter?

prompt_place-virus-counter-on = Place 1 virus counter on {$title}?

prompt_purge-virus-counters = Purge virus counters?

prompt_remove-how-many-tags = Remove how many tags?

prompt_search-for-a-card = Search for a card?

prompt_search-rd-for-a-piece-of-ice = Search {-rd} for a piece of ice?

prompt_search-rd-for-ice-to-install-protecting-a-remote-server = Search {-rd} for a piece of ice to install protecting a remote server?

prompt_shuffle-a-copy-back-into-stack = Shuffle a copy of {$title} back into {-stack}?

prompt_spend-hosted-power-counter-to-access-additional-card = Spend 1 hosted power counter to access 1 additional card?

prompt_swap-pieces-of-ice = Swap 2 pieces of ice?

prompt_swap-with-another-ice = Swap {$card} with another ice?

prompt_take-bad-publicity = Take {$count} {-bad-publicity}?

prompt_the-top-card-of-rd-is = The top card of {-rd} is {$title}

prompt_the-top-card-of-the-stack-is = The top card of {-stack} is {$title}

prompt_top-cards-of-rd = {$count ->
    [one] The top card of {-rd} is {$titles}
    *[other] The top cards of {-rd} are (top->bottom): {$titles}
}

prompt_top-cards-of-stack = {$count ->
    [one] The top card of {-stack} is {$titles}
    *[other] The top cards of {-stack} are (top->bottom): {$titles}
}

prompt_trash = Trash {$title}?

prompt_trash-to-force-the-corp-to-trace = Trash {$title} to force {-corp(case: "accusative")} to trace

prompt_trash-to-reduce-the-base-trace-strength = Trash {$title} to reduce the base trace strength to 0?

prompt_which-position-to-install-in = Which position to install in? (0 is innermost)

prompt_you-have-no-ice-in-rd = You have no ice in {-rd}

## Prompt Choices

choice_add-to-score-area = Add {$title} to score area

choice_bottom-of-rd = Bottom of {-rd}

choice_bottom-of-stack = Bottom of {-stack}

choice_cancel = Cancel

choice_card-from-deck = Card from deck

choice_card-from-hand = Card from hand

choice_carry-on = Carry on!

choice_done = Done

choice_draw-cards = Draw {$count ->
    [one] {$count} card
    *[other] {$count} cards
}

choice_end-the-run = End the run

choice_everything-else = Everything else

choice_facedown-card-in-archives = Facedown card in {-archives}

choice_gain-credits = Gain {$credits} [Credits]

choice_gain-credits-and-draw-cards = Gain {$credits} [Credits] and draw {$count ->
    [one] {$count} card
    *[other] {$count} cards
}

choice_i-want-to-start-over = I want to start over

choice_install = Install {$title}

choice_jack-out = Jack out

choice_no = No

choice_no-action = No action

choice_no-server = No server

choice_none = None

choice_ok = OK

choice_pass-priority = Pass priority

choice_pay-credits = Pay {$credits} [Credits]

choice_pay-to-access = Pay to access

choice_pay-to-steal = Pay to steal

choice_pay-to-trash = Pay {$count} [Credits] to trash

choice_remove-tags = {$count ->
    [one] Remove 1 tag
    *[other] Remove {$count} tags
}

choice_roll-d6 = Roll a d6

choice_spend-click = Spend [Click]

choice_start-over = Start over

choice_steal = Steal

choice_suffer-core-damage = Suffer {$count} core damage

choice_suffer-meat-damage = Suffer {$count} meat damage

choice_suffer-net-damage = Suffer {$count} net damage

choice_take-core-damage = Take {$count} core damage

choice_take-net-damage = Take {$count} net damage

choice_take-tags = Take {$count ->
    [one] {$count} tag
    *[other] {$count} tags
}

choice_the-corp-trashes-a-program = The Corp trashes a program

choice_top-of-rd = Top of {-rd}

choice_top-of-stack = Top of {-stack}

choice_trash = Trash {$card}

choice_trash-an-installed-card = Trash an installed card

choice_unrezzed-upgrade = Unrezzed upgrade

choice_yes = Yes
