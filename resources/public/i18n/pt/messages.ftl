## Logical templates

use-card = {$username} usa {$title} para {$do-ability}.
pay-use-card = {$username} {$payment} para usar {$title} para {$do-ability}.

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}e{" "}
join-list = ,{" "}

## locations

-archives = Arquivos
-hq = QG
-rd = PD

-credit = [Credit]
-click = [Click]

server-name = {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-PD}
    *[other] Servidor {$server}
}

## Ability fragments

trash-card = excluir {$card-str}
trash-card-at-no-cost = excluir {$card-str} sem custo

trash-n-cards = trash {$count ->
    [zero] nenhuma carta
    [one] 1 carta
    *[other] {$count} cartas
}

trash-cards = excluir {$count ->
    [zero] nenhuma carta
    [one] 1 carta ({$card-strs})
    *[other] {$count} cartas ({$card-strs})
}

gain-credits = ganhar {$count} {-credit}

draw-cards = comprar {$count ->
    [one] 1 carta
    *[other] {$count} cartas
}

make-a-run = fazer um corre
make-a-run-on = fazer um corre no {$server}

## Payments

payment-click = gasta {$value} {-click}
payment-credit = paga {$value} {-credit}
