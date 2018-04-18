from unidecode import unidecode

card_types = ['agendas', 'assets', 'events', 'hardware', 'ice', 'icebreakers', 'identities', 'operations', 'programs',
              'resources', 'upgrades']

for card_type in card_types:
    with open(f'{card_type}.clj', 'r', encoding='utf-8') as infile:
        header, *card_defs = infile.read().split('\n\n   "')
        header += '\n'

        for line in card_defs:
            name, *ability = line.splitlines()

            file_name = '-'.join(unidecode(name)
                                 .strip('\ "')
                                 .replace('\\', '')
                                 .replace('"', '')
                                 .replace("'", '')
                                 .replace('/', '')
                                 .replace(':', '')
                                 .replace('*', '')
                                 .replace('.', '')
                                 .replace(',', '')
                                 .lower()
                                 .split(' '))
            def_name = f'(def card-definitions-{card_type}-{file_name}'

            name = f'  {{"{name.strip()}'
            ability = '\n'.join(ability) + '})'

            lines = [header, def_name, name, ability]
            # print('\n'.join(lines))
            # exit()

            with open(f'{card_type}/{file_name}.clj', 'w', encoding='utf-8', newline='\r\n') as outfile:
                print(f'{card_type}/{file_name}.clj')
                outfile.write('\n'.join(lines))
                outfile.write('\n')
