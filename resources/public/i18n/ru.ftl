annotations_available-annotations = Доступные аннотации
annotations_clear = Очистить локальные аннотации
annotations_click-placeholder = Заметки к этому клику
annotations_import-local = Импортировать файл с аннотациями
annotations_load-local = Загрузить
annotations_no-published-annotations = Нет опубликованных аннотаций.
annotations_publish = Опубликовать
annotations_save-local = Сохранить
annotations_turn-placeholder = Заметки к этому ходу

card-browser_advancement = Требование к продвижению
card-browser_agenda-points = Победные очки
card-browser_artist-info = Информация о художнике
card-browser_clear = Сбросить
card-browser_cost = Цена
card-browser_faction = Фракция
card-browser_format = Формат
card-browser_implementation-note = undefined
card-browser_inf-limit = Предел влияния
card-browser_influence = Влияние
card-browser_memory = Память
card-browser_min-deck-size = Минимальный размер колоды
card-browser_more-info = Больше информации
card-browser_search-hint = Искать карты
card-browser_select-art = Выбрать изображение
card-browser_selected-art = Выбранное изображение
card-browser_set = Набор
card-browser_side = Сторона
card-browser_sort = Сортировать по
card-browser_sort-by = {$by ->
    [cost] Цена
    [faction] Фракция
    [influence] Влияние
    [name] Название
    [set-number] Номер в наборе
    [type] Тип
    *[unknown] undefined
}
card-browser_strength = Сила
card-browser_trash-cost = Цена сноса
card-browser_type = Тип
card-browser_update-failure = Ошибка обновления изображения
card-browser_update-success = Изображение обновлено

card-type_name = {$type ->
    [agenda] Проект
    [all] Все
    [asset] Актив
    [event] Событие
    [hardware] Устройство
    [ice] Лёд
    [identity] Роль
    [operation] Операция
    [program] Программа
    [resource] Ресурс
    [upgrade] Улучшение
    *[unknown] undefined
}

chat_block = Заблокировать пользователя
chat_cancel = Отмена
chat_channels = Каналы
chat_delete = Удалить сообщение
chat_delete-all = Удалить все сообщения пользователя
chat_length-exceeded = Превышена длина
chat_message-blocked = Сообщение заблокировано: {$reason-str}
chat_placeholder = Напишите что-нибудь...
chat_rate-exceeded = Превышена частота
chat_send = Отправить
chat_title = Играть в Netrunner в браузере

deck-builder_add-cards = Добавить карты
deck-builder_add-to-deck = Добавить в колоду
deck-builder_agenda-points = Победные очки
deck-builder_cancel = Отмена
deck-builder_card-count = {$cnt ->
    *[one] {$cnt} карты
    [few] {$cnt} карты
    [many] {$cnt} карты
    [other] {$cnt} карты
}
deck-builder_card-name = Название карты
deck-builder_clear-stats = Сбросить статистику
deck-builder_completed = Зав.
deck-builder_confirm-delete = Подтвердить удаление
deck-builder_copy = Скопировать
deck-builder_create-game = Создать игру
deck-builder_deck-copy-suffix = копия
deck-builder_deck-count = {$cnt ->
    [zero] Нет колод
    *[one] {$cnt} колода
    [few] {$cnt} колоды
    [many] {$cnt} колод
    [other] {$cnt} колод
}
deck-builder_deck-count-filtered = {$cnt ->
    [zero] Нет колод (Фильтр)
    *[one] {$cnt} колода (Фильтр)
    [few] {$cnt} колоды (Фильтр)
    [many] {$cnt} колод (Фильтр)
    [other] {$cnt} колод (Фильтр)
}
deck-builder_deck-name = Название колоды
deck-builder_deck-notes = Примечания
deck-builder_deck-points = Очки колоды
deck-builder_decklist = Список карт
deck-builder_decklist-inst = (Напишите или вставьте список карт, он будет распознан)
deck-builder_delete = Удалить
deck-builder_edit = Изменить
deck-builder_format = Формат
deck-builder_games = Игр
deck-builder_hash = Турнирный хеш
deck-builder_identity = Роль
deck-builder_illegal = : нелегальна
deck-builder_import = Импортировать
deck-builder_import-button = Импорт колоды
deck-builder_import-placeholder = undefined
deck-builder_import-title = Вставьте ID или ссылку на публичную колоду с NRDB
deck-builder_influence = Влияние
deck-builder_legal = : легальна
deck-builder_loading-msg = Загрузка коллекции колод...
deck-builder_lost = Пор.
deck-builder_max = максимум
deck-builder_min = минимум
deck-builder_min-deck-size = Минимальный размер колоды
deck-builder_new-corp = Создать Корпу
deck-builder_new-deck = Новая колода
deck-builder_new-runner = Создать Бегущего
deck-builder_notes = Примечания
deck-builder_reset = Сброс
deck-builder_save = Сохранить
deck-builder_why = Почему?
deck-builder_won = Побед

diagrams_run-timing_approach = 6.9.2: Фаза приближения ко льду
diagrams_run-timing_approach-a = Вы приближаетесь ко льду. Срабатывают соответствующие эффекты карт
diagrams_run-timing_approach-b = Окно платной способности. Корпорация может развернуть лёд, к которому приближается Бегущий, а также другие карты, помимо льдов
diagrams_run-timing_approach-c = Если лёд, к которому приближается Бегущий, развёрнут, перейдите к фазе встречи (6.9.3)
diagrams_run-timing_approach-d = В противном случае перейдите к фазе движения (6.9.4)
diagrams_run-timing_disclaimer = Данная схема была упрощена для наглядности. Полные правила можно изучить на сайте Null Signal Games.
diagrams_run-timing_encounter = 6.9.3: Фаза встречи со льдом
diagrams_run-timing_encounter-a = Вы встретились с данным льдом. Срабатывают соответствующие эффекты карт
diagrams_run-timing_encounter-b = Окно платной способности. Бегущий может контактировать со льдом во время этого окна
diagrams_run-timing_encounter-c = Если остались несломанные подпрограммы, Корпорация выполняет верхнюю несломанную подпрограмму. По завершении повторите этот шаг
diagrams_run-timing_encounter-d = Встреча завершена. Перейдите к фазе движения (6.9.4)
diagrams_run-timing_header = Структура забега
diagrams_run-timing_initiation = 6.9.1: Фаза инициации
diagrams_run-timing_initiation-a = Бегущий объявляет сервер
diagrams_run-timing_initiation-b = Бегущий получает кредиты плохой репутации
diagrams_run-timing_initiation-c = Забег формально начинается - Срабатывают эффекты событий-забегов
diagrams_run-timing_initiation-d = Перейдите к внешнему льду при его наличии и начните фазу приближения (6.9.2)
diagrams_run-timing_initiation-e = В противном случае перейдите к фазе движения (6.9.4)
diagrams_run-timing_movement = 6.9.4: Фаза движения
diagrams_run-timing_movement-a = Если вы были во встрече со льдом или приближались ко льду, вы его проходите. Срабатывают карты с формулировкой 'при прохождении льда'
diagrams_run-timing_movement-b = Если между вами и корнем сервера не осталось больше льдов, срабатывают карты с формулировкой 'при прохождении всех льдов на сервере'
diagrams_run-timing_movement-c = Окно платной способности
diagrams_run-timing_movement-d = Бегущий может отключиться. Если Бегущий отключился, перейдите к фазе завершения забега (6.9.6)
diagrams_run-timing_movement-e = Бегущий продвигается на одну позицию по направлению к корню сервера, если это возможно
diagrams_run-timing_movement-f = Окно платной способности. Корпорация может разворачивать карты, кроме льдов
diagrams_run-timing_movement-g = Если вы приближаетесь ко льду, перейдите к фазе приближения ко льду (6.9.2)
diagrams_run-timing_movement-h = Бегущий приближается к атакуемому серверу. Срабатывают соответствующие эффекты карт
diagrams_run-timing_movement-i = Перейдите к фазе успеха (6.9.5)
diagrams_run-timing_run-ends = 6.9.6: Фаза завершения забега
diagrams_run-timing_run-ends-a = Все открытые окна приоритета завершаются или закрываются
diagrams_run-timing_run-ends-b = Бегущий теряет все непотраченные кредиты плохой репутации
diagrams_run-timing_run-ends-c = Если данной фазе не предшествовала фаза успеха, а сервер всё ещё существует, забег объявляется неуспешным
diagrams_run-timing_run-ends-d = Забег завершается. Срабатывают карты с формулировкой 'по завершении забега'
diagrams_run-timing_success = 6.9.5: Фаза успеха
diagrams_run-timing_success-a = Забег объявляется успешным. Срабатывают карты с формулировкой 'при успешном забеге'
diagrams_run-timing_success-b = Бегущий взламывает атакуемый сервер
diagrams_run-timing_success-c = Фаза успеха завершена. Перейдите к фазе завершения забега (6.9.6)
diagrams_turn_corp-action-phase = 5.6.2: Фаза действий
diagrams_turn_corp-action-phase-a = Окно платной способности. Корпорация может разворачивать карты, кроме льдов, и засчитывать проекты
diagrams_turn_corp-action-phase-b = Если у Корпорации есть непотраченные [Clicks], она совершает действие
diagrams_turn_corp-action-phase-c = Если действие было совершено, вернитесь к (a)
diagrams_turn_corp-action-phase-d = Фаза действий завершена. Перейдите к фазе сброса (5.6.3)
diagrams_turn_corp-discard-phase = 5.6.3: Фаза сброса
diagrams_turn_corp-discard-phase-a = Корпорация сбрасывает карты до максимального размера руки при необходимости
diagrams_turn_corp-discard-phase-b = Окно платной способности. Корпорация может разворачивать карты, кроме льдов
diagrams_turn_corp-discard-phase-c = Если у Корпорации остались [Clicks], она теряет эти [Clicks]
diagrams_turn_corp-discard-phase-d = Ход Корпорации формально окончен. Срабатывают карты с формулировкой 'в конце хода'
diagrams_turn_corp-discard-phase-e = Перейдите к ходу Бегущего
diagrams_turn_corp-draw-phase = 5.6.1: Фаза добора
diagrams_turn_corp-draw-phase-a = Корпорация получает полагающиеся клики (по умолчанию: [click][click][click])
diagrams_turn_corp-draw-phase-b = Окно платной способности. Корпорация может разворачивать карты, кроме льдов, и засчитывать проекты
diagrams_turn_corp-draw-phase-c = Обновляются возобновляемые кредиты Корпорации
diagrams_turn_corp-draw-phase-d = Ход формально начинается. Срабатывают карты с формулировкой 'в начале хода'
diagrams_turn_corp-draw-phase-e = Корпорация берёт 1 карту с верха R&D
diagrams_turn_corp-draw-phase-f = Перейдите к фазе действий (5.6.2)
diagrams_turn_corp-turn = Ход Корпорации
diagrams_turn_runner-action-phase = 5.7.1: Фаза действий
diagrams_turn_runner-action-phase-a = Бегущий получает полагающиеся клики (по умолчанию: [click][click][click][click])
diagrams_turn_runner-action-phase-b = Окно платной способности. Корпорация может разворачивать карты, кроме льдов
diagrams_turn_runner-action-phase-c = Обновляются возобновляемые кредиты Бегущего
diagrams_turn_runner-action-phase-d = Ход формально начинается. Срабатывают карты с формулировкой 'в начале хода'
diagrams_turn_runner-action-phase-e = Окно платной способности. Корпорация может разворачивать карты, кроме льдов
diagrams_turn_runner-action-phase-f = Если у Бегущего есть непотраченные [Clicks], он совершает действие
diagrams_turn_runner-action-phase-g = Если действие было совершено, вернитесь к (e)
diagrams_turn_runner-action-phase-h = Фаза действий завершена. Перейдите к фазе сброса (5.7.2)
diagrams_turn_runner-discard-phase = 5.7.2: Фаза сброса
diagrams_turn_runner-discard-phase-a = Бегущий сбрасывает карты до максимального размера руки при необходимости
diagrams_turn_runner-discard-phase-b = Окно платной способности. Корпорация может разворачивать карты, кроме льдов
diagrams_turn_runner-discard-phase-c = Если у Бегущего остались [Clicks], он теряет эти [Clicks]
diagrams_turn_runner-discard-phase-d = Ход Бегущего формально окончен. Срабатывают карты с формулировкой 'в конце хода'
diagrams_turn_runner-discard-phase-e = Перейдите к ходу Корпорации
diagrams_turn_runner-turn = Ход Бегущего

faction_name = {$faction ->
    [adam] Адам
    [all] Все
    [anarch] Анарх
    [any-faction] Все фракции
    [apex] Апекс
    [criminal] Криминал
    [haas-bioroid] Хаас-Биороид
    [jinteki] Джинтеки
    [nbn] undefined
    [neutral] Нейтральные
    [shaper] Шейпер
    [sunny-lebeau] Санни Лебо
    [weyland-consortium] Консорциум Вейланда
    *[unknown] undefined
}

format_name = {$format ->
    [all] Все
    [any-format] Любой формат
    [casual] Свободный
    [classic] Классический
    [core-experience] undefined
    [eternal] Вечный
    [neo] Нео
    [preconstructed] Чемпионский
    [snapshot] undefined
    [snapshot-plus] undefined
    [socr] undefined
    [standard] Стандарт
    [startup] Стартап
    [sunset] Закат
    [system-gateway] Врата Системы
    [throwback] undefined
    *[unknown] undefined
}

game_abilities = Способности
game_actions = Действия
game_agenda-count = {$cnt ->
    *[one] {$cnt} победное очко
    [few] {$cnt} победных очка
    [many] {$cnt} победных очков
    [other] {$cnt} победных очков
}
game_agenda-count-with-req = undefined
game_approach-ice = Приближение ко льду
game_archives = Архивы{"\u00A0"}({$faceup} ↑ {$facedown} ↓)
game_attempt-reconnect = Переподключиться
game_auto-pass = Автопередача приоритета
game_bad-pub-count = {$cnt ->
    *[one] {$cnt} плохая репутация
    [few] {$cnt} плохих репутации
    [many] {$cnt} плохих репутаций
    [other] {$cnt} плохих репутаций
}
game_bad-pub-count-additional = {$cnt ->
    *[one] {$cnt} плохая репутация + {$additional}
    [few] {$cnt} плохих репутации + {$additional}
    [many] {$cnt} плохих репутаций + {$additional}
    [other] {$cnt} плохих репутаций + {$additional}
}
game_beat-trace = Перебить слежку
game_brain-damage = {$cnt ->
    *[one] {$cnt} критический урон
    [few] {$cnt} критического урона
    [many] {$cnt} критических уронов
    [other] {$cnt} критических уронов
}
game_breach-server = Взломать сервер
game_card = Карта
game_card-count = {$cnt ->
    *[one] {$cnt} карта
    [few] {$cnt} карты
    [many] {$cnt} карт
    [other] {$cnt} карт
}
game_click-count = {$cnt ->
    *[one] {$cnt} клик
    [few] {$cnt} клика
    [many] {$cnt} кликов
    [other] {$cnt} кликов
}
game_close = Закрыть
game_close-shuffle = Закрыть и перемешать
game_concede = Сдаться
game_continue = Продолжить
game_continue-to = Продолжить к фазе:
game_corp-view = Перспектива Корпорации
game_credit-count = {$cnt ->
    *[one] {$cnt} кредит
    [few] {$cnt} кредита
    [many] {$cnt} кредитов
    [other] {$cnt} кредитов
}
game_credit-count-with-run-credits = {$cnt ->
    *[one] {$cnt} кредит ({$run-credit} для забега)
    [few] {$cnt} кредита ({$run-credit} для забега)
    [many] {$cnt} кредитов ({$run-credit} для забега)
    [other] {$cnt} кредитов ({$run-credit} для забега)
}
game_credits = кредитов
game_current = Течение
game_current-phase = Текущая фаза
game_draw = Взять карту
game_encounter-ice = Встреча со льдом
game_end-turn = Закончить ход
game_error = Внутренняя ошибка сервера. Пожалуйста, напишите /bug в чат и следуйте инструкциям.
game_face-down-count = {$cnt ->
    *[one] {$cnt} карта, {$facedown} лицом вниз
    [few] {$cnt} карты, {$facedown} лицом вниз
    [many] {$cnt} карт, {$facedown} лицом вниз
    [other] {$cnt} карт, {$facedown} лицом вниз
}
game_fire-unbroken = Выполнить несломанные подпрограммы
game_gain-credit = Получить кредит
game_game-start = Начало игры
game_grip = Рука
game_heap = Куча
game_hq = undefined
game_inactivity = Игра закрыта из-за отсутствия активности
game_indicate-action = Обозначить действие
game_initiation = Инициация
game_jack-out = Отключиться
game_keep = Оставить
game_last-revealed = undefined
game_leave = Выйти из игры
game_leave-replay = Выйти из записи
game_let-subs-fire = Разрешить выполнение несломанных подпрограмм
game_link-strength = Мощность канала
game_lost-connection = Утрачено соединени с сервером. Идёт переподключение.
game_mandatory-draw = Карта за начало хода
game_max-hand = Макс. размер руки
game_minutes = м:
game_movement = Движение
game_mu-count = {$unused} из {$available} свободных ЯП
game_mulligan = Пересдать
game_mute = Выключить сообщения зрителей
game_no-current-run = Нет активного забега
game_no-further = Нет дальнейших действий
game_ok = undefined
game_play-area = Игровая зона
game_prompt = {$msg ->
    [advance] продвинуть
    [archives] Архивы
    [derez] свернуть
    [expend] Израсходовать
    [hq] undefined
    [new-remote] Новый сервер
    [r-d] undefined
    [rez] развернуть
    [score] засчитать
    [server-1] Сервер 1
    [server-10] Сервер 10
    [server-2] Сервер 2
    [server-3] Сервер 3
    [server-4] Сервер 4
    [server-5] Сервер 5
    [server-6] Сервер 6
    [server-7] Сервер 7
    [server-8] Сервер 8
    [server-9] Сервер 9
    [trash] снести
    *[unknown] undefined
}
game_purge = Сбросить вирусы
game_reconnected-to-server = Вы переподключены к серверу
game_remove-tag = Снять метку
game_reveal-my-hand = undefined
game_rez = Развернуть
game_rez-all = undefined
game_rfg = Удалены из игры
game_rnd = undefined
game_run = Начать забег
game_run-ends = Забег завершается
game_runner-view = Перспектива Бегущего
game_scored-area = Зона зачёта
game_seconds = с
game_seconds-remaining = с осталось
game_server = Сервер
game_set-aside = Отложить в сторону
game_show = Показать
game_show-decklists = undefined
game_shuffle = Перемешать
game_spec-count = {$cnt ->
    *[one] {$cnt} зритель
    [few] {$cnt} зрителя
    [many] {$cnt} зрителей
    [other] {$cnt} зрителей
}
game_spec-view = Перспектива зрителя
game_special-mu-count = {$unused} из {$available} {$mu-type} свободных ЯП
game_stack = Стек
game_start = Начать игру
game_start-turn = Начать ход
game_stop-auto-pass = Прекратить автопередачу приоритета
game_subs = Подпрограммы
game_success = Успех
game_tag-count = {$cnt ->
    *[one] {$cnt} метка
    [few] {$cnt} метки
    [many] {$cnt} меток
    [other] {$cnt} меток
}
game_tag-count-additional = {$cnt ->
    *[one] {$cnt} метка + {$additional}
    [few] {$cnt} метки + {$additional}
    [many] {$cnt} меток + {$additional}
    [other] {$cnt} меток + {$additional}
}
game_take-clicks = Получить клики
game_time-taken = Затраченное время: {$time} минут
game_timeout-soon = undefined
game_trace = Отследить
game_trash-like-cards = undefined
game_trash-resource = Снести ресурс
game_unbeatable = Сделать неперебиваемой
game_unimplemented = Не реализовано
game_unknown-phase = Неизвестная фаза
game_unmute = Включить сообщения зрителей
game_win-claimed = {$winner} ({$side}) объявляет победу на  {$turn} ходу
game_win-conceded = {$winner} ({$side}) побеждает. Соперник сдался на {$turn} ходу
game_win-decked = {$winner} ({$side}) побеждает. Соперник не смог взять карту из R&D на {$turn} ходу
game_win-flatlined = {$winner} ({$side}) побеждает. Соперник получил флетлайн на {$turn} ходу
game_win-other = побеждает из-за {$reason} на {$turn} ходу
game_win-points = {$winner} ({$side}) побеждает, засчитав победные очки на {$turn} ходу

ingame-settings_alt-art = Альтернативные изображения
ingame-settings_board-overlap = Пересечение столов
ingame-settings_card-backs = Рубашки
ingame-settings_card-image = Изображение карты
ingame-settings_card-images = Изображения карт
ingame-settings_card-sorting = Сортировка
ingame-settings_card-stacking = Настройки карт
ingame-settings_card-text = Текст карты
ingame-settings_display-encounter-info = undefined
ingame-settings_game-settings = undefined
ingame-settings_ghost-trojans = Показывать дубликаты троянов в риге
ingame-settings_high-res = Включить изображения высокого разрешения
ingame-settings_label-faceup-cards = Подписывать карты лицом вверх
ingame-settings_label-unrezzed-cards = Подписывать неразвёрнутые карты
ingame-settings_log-timestamps = undefined
ingame-settings_log-timestamps-toggle = undefined
ingame-settings_pass-on-rez = undefined
ingame-settings_preview-zoom = Увеличение карт при наведении
ingame-settings_runner-board-order = Раскладка Бегущего
ingame-settings_runner-classic = Классическая
ingame-settings_runner-reverse = Обратная
ingame-settings_save = Сохранить
ingame-settings_show-alt = Показывать альтернативные изображения карт
ingame-settings_sides-overlap = Карты Бегущего и Корпорации могут пересекаться
ingame-settings_sort-archives = Отсортировать Архивы
ingame-settings_sort-heap = Отсортировать Кучу
ingame-settings_stack-cards = Складывать одинаковые карты вместе

lobby_aborted = Соединение разорвано
lobby_api-access = Разрешить доступ к данным игры через API
lobby_api-access-details = Данная опция даёт доступ к информации о вашей игре расширенияим от сторонних разработчиков. Требует создание ключа API в Настройках.
lobby_api-requires-key = (Требуется ключ API в Настройках)
lobby_as-corp = За Корпорацию
lobby_as-runner = За Бегущего
lobby_both-perspective = Перспектива обоих
lobby_cancel = Отмена
lobby_chat = Чат
lobby_closed-msg = Лобби закрыто из-за отсутствия активности
lobby_completion-rate = Процент завершённых игр
lobby_corp-perspective = Перспектива Корпорации
lobby_create = Создать
lobby_deck-selected = Колода выбрана
lobby_default-game-format = Формат по умолчанию
lobby_delete = Удалить игру
lobby_filter = Фильтр
lobby_format = Формат
lobby_game-count = {$cnt ->
    *[one] {$cnt} игра
    [few] {$cnt} игры
    [many] {$cnt} игр
    [other] {$cnt} игр
}
lobby_game-count-filtered = {$cnt ->
    *[one] {$cnt} игра (с фильтром)
    [few] {$cnt} игры (с фильтром)
    [many] {$cnt} игр (с фильтром)
    [other] {$cnt} игр (с фильтром)
}
lobby_gateway-format = {$format ->
    [beginner] Новичок
    [beginner-info] В этом лобби можно играть стартовыми колодами Корпорации и Бегущего из набора Врата Cистемы. С этих колод рекомендуется начинать знакомство с игрой. Игра ведётся до 6 победных очков.
    [beginner-ul] Врата Системы - Стартовые обучающие колоды
    [constructed] Свои колоды
    [intermediate] Продвинутый
    [intermediate-info] В этом лобби можно играть продвинутыми колодами Корпорации и Бегущего из набора Врата Cистемы. В этих колодах больше опций и механик, чем в стартовых. Игра ведётся до 7 победных очков.
    [intermediate-ul] Врата Системы - Продвинутые обучающие колоды
    *[unknown] undefined
}
lobby_hidden = Раскрыть зрителям скрытую информацию
lobby_hidden-details = Данная опция раскроет всю скрытую информацию обеих сторон для ВСЕХ зрителей вашей игры, в том числе руки и карты лицом вниз.
lobby_hidden-password = Мы рекомендуем использовать пароль, чтобы посторонние не помешали вашей игре.
lobby_invalid-password = Неправильный пароль
lobby_join = Войти
lobby_leave = Выйти
lobby_load-replay = Загрузить запись
lobby_new-game = Новая игра
lobby_no-games = Нет игр
lobby_not-allowed = Не разрешено
lobby_open-decklists = undefined
lobby_open-decklists-b = undefined
lobby_options = Параметры
lobby_password = Пароль
lobby_password-error = Пожалуйста, введите пароль.
lobby_password-for = Пароль для
lobby_password-protected = Защитить паролем
lobby_players = Игроки
lobby_private = ЗАКРЫТАЯ
lobby_rejoin = Перезайти
lobby_reload = Перезагрузить список
lobby_replay-invalid-file = Выберите подходящий файл записи.
lobby_replay-link-error = Ссылка на запись недействительна.
lobby_reset = Сбросить название игры
lobby_runner-perspective = Перспектива Бегущего
lobby_save-replay = Сохранить запись
lobby_save-replay-beta = БЕТА функционал: У нас может возникнуть необходимость удалить сохранённые записи, поэтому обязательно скачивайте важные для вас повторы. Будущие изменения функционала сайта могут нарушить совместимость со старыми записями.
lobby_save-replay-details = Данная опция сохранит запись игры с открытой информацией (например, о картах в руке). Файл будет доступен по завершении игры.
lobby_save-replay-unshared = Мы храним 15 ваших последних записей, которыми вы не делились, так что не забудьте скачать повтор или поделиться им после игры.
lobby_select-deck = Выбрать колоду
lobby_select-error = Невозможно выбрать эту колоду
lobby_select-title = Выберите вашу колоду
lobby_side = Сторона
lobby_singleton = Синглтон
lobby_singleton-b = (синглтон)
lobby_singleton-details = С данной опцией играть можно только колодами без повторяющихся карт. Рекомендуется использовать следующие карты ролей.
lobby_singleton-example = 1) Nova Initiumia: Catalyst & Impetus 2) Ampere: Cybernetics For Anyone
lobby_singleton-restriction = Данное лобби запущено в режиме "Синглтон". Играть можно только колодами без повторяющихся карт.
lobby_spectator-count = {$cnt ->
    *[one] {$cnt} зритель
    [few] {$cnt} зрителя
    [many] {$cnt} зрителей
    [other] {$cnt} зрителей
}
lobby_spectators = Допускать зрителей
lobby_start = Начать
lobby_start-replay = Запустить запись
lobby_swap = Изменить стороны
lobby_timed-game = Запустить таймер
lobby_timed-game-details = Таймер носит информационный характер: игра будет продолжаться, когда время выйдет.
lobby_timer-length = Продолжительность таймера (минут)
lobby_title = Название
lobby_title-error = Пожалуйста, введите название игры.
lobby_too-little-data = Слишком мало данных
lobby_type = {$type ->
    [angel-arena] Арена Ангелов
    [casual] Обычные
    [competitive] Соревновательные
    [tournament] Турнирные
    *[unknown] undefined
}
lobby_waiting = Игроки выбирают колоды
lobby_watch = Смотреть

log_annotating = Аннотации
log_game-log = Журнал
log_remote-annotations-fail = Не удалось загрузить аннотации.
log_run-timing = Фазы забега
log_settings = Настройки
log_shared = Опубликованные аннотации
log_turn-timing = Фазы хода

menu_admin = {nav_admin}
menu_donor = Спонсор
menu_logout = Отключиться
menu_moderator = Модератор
menu_settings = {nav_settings}

missing = :ru missing text

nav_about = О нас
nav_admin = Администратор
nav_cards = Карты
nav_chat = Чат
nav_deck-builder = Колоды
nav_features = Возможности
nav_game-count = {$cnt ->
    *[one] {$cnt} игра
    [few] {$cnt} игры
    [many] {$cnt} игр
    [other] {$cnt} игр
}
nav_help = Помощь
nav_play = Играть
nav_settings = Настройки
nav_stats = Статистика
nav_tournaments = Турниры
nav_users = Пользователи
nav_welcome = Правила

preconstructed_worlds-2012-a = ЧМ 2012: Бен Марш (К) против Джереми Звирна (Б)
preconstructed_worlds-2012-a-tag = Бен Марш (К) против Джереми Звирна (Б)
preconstructed_worlds-2012-a-ul = ЧМ 2012: Вейланд против Криминала
preconstructed_worlds-2012-b = ЧМ 2012: Джереми Звирн (К) против Бена Марша (Б)
preconstructed_worlds-2012-b-tag = Джереми Звирн (К) против Бена Марша (Б)
preconstructed_worlds-2012-b-ul = ЧМ 2012: Хаас-Биороид против Криминала
preconstructed_worlds-2012-info = Во время Чемпионата мира 2012 единственным легальным набором был Базовый (до 3 коробок). Джереми Звирн (Building a Better World, Gabriel Santiago) выиграл в финале у Бена Марша (Engineering the Future, Gabriel Santiago) и стал первым чемпионом мира по Netrunner.
preconstructed_worlds-2013-a = ЧМ 2013: Йенс Эриксон (К) против Эндрю Вина (Б)
preconstructed_worlds-2013-a-tag = Йенс Эриксон (К) против Эндрю Вина (Б)
preconstructed_worlds-2013-a-ul = ЧМ 2013: HB FastAdv против Shaper Katman
preconstructed_worlds-2013-b = ЧМ 2013: Эндрю Вин (К) против Йенса Эриксона (Б)
preconstructed_worlds-2013-b-tag = Эндрю Вин (К) против Йенса Эриксона (Б)
preconstructed_worlds-2013-b-ul = ЧМ 2013: NBN Fast Adv против Andy Sucker
preconstructed_worlds-2013-info = В Чемпионате мира 2013 года участвовали 166 игроков. Турнир проходил в Миннеаполисе, штат Миннесота, США, и состоял из 6 раундов по швейцарской системе, переходящих в топ кат на 32 участника. Последний легальный набор: Opening Moves.
preconstructed_worlds-2014-a = ЧМ 2014: Дэн Д'Ардженио (К) против Минна Трана (Б)
preconstructed_worlds-2014-a-tag = Дэн Д'Ардженио (К) против Минна Трана (Б)
preconstructed_worlds-2014-a-ul = ЧМ 2014: Honor and Perfection против Andromedium
preconstructed_worlds-2014-b = ЧМ 2014: Минн Тран (К) против Дэна Д'Ардженио (Б)
preconstructed_worlds-2014-b-tag = Минн Тран (К) против Дэна Д'Ардженио (Б)
preconstructed_worlds-2014-b-ul = ЧМ 2014: Personal Evolution против Daily QT Andy
preconstructed_worlds-2014-info = В Чемпионате мира 2014 года участвовали 238 игроков. Турнир проходил в Миннеаполисе, штат Миннесота, США, и состоял из 7 раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: Up and Over.
preconstructed_worlds-2015-a = ЧМ 2015: Дэн Д'Ардженио (К) против Тимми Вона (Б)
preconstructed_worlds-2015-a-tag = Дэн Д'Ардженио (К) против Тимми Вона (Б)
preconstructed_worlds-2015-a-ul = ЧМ 2015: Foodcoatshop против The Morning After
preconstructed_worlds-2015-b = ЧМ 2015: Тимми Вон (К) против Дэна Д'Ардженио (Б)
preconstructed_worlds-2015-b-tag = Тимми Вон (К) против Дэна Д'Ардженио (Б)
preconstructed_worlds-2015-b-ul = ЧМ 2015: Yellow Shell против Radisson Cheese Plate
preconstructed_worlds-2015-info = В Чемпионате мира 2015 года участвовали 269 игроков. Турнир проходил в Миннеаполисе, штат Миннесота, США, и состоял из 8 раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: Data and Destiny.
preconstructed_worlds-2016-a = ЧМ 2016: Крис Даер (К) против Бенджамина Ни (Б)
preconstructed_worlds-2016-a-tag = Крис Даер (К) против Бенджамина Ни (Б)
preconstructed_worlds-2016-a-ul = ЧМ 2016: Snekbite против Minh MaxX++
preconstructed_worlds-2016-b = ЧМ 2016: Бенджамин Ни (Б) против Криса Даера (К)
preconstructed_worlds-2016-b-tag = Бенджамин Ни (Б) против Криса Даера (К)
preconstructed_worlds-2016-b-ul = ЧМ 2016: Fiery Info против Papa Smurf
preconstructed_worlds-2016-info = В Чемпионате мира 2016 года участвовали 278 игроков. Турнир проходил в Миннеаполисе, штат Миннесота, США, и состоял из 9 раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: Escalation.
preconstructed_worlds-2017-a = ЧМ 2017: Джесс Хориг (К) против Грей Тон (Б)
preconstructed_worlds-2017-a-tag = Джесс Хориг (К) против Грей Тон (Б)
preconstructed_worlds-2017-a-ul = ЧМ 2017: Stinson Reversed CI против Aesops Hayley
preconstructed_worlds-2017-b = ЧМ 2017: Грей Тон (К) против Джесс Хориг (Б)
preconstructed_worlds-2017-b-tag = Грей Тон (К) против Джесс Хориг (Б)
preconstructed_worlds-2017-b-ul = ЧМ 2017: No-Show Rewiring CI против Laguna Lock Hayley
preconstructed_worlds-2017-info = В Чемпионате мира 2016 года участвовали 233 игрока. Турнир проходил в Миннеаполисе, штат Миннесота, США, и состоял из 8(?) раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: Revised Core set.
preconstructed_worlds-2018-a = ЧМ 2018: Джо Шуп (К) против Криса Даера (Б)
preconstructed_worlds-2018-a-tag = Джо Шуп (К) против Криса Даера (Б)
preconstructed_worlds-2018-a-ul = ЧМ 2018: AMERICA CtM против Gooseberry MaxX
preconstructed_worlds-2018-b = ЧМ 2018: Крис Даер (К) против Джо Шупа (Б)
preconstructed_worlds-2018-b-tag = Крис Даер (К) против Джо Шупа (Б)
preconstructed_worlds-2018-b-ul = ЧМ 2018: Trust the Process против Dan D'Argenio KoS Val
preconstructed_worlds-2018-info = В Чемпионате мира 2016 года участвовали 403(!) игрока. Это последний ЧМ, прошедший под крылом FFG. Турнир проходил в Миннеаполисе, штат Миннесота, США, и состоял из 9(?) раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: Reign and Reverie
preconstructed_worlds-2019-a = ЧМ 2019: Pinsel (К) против Testrunning (Б)
preconstructed_worlds-2019-a-tag = Pinsel (К) против Testrunning (Б)
preconstructed_worlds-2019-a-ul = ЧМ 2019: Fully dedicated to efficiency против Trash Panda
preconstructed_worlds-2019-b = ЧМ 2019: Testrunning (К) против Pinsel (Б)
preconstructed_worlds-2019-b-tag = Testrunning (К) против Pinsel (Б)
preconstructed_worlds-2019-b-ul = ЧМ 2019: 2 Grid for 2 Place против Trash Panda
preconstructed_worlds-2019-info = В первом Чемпионате мира по Netrunner, устроенным Project NISEI в 2019 году, поучаствовали 256 человек. Турнир проходил в Роттердаме, Нидерланды, и состоял из 9 раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: Uprising Booster Pack
preconstructed_worlds-2020-a = ЧМ 2020: Limes (К) против tf34 (Б)
preconstructed_worlds-2020-a-tag = Limes (К) против tf34 (Б)
preconstructed_worlds-2020-a-ul = ЧМ 2020: I don't like this deck против Engolo Freedom
preconstructed_worlds-2020-b = ЧМ 2020: tf34 (Б) против Limes (К)
preconstructed_worlds-2020-b-tag = tf34 (Б) против Limes (К)
preconstructed_worlds-2020-b-ul = ЧМ 2020: Malia CTM против Imp-pressive Hoshiko
preconstructed_worlds-2020-info = В первом онлайн Чемпионате мира по Netrunner, проведённом Project NISEI в 2020 году, приняли участие 294 игрока. Из-за ограничений в перемещениях в начале пандемии COVID-19 турнир был проведён онлайн на jinteki.net и состоял из 8 раундов по швейцарской системе, разбитых на 2 дня для двух разных групп игроков, переходящих в топ кат на 16 участников. Последний легальный набор: Uprising.
preconstructed_worlds-2021-a = ЧМ 2021: Патрик Гауэр (К) против Джонаса (Б)
preconstructed_worlds-2021-a-tag = Патрик Гауэр (К) против Джонаса (Б)
preconstructed_worlds-2021-a-ul = ЧМ 2021: 44 Card PD против Watch Me Drip, Watch Me Maemi
preconstructed_worlds-2021-b = ЧМ 2021: Джонас (К) против Патрика Гауэра (Б)
preconstructed_worlds-2021-b-tag = Джонас (К) против Патрика Гауэра (Б)
preconstructed_worlds-2021-b-ul = ЧМ 2021: Is Gagarin Good? против Medium to Large Maxx
preconstructed_worlds-2021-info = Во втором онлайн Чемпионате мира по Netrunner, проведённом Project NISEI в 2021 году, принял участие 201 игрок. Из-за продолжающихся осложнений пандемии COVID-19 турнир был проведён онлайн на jinteki.net и состоял из 8 раундов по швейцарской системе, разбитых на 2 дня для двух разных групп игроков, переходящих в топ кат на 16 участников. Последний легальный набор: System Gateway.
preconstructed_worlds-2022-a = ЧМ 2022: Уильям Хвон (К) против skry (Б)
preconstructed_worlds-2022-a-tag = Уильям Хвон (К) против skry (Б)
preconstructed_worlds-2022-a-ul = ЧМ 2022: SNACS против Liberté, Égalité, Humidité
preconstructed_worlds-2022-b = ЧМ 2022: skry (К) против Уильяма Хвона (Б)
preconstructed_worlds-2022-b-tag = skry (К) против Уильяма Хвона (Б)
preconstructed_worlds-2022-b-ul = ЧМ 2022: Dies to Doom Blade против ApocoLat
preconstructed_worlds-2022-info = В 2022 году Null Signal Games, ранее известные как Project NISEI, провели первый Чемпионат мира по Netrunner оффлайн с начала пандемии COVID-19. В мероприятии приняли участие 158 игроков. Турнир проходил в Торонто, Канада, и состоял из 7 раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: Midnight Sun.
preconstructed_worlds-2023-a = ЧМ 2023: Уильям Хвон (К) против cableCarnage (Б)
preconstructed_worlds-2023-a-tag = Уильям Хвон (К) против cableCarnage (Б)
preconstructed_worlds-2023-a-ul = ЧМ 2023: The Worlds Grind против sableCarnage
preconstructed_worlds-2023-b = ЧМ 2023: cableCarnage (К) против Уильяма Хвона (Б)
preconstructed_worlds-2023-b-tag = cableCarnage (К) против Уильяма Хвона (Б)
preconstructed_worlds-2023-b-ul = ЧМ 2023: tableCarnage против You *do* always come back!
preconstructed_worlds-2023-info = Во втором Чемпионате мира по Netrunner под крылом Null Signal Games приняли участие 254 игрока. Турнир проходил в Барселоне, Испания, и состоял из 9 раундов по швейцарской системе, переходящих в топ кат на 16 участников. Последний легальный набор: The Automata Initiative.

pronouns = {$pronoun ->
    [any] Любые
    [blank] [пусто]
    [ey] undefined
    [faefaer] undefined
    [he] Он/его
    [heit] undefined
    [heshe] undefined
    [hethey] Он/они
    [it] Оно
    [myodb] Не хочу указывать
    [ne] undefined
    *[none] Не указаны
    [she] Она/её
    [sheit] undefined
    [shethey] Она/они
    [they] Они/их
    [ve] undefined
    [xe] undefined
    [xi] undefined
    [zehir] undefined
    [zezir] undefined
}

set_name = {$name ->
    [a23-seconds] 23 Seconds
    [a-study-in-static] undefined
    [all] Все
    [all-that-remains] undefined
    [alt-art] Альтернативные арты
    [alternate] Альтернативные
    [ashes-cycle] undefined
    [blood-and-water] undefined
    [blood-money] undefined
    [borealis-cycle] undefined
    [breaker-bay] undefined
    [business-first] undefined
    [championship-2019] Championship 2019
    [championship-2020] Championship 2020
    [chrome-city] undefined
    [core-set] undefined
    [council-of-the-crest] undefined
    [creation-and-control] undefined
    [crimson-dust] undefined
    [cyber-exodus] undefined
    [daedalus-complex] undefined
    [data-and-destiny] undefined
    [democracy-and-dogma] undefined
    [double-time] undefined
    [down-the-white-nile] undefined
    [downfall] undefined
    [draft] undefined
    [draft-cycle] undefined
    [earth-s-scion] undefined
    [escalation] undefined
    [fear-and-loathing] undefined
    [fear-the-masses] undefined
    [first-contact] undefined
    [flashpoint-cycle] undefined
    [free-mars] undefined
    [future-proof] undefined
    [genesis-cycle] undefined
    [gnk-2019] GNK 2019
    [honor-and-profit] undefined
    [humanity-s-shadow] undefined
    [intervention] undefined
    [kala-ghoda] undefined
    [kampala-ascendent] undefined
    [kitara-cycle] undefined
    [kysra-alt-arts] Альт-арты от Kysra
    [liberation-cycle] undefined
    [lunar-cycle] undefined
    [magnum-opus] undefined
    [magnum-opus-reprint] undefined
    [mala-tempora] undefined
    [martial-law] undefined
    [midnight-sun] undefined
    [midnight-sun-booster-pack] undefined
    [mumbad-cycle] undefined
    [napd-multiplayer] undefined
    [ntscape-navigator-alt-arts] Альт-арты от Ntscape Navigator
    [old-hollywood] undefined
    [opening-moves] undefined
    [order-and-chaos] undefined
    [parhelion] undefined
    [plural-and-miniplural-alt-arts] Альт-арты от Plural и MiniPlural
    [previous-versions] Предыдущие версии
    [quorum] undefined
    [rebellion-without-rehearsal] undefined
    [red-sand-cycle] undefined
    [reign-and-reverie] undefined
    [revised-core-set] undefined
    [salsette-island] undefined
    [salvaged-memories] undefined
    [sansan-cycle] undefined
    [second-thoughts] undefined
    [signed-championship-2020] Signed Championship 2020
    [sovereign-sight] undefined
    [spin-cycle] undefined
    [station-one] undefined
    [system-core-2019] System Core 2019
    [system-gateway] Врата Системы
    [system-update-2021] Обновление Системы 2021
    [terminal-directive-campaign] undefined
    [terminal-directive-cards] undefined
    [terminal-directive-cycle] undefined
    [the-automata-initiative] undefined
    [the-devil-and-the-dragon] undefined
    [the-liberated-mind] undefined
    [the-source] undefined
    [the-spaces-between] undefined
    [the-underway] undefined
    [the-universe-of-tomorrow] undefined
    [the-valley] undefined
    [trace-amount] undefined
    [true-colors] undefined
    [unreleased] Не выпущенные
    [up-and-over] undefined
    [uprising] undefined
    [uprising-booster-pack] undefined
    [upstalk] undefined
    [what-lies-ahead] undefined
    [whispers-in-nalubaale] undefined
    [world-champion-2015] Чемпион мира 2015
    [world-champion-2016] Чемпион мира 2016
    [world-champion-2017] Чемпион мира 2017
    *[unknown] undefined
}

settings_alt-art = Альтернативные изображения карт
settings_always = Всегда
settings_api-keys = Ключи API
settings_avatar = Аватар
settings_background = Фон игрового поля
settings_bespoke-sounds = {$sound ->
    [bespoke-sounds-header] Звуки для отдельных карт
    [archer] Лучник
    [end-of-the-line] undefined
    [harmonics] Гармонические льды (Bloop, Echo, Pulse, Wave)
    *[unknown] undefined
}
settings_bg = {$slug ->
    [custom-bg] Индивидуальный фон
    [monochrome-bg] Монотонный
    [worlds2020-bg] Чемпионат мира 2020
    *[unknown] undefined
}
settings_block = Заблокировать пользователя
settings_blocked = Заблокированные пользователи
settings_cancel = Отмена
settings_card-backs = Рубашки карт
settings_card-iamge = Показывать изображение карты
settings_card-images = Изображения карт
settings_card-preview-zoom = Увеличение карт при наведении
settings_card-text = Показывать текст карты
settings_change-avatar = Изменить на gravatar.com
settings_change-email = Изменить адрес электронной почты
settings_comp-only = Только в соревновательном лобби
settings_connection = undefined
settings_create-api-key = Создать ключ API
settings_current-email = Текущий адрес
settings_deck-stats = Учёт статистики колод
settings_delete-api-key = Удалить
settings_desired-email = Новый адрес
settings_disable-websockets = undefined
settings_display-encounter-info = undefined
settings_email = Электронная почта
settings_email-placeholder = Адрес электронной почты
settings_email-title = Изменить адрес электронной почты
settings_enable-game-sounds = Включить звуки в игре
settings_enable-lobby-sounds = Включить звуки в лобби
settings_enter-valid = Пожалуйста, введите корректный адрес электронной почты
settings_ffg = undefined
settings_game-stats = Учёт статистики побед/поражений
settings_get-log-top = Записать текущую высоту журнала
settings_get-log-width = Записать текущую ширину журнала
settings_ghost-trojans = Отображать бледные дубликаты программ-троянов, установленных на карты-носители
settings_high-res = Включить загрузку изображений высокого разрешения
settings_invalid-email = Учётной записи с таким адресом электронной почты не существует
settings_invalid-password = Неправильный логин или пароль
settings_language = Язык
settings_layout-options = Настройки расположения
settings_log-player-highlight = Подсвечивать игроков в журнале
settings_log-player-highlight-none = Не подсвечивать
settings_log-player-highlight-red-blue = Корпорация: Синий / Бегущий: Красный
settings_log-size = Размер журнала
settings_log-timestamps = undefined
settings_none = Не учитывать
settings_nsg = undefined
settings_pin-zoom = Оставлять последнюю увеличенную карту на экране
settings_player-stats-icons = Использовать иконки в игровой статистике
settings_pronouns = Местоимения
settings_reset = Сбросить до официальных изображений
settings_runner-classic = Классическая раскладка Бегущего (Сверху вниз: Программы, Устройства, Ресурсы)
settings_runner-layout = Раскладка Бегущего при игре за Корпорацию
settings_runner-reverse = Обратная раскладка Бегущего (Сверху вниз: Ресурсы, Устройства, Программы)
settings_set = Изменить
settings_set-all = Изменить все карты на
settings_show-alt = Показывать альтернативные изображения карт
settings_sides-overlap = Карты Бегущего и Корпорации могут перекрывать друг друга
settings_sounds = Звуки
settings_stacked-cards = Складывать вместе одинаковые карты (по умолчанию включено)
settings_toggle-log-timestamps = undefined
settings_update = Сохранить
settings_update-profile = Сохранить настройки
settings_updated = Настройки сохранены. Пожалуйста, перезагрузите страницу браузера
settings_updating = Сохранение настроек...
settings_user-name = Имя пользователя
settings_volume = Громкость

side_name = {$side ->
    [all] Все
    [any-side] Обе стороны
    [corp] Корпорация
    [runner] Бегущий
    *[unknown] undefined
}

stats_all-games = Показать все игры
stats_cards-accessed = Карт в доступе
stats_cards-click = Взято карт за клики
stats_cards-drawn = Взято карт
stats_cards-rezzed = Развёрнуто карт
stats_cards-sabotaged = Сброшено карт от саботажа
stats_clear-stats = Очистить статистику
stats_clicks-gained = Получено кликов
stats_completed = Завершено
stats_corp-stats = Статистика за Корпорацию
stats_credits-click = Получено кредитов за клики
stats_credits-gained = Получено кредитов
stats_credits-spent = Потрачено кредитов
stats_damage-done = Нанесено урона
stats_download = Скачать запись
stats_ended = Закончено
stats_events-played = Разыграно событий
stats_format = Формат
stats_game-stats = Игровая статистика
stats_game-title = {$title} ({$cnt ->
    *[one] {$cnt} ход
    [few] {$cnt} хода
    [many] {$cnt} ходов
    [other] {$cnt} ходов
})
stats_launch = Запустить запись
stats_lobby = Лобби
stats_log-count = {$cnt ->
    *[one] {$cnt} журнал
    [few] {$cnt} журнала
    [many] {$cnt} журналов
    [other] {$cnt} журналов
}
stats_log-count-filtered = {$cnt ->
    *[one] {$cnt} журнал (отфильтровано)
    [few] {$cnt} журнала (отфильтровано)
    [many] {$cnt} журналов (отфильтровано)
    [other] {$cnt} журналов (отфильтровано)
}
stats_lost = Проиграно
stats_no-games = Нет игр
stats_no-log = Журнал недоступен
stats_not-completed = Не завершено
stats_operations-played = Разыграно операций
stats_psi-game-total = Пси-игры: Сыграно игр
stats_psi-game-total-bid-0 = Пси-игры: Ставка 0
stats_psi-game-total-bid-1 = Пси-игры: Ставка 1
stats_psi-game-total-bid-2 = Пси-игры: Ставка 2
stats_psi-game-total-wins = Пси-игры: Побед
stats_rashida-count = Разыграно карт Rashida Jaheem
stats_replay-shared = undefined
stats_runner-stats = Статистика за Бегущего
stats_runs-made = Совершено забегов
stats_share = Поделиться записью
stats_shared-games = Показать игры с публичными записями
stats_shuffle-count = Перемешиваний колоды
stats_started = Начато
stats_tags-gained = Получено меток
stats_unavailable = Запись недоступна
stats_unique-accesses = Уникальных карт в доступе
stats_view-games = Вернуться к статистике
stats_view-log = Смотреть журнал
stats_win-method = Способ победы
stats_winner = Победитель
stats_won = Выиграно
