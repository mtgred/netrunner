annotations_available-annotations = 可用注释
annotations_clear = 清除本地注释
annotations_click-placeholder = 关于此点击的注释
annotations_import-local = 导入本地注释文件
annotations_load-local = 加载
annotations_no-published-annotations = 没有已发布的注释。
annotations_publish = 发布
annotations_save-local = 保存
annotations_turn-placeholder = 关于此回合的注释

card-browser_advancement = 推进需求
card-browser_agenda-points = 议案分数
card-browser_artist-info = 艺术家
card-browser_clear = 清除
card-browser_cost = 费用
card-browser_faction = 派系
card-browser_format = 赛制
card-browser_implementation-note = 实施笔记
card-browser_inf-limit = 影响力上限
card-browser_influence = 影响力
card-browser_memory = 内存
card-browser_min-deck-size = 牌组张数下限
card-browser_more-info = 更多信息
card-browser_search-hint = 搜索卡牌
card-browser_select-art = 选择卡面
card-browser_selected-art = 已选卡面
card-browser_set = 系列
card-browser_side = 阵营
card-browser_sort = 排序方式
card-browser_strength = 强度
card-browser_trash-cost = 销毁费用
card-browser_type = 类别
card-browser_update-failure = 卡面更换失败
card-browser_update-success = 卡面已更换

card-browser_sort-by = {$by ->
    [cost] 费用
    [faction] 派系
    [influence] 影响力
    [name] 牌名
    [set-number] 系列编号
    [type] 类别
    *[unknown] undefined
}

card-type_name = {$type ->
    [agenda] 议案
    [all] 全部
    [asset] 资产
    [event] 事件
    [hardware] 硬件
    [ice] 防火墙
    [identity] 特性（ID）
    [operation] 事务
    [program] 程序
    [resource] 资源
    [upgrade] 升级
    *[unknown] undefined
}

chat_block = 屏蔽用户
chat_cancel = 取消
chat_channels = 频道
chat_delete = 删除消息
chat_delete-all = 删除该用户发送的所有消息
chat_length-exceeded = 超出长度
chat_message-blocked = 消息堵塞: {$reason-str}
chat_placeholder = 说点什么吧
chat_rate-exceeded = 超出速率
chat_send = 发送
chat_title = 在浏览器中玩《安卓纪元：矩阵潜袭》

deck-builder_add-cards = 添加卡牌
deck-builder_add-to-deck = 添加到牌组
deck-builder_agenda-points = 议案分数
deck-builder_cancel = 取消
deck-builder_card-name = 卡牌名称
deck-builder_card-count = undefined
deck-builder_clear-stats = 清除统计数据
deck-builder_completed = 完成
deck-builder_confirm-delete = 确认删除
deck-builder_copy = 复制
deck-builder_create-game = 创建对战
deck-builder_deck-copy-suffix = 复制
deck-builder_deck-count = {$cnt ->
    [zero] 没有牌组
    *[other] 套牌组
}
deck-builder_deck-count-filtered = {$cnt ->
    [zero] 没有牌组（过滤后）
    *[other] 套牌组（过滤后）
}
deck-builder_deck-name = 牌组名称
deck-builder_deck-notes = 牌组备注
deck-builder_deck-points = 牌组点数
deck-builder_decklist = 牌表
deck-builder_decklist-inst = （在此输入或粘贴牌表，系统会自动解析）
deck-builder_delete = 删除
deck-builder_edit = 编辑
deck-builder_format = 赛制
deck-builder_games = 局数
deck-builder_hash = 卡组hash（比赛用）
deck-builder_identity = 特性（ID）
deck-builder_illegal = 不可用
deck-builder_import = 导入
deck-builder_import-button = 导入牌组
deck-builder_import-placeholder = NRDB ID
deck-builder_import-title = 请输入NRDB上公开牌组的ID或URL
deck-builder_influence = 影响力
deck-builder_legal = 可用
deck-builder_loading-msg = 牌组加载中……
deck-builder_lost = 败北
deck-builder_max = 最多
deck-builder_min = 最少
deck-builder_min-deck-size = undefined
deck-builder_new-corp = 新建公司牌组
deck-builder_new-deck = 新建牌组
deck-builder_new-runner = 新建潜袭者牌组
deck-builder_notes = 备注
deck-builder_reset = 重置
deck-builder_save = 保存
deck-builder_why = 为什么？
deck-builder_won = 胜利

diagrams_run-timing_approach = 6.9.2: 接驳防火墙阶段
diagrams_run-timing_approach-a = 潜袭者接驳所在位置的防火墙。接驳事件结算
diagrams_run-timing_approach-b = 付费能力窗口。公司可以激活非防火墙卡牌和/或潜袭者接驳的防火墙。
diagrams_run-timing_approach-c = 如果接驳的防火墙已激活，继续到遭遇阶段（6.9.3）
diagrams_run-timing_approach-d = 否则，进入到移动阶段（6.9.4）
diagrams_run-timing_disclaimer = 为了清晰起见，该时序图已被简化。如需完整规则，请参阅“Null Signal Games”网站。
diagrams_run-timing_encounter = 6.9.3: 遭遇防火墙阶段
diagrams_run-timing_encounter-a = 潜袭者遭遇所在位置的防火墙。遭遇事件结算
diagrams_run-timing_encounter-b = 付费能力窗口。可以与所遭遇的防火墙进行交互。
diagrams_run-timing_encounter-c = 如果有需要结算的未破解子进程，公司会至上而下依次结算未破解子进程。
diagrams_run-timing_encounter-d = 遭遇完成。进入到移动阶段（6.9.4）
diagrams_run-timing_header = 潜袭时序图
diagrams_run-timing_initiation = 6.9.1: 发起阶段
diagrams_run-timing_initiation-a = 潜袭者宣布攻击的服务器
diagrams_run-timing_initiation-b = 潜袭者获得负面声誉信用点
diagrams_run-timing_initiation-c = 潜袭正式开始 —— 潜袭事件结算
diagrams_run-timing_initiation-d = 如可能，接驳最外层的防火墙，并开始接驳阶段（6.9.2）
diagrams_run-timing_initiation-e = 否则，进入到移动阶段（6.9.4）
diagrams_run-timing_movement = 6.9.4: 移动阶段
diagrams_run-timing_movement-a = 如果潜袭者从接驳阶段或遭遇阶段到达此阶段，则通过防火墙。通过防火墙事件结算
diagrams_run-timing_movement-b = 如果潜袭者与服务器之间没有防火墙，‘当你通过服务器上的所有防火墙’事件结算
diagrams_run-timing_movement-c = 付费能力窗口
diagrams_run-timing_movement-d = 潜袭者可以退出。如这样做, 进入到潜袭结束阶段（6.9.6）
diagrams_run-timing_movement-e = 如可能，潜袭者向内移动1位置
diagrams_run-timing_movement-f = 付费能力窗口。公司可以激活非防火墙卡牌
diagrams_run-timing_movement-g = 如果潜袭者接驳另一个防火墙，返回到接驳阶段（6.9.2）
diagrams_run-timing_movement-h = 潜袭者接驳攻击的服务器。接驳事件结算
diagrams_run-timing_movement-i = 继续到成功阶段（6.9.5）
diagrams_run-timing_run-ends = 6.9.6: 潜袭结束阶段
diagrams_run-timing_run-ends-a = 任何打开的优先权窗口结算或关闭
diagrams_run-timing_run-ends-b = 潜袭者失去未花费的负面声誉信用
diagrams_run-timing_run-ends-c = 如果未达到成功阶段且攻击的服务器仍然存在，潜袭声明为失败
diagrams_run-timing_run-ends-d = 潜袭结束。潜袭结束事件结算
diagrams_run-timing_success = 6.9.5: 成功阶段
diagrams_run-timing_success-a = 此潜袭声明为成功。潜袭成功事件结算
diagrams_run-timing_success-b = 潜袭者侵入服务器
diagrams_run-timing_success-c = 成功阶段完成。继续到潜袭结束阶段（6.9.6）
diagrams_turn_corp-action-phase = 5.6.2: 行动阶段
diagrams_turn_corp-action-phase-a = 付费能力窗口。公司可以激活非防火墙卡牌和/或计分议案
diagrams_turn_corp-action-phase-b = 如果公司有未使用的[Clicks]，进行一个行动
diagrams_turn_corp-action-phase-c = 如果一个行动已发生，返回到（a）
diagrams_turn_corp-action-phase-d = 行动阶段完成。进入到弃牌阶段（5.6.3）
diagrams_turn_corp-discard-phase = 5.6.3: 弃牌阶段
diagrams_turn_corp-discard-phase-a = 如可能，公司弃牌至手牌上限
diagrams_turn_corp-discard-phase-b = 付费能力窗口。公司可以激活非防火墙卡牌
diagrams_turn_corp-discard-phase-c = 如果公司有任何[Clicks]剩余，则失去这些[Clicks]
diagrams_turn_corp-discard-phase-d = 公司回合正式结束。回合结束触发效果结算
diagrams_turn_corp-discard-phase-e = 进入到潜袭者回合
diagrams_turn_corp-draw-phase = 5.6.1: 抽牌阶段
diagrams_turn_corp-draw-phase-a = 公司获得可用时点（默认: [click][click][click]）
diagrams_turn_corp-draw-phase-b = 付费能力窗口。公司可以激活非防火墙卡牌和/或计分议案
diagrams_turn_corp-draw-phase-c = 公司填充可再生信用点
diagrams_turn_corp-draw-phase-d = 回合正式开始。回合开始事件结算
diagrams_turn_corp-draw-phase-e = 公司执行强制抽牌
diagrams_turn_corp-draw-phase-f = 进入到行动阶段（5.6.2）
diagrams_turn_corp-turn = 公司回合
diagrams_turn_runner-action-phase = 5.7.1: 行动阶段
diagrams_turn_runner-action-phase-a = 潜袭者获得可用时点（默认: [click][click][click][click]）
diagrams_turn_runner-action-phase-b = 付费能力窗口。公司可以激活非防火墙卡牌
diagrams_turn_runner-action-phase-c = 潜袭者填充可再生信用点
diagrams_turn_runner-action-phase-d = 回合正式开始。回合开始事件结算
diagrams_turn_runner-action-phase-e = 付费能力窗口。公司可以激活非防火墙卡牌
diagrams_turn_runner-action-phase-f = 如果潜袭者有未使用的[Clicks]，进行一个行动
diagrams_turn_runner-action-phase-g = 如果一个行动已发生，返回到（e）
diagrams_turn_runner-action-phase-h = 行动阶段完成。进入到弃牌阶段（5.7.2）
diagrams_turn_runner-discard-phase = 5.7.2: 弃牌阶段
diagrams_turn_runner-discard-phase-a = 如可能，潜袭者弃牌至手牌上限
diagrams_turn_runner-discard-phase-b = 付费能力窗口。公司可以激活非防火墙卡牌
diagrams_turn_runner-discard-phase-c = 如果潜袭者有任何[Clicks]剩余，则失去这些[Clicks]
diagrams_turn_runner-discard-phase-d = 潜袭者回合正式结束。回合结束触发效果结算
diagrams_turn_runner-discard-phase-e = 进入到公司回合
diagrams_turn_runner-turn = 潜袭者回合

faction_name = {$faction ->
    [adam] 亚当
    [all] 全部
    [anarch] 反叛者
    [any-faction] 任意派系
    [apex] 尖峰
    [criminal] 逆法者
    [haas-bioroid] 哈斯生化
    [jinteki] 人间会社
    [nbn] 网际传媒
    [neutral] 中立
    [shaper] 塑造者
    [sunny-lebeau] 桑妮·勒博
    [weyland-consortium] 威兰财团
    *[unknown] undefined
}

format_name = {$format ->
    [all] 全部
    [any-format] 任意赛制
    [casual] 休闲
    [classic] 经典
    [core-experience] 核心体验
    [eternal] 永久
    [neo] Neo
    [preconstructed] 预构筑
    [snapshot] 快照
    [snapshot-plus] 快照+
    [socr] SOCR
    [standard] 标准
    [startup] 新启
    [sunset] 落日
    [system-gateway] 核心网关
    [throwback] undefined
    *[unknown] undefined
}

game_abilities = 能力
game_actions = 操作
game_agenda-count = agenda-point 议案分数
game_agenda-count-with-req = undefined
game_approach-ice = 接驳防火墙
game_archives = 档案库{"\u00A0"}({$faceup} ↑ {$facedown} ↓)
game_attempt-reconnect = 尝试重新连接
game_auto-pass = 自动让过优先权
game_bad-pub-count = {$base} 负面声誉
game_bad-pub-count-additional = {$base} 负面声誉 + {$additional} 负面声誉
game_beat-trace = 击败追踪
game_brain-damage = 脑部伤害
game_breach-server = 侵入服务器
game_card = 卡牌
game_card-count = {$size} 张卡牌。
game_click-count = {$click} 时点
game_close = 关闭
game_close-shuffle = 关闭并洗牌
game_concede = 投降
game_continue = 继续
game_continue-to = 继续：
game_corp-view = 公司视图
game_credit-count = {$credit} 信用点
game_credit-count-with-run-credits = {$credit} 信用点（{$run-credit} 个本次潜袭专用）
game_credits = 个信用点
game_current = 局势
game_current-phase = 当前阶段
game_draw = 抽牌
game_encounter-ice = 遭遇防火墙
game_end-turn = 结束回合
game_error = 内部服务器错误。请在聊天中键入/bug并按照说明操作。
game_face-down-count = {$total}张卡牌，{$facedown}张牌面朝下。
game_fire-unbroken = 结算未破解的子进程
game_gain-credit = 获得信用点
game_game-start = 对战开始时间
game_grip = 操控器
game_heap = 堆阵
game_hq = 总部
game_inactivity = 对战因长期无活动而关闭
game_indicate-action = 我要响应
game_initiation = 发起潜袭
game_jack-out = 退出潜袭
game_keep = 保留
game_last-revealed = undefined
game_leave = 离开对战
game_leave-replay = 关闭回放
game_let-subs-fire = 让公司结算子进程
game_link-strength = 中转强度
game_lost-connection = 与服务器的连接中断。重新连接。
game_mandatory-draw = 回合开始抽牌
game_max-hand = 手牌上限
game_minutes = 分:
game_movement = 移动
game_mu-count = {$unused} / {$available} 空闲内存
game_mulligan = 调度
game_mute = 禁止旁观者发言
game_no-current-run = 当前无潜袭进行中
game_no-further = 没有响应
game_ok = OK
game_play-area = 出牌区
game_purge = 清除病毒指示物
game_reconnected-to-server = 已重新连接到服务器
game_remove-tag = 移除锁定标记
game_reveal-my-hand = undefined
game_rez = 激活
game_rez-all = undefined
game_rfg = 移出游戏
game_rnd = 研发中心
game_run = 潜袭
game_run-ends = 潜袭结束
game_runner-view = 潜袭者视图
game_scored-area = 计分区
game_seconds = 秒
game_seconds-remaining = 秒剩余
game_server = 服务器
game_set-aside = 设置一旁
game_show = 显示
game_show-decklists = undefined
game_shuffle = 洗牌
game_spec-count = {$cnt} 位观众
game_spec-view = 旁观者视图
game_special-mu-count = {$unused} / {$available} {$mu-type} 空闲内存
game_stack = 存储栈
game_start = 开始对战
game_start-turn = 开始回合
game_stop-auto-pass = 取消自动让过优先权
game_subs = 子进程
game_success = 成功
game_tag-count = {$base} 锁定标记
game_tag-count-additional = {$base} + {$additional} 锁定标记
game_take-clicks = 进入行动阶段
game_time-taken = 对战用时：{$time}分钟
game_timeout-soon = undefined
game_trace = 追踪强度
game_trash-like-cards = undefined
game_trash-resource = 销毁资源
game_unbeatable = 使不可击败
game_unimplemented = 功能未实现
game_unknown-phase = 未知阶段
game_unmute = 允许旁观者发言
game_win-claimed = {$winner} ({$side})于第{$turn}回合因声明而获胜
game_win-conceded = {$winner} ({$side})于第{$turn}回合因对手投降而获胜
game_win-decked = {$winner} ({$side})于第{$turn}回合因公司无牌可抽而获胜
game_win-flatlined = {$winner} ({$side})于第{$turn}回合通过杀死潜袭者而获胜
game_win-other = {$winner} ({$side})于第{$turn}回合因{$reason}而获胜
game_win-points = {$winner} ({$side})于第{$turn}回合因议案分数而获胜

game_prompt = {$msg ->
    [advance] 推进
    [archives] 档案库
    [derez] 关闭
    [expend] 消耗
    [hq] 总部
    [new-remote] 新远程
    [r-d] 研发中心
    [rez] 激活
    [score] 计分
    [server-1] 服务器 1
    [server-10] 服务器 10
    [server-2] 服务器 2
    [server-3] 服务器 3
    [server-4] 服务器 4
    [server-5] 服务器 5
    [server-6] 服务器 6
    [server-7] 服务器 7
    [server-8] 服务器 8
    [server-9] 服务器 9
    [trash] 销毁
    *[unknown] undefined
}

ingame-settings_alt-art = 异画卡
ingame-settings_board-overlap = 面板重叠
ingame-settings_card-backs = 卡背
ingame-settings_card-image = 卡牌图像
ingame-settings_card-images = 卡牌图像
ingame-settings_card-sorting = 排序
ingame-settings_card-stacking = 卡牌设置
ingame-settings_card-text = 卡牌文本
ingame-settings_display-encounter-info = undefined
ingame-settings_game-settings = undefined
ingame-settings_ghost-trojans = 对于被负载的程序显示鬼影
ingame-settings_high-res = 启用高分辨率卡牌图像
ingame-settings_label-faceup-cards = 贴标签于面朝上卡牌
ingame-settings_label-unrezzed-cards = 贴标签于未激活卡牌
ingame-settings_log-timestamps = undefined
ingame-settings_pass-on-rez = undefined
ingame-settings_preview-zoom = 卡牌预览缩放
ingame-settings_runner-board-order = 潜袭者面板顺序
ingame-settings_runner-classic = 经典
ingame-settings_runner-reverse = 反转
ingame-settings_save = 保存
ingame-settings_show-alt = 显示异画卡
ingame-settings_sides-overlap = 潜袭者和公司面板可以重叠
ingame-settings_sort-archives = 排序档案库
ingame-settings_sort-heap = 排序堆阵
ingame-settings_stack-cards = 启用服务器堆叠
ingame-settings_log-timestamps-toggle = undefined

lobby_aborted = 连接已中断
lobby_api-access = 允许 API 访问游戏信息
lobby_api-access-details = 这允许第三方扩展访问有关你游戏的信息。需要在“设置”中创建API密钥
lobby_api-requires-key = (需要设置 API 密钥)
lobby_as-corp = 作为公司
lobby_as-runner = 作为潜袭者
lobby_both-perspective = 双方视角
lobby_cancel = 取消
lobby_chat = 聊天
lobby_closed-msg = 房间因长期无活动而关闭
lobby_completion-rate = 游戏完成率
lobby_corp-perspective = 公司视角
lobby_create = 创建
lobby_deck-selected = 牌组已选择
lobby_default-game-format = 默认游戏赛制
lobby_delete = 删除房间
lobby_filter = 过滤
lobby_format = 赛制
lobby_game-count = {$cnt} 局对战
lobby_game-count-filtered = {$cnt} 局对战（过滤后）
lobby_hidden = 允许旁观者查看玩家的隐藏信息
lobby_hidden-details = 这将向游戏的所有观众揭示两名玩家的隐藏信息，包括手牌和面朝下的牌。
lobby_hidden-password = 我们建议使用密码来防止陌生人破坏游戏。
lobby_invalid-password = 密码不正确
lobby_join = 加入
lobby_leave = 离开
lobby_load-replay = 加载录像
lobby_new-game = 新建对战
lobby_no-games = 当前无对战
lobby_not-allowed = 操作被禁止
lobby_open-decklists = undefined
lobby_open-decklists-b = undefined
lobby_options = 选项
lobby_password = 密码
lobby_password-error = 请设置密码。
lobby_password-for = 请输入房间密码：
lobby_password-protected = 密码保护
lobby_players = 玩家
lobby_private = 私密
lobby_rejoin = 重新加入
lobby_reload = 刷新列表
lobby_replay-invalid-file = 请选择有效的录像文件。
lobby_replay-link-error = 录像链接无效。
lobby_reset = 重置房间名
lobby_runner-perspective = 潜袭者视角
lobby_save-replay = 保存录像
lobby_save-replay-beta = 测试功能：请注意，我们可能会重置已保存的录像，因此请确保下载你想要保留的游戏。此外，请记住，我们可能会在未来对网站进行更改，这可能会使回放不兼容。
lobby_save-replay-details = 这将保存此游戏的回放文件，其中包含公开信息（例如公开手牌）。该文件仅在游戏结束后可用。
lobby_save-replay-unshared = 只会保留你最近的15个未分享游戏，因此请务必在之后下载或分享游戏。
lobby_select-deck = 选择牌组
lobby_select-error = 不能选择那个牌组
lobby_select-title = 请选择牌组
lobby_side = 阵营
lobby_singleton = 单张卡牌
lobby_singleton-b = (单张卡牌)
lobby_singleton-details = 这将限制牌组中每种同名卡牌仅限1张。建议你使用下列基于此模式的特性ID组建的牌组。
lobby_singleton-example = 1) 复始：触媒&促力 2) 安培：全民的义体
lobby_singleton-restriction = 这局要求为单张卡牌模式。这意味着牌组中每种同名卡牌限1张。
lobby_spectator-count = {$cnt} 位观众
lobby_spectators = 允许旁观
lobby_start = 开始
lobby_start-replay = 开始回放
lobby_swap = 交换阵营
lobby_timed-game = 带有计时器
lobby_timed-game-details = 计时器仅为方便使用：当计时器结束时，游戏不会停止。
lobby_timer-length = 计时器长度（分钟）
lobby_title = 房间名
lobby_title-error = 请设置房间名。
lobby_too-little-data = 数据不足
lobby_waiting = 等待玩家选择牌组
lobby_watch = 观战

lobby_type = {$type ->
    [casual] 休闲
    [competitive] 竞技
    [angel-arena] 天使竞技场
    [tournament] 比赛
    *[unknown] undefined
}

lobby_gateway-format = {$format ->
    [beginner] 初学者
    [beginner-info] 此大厅为公司和潜袭者使用核心网关初学者牌组而准备。建议你在初次游戏中使用这些牌组。游戏按6点议案分数进行。
    [beginner-ul] 核心网关 - 初学者教学牌组
    [constructed] 构筑
    [intermediate] 进阶
    [intermediate-info] 此大厅为公司和潜袭者使用核心网关进阶牌组而准备。这些牌组比初学者牌组稍大。游戏按7点议案分数进行。
    [intermediate-ul] 核心网关 - 进阶教学牌组
    *[unknown] undefined
}

log_annotating = 注释
log_game-log = 游戏日志
log_remote-annotations-fail = 无法获取远程注释。
log_run-timing = 潜袭时序
log_settings = 设置
log_shared = 共享注释
log_turn-timing = 回合时序

menu_admin = 网站管理员
menu_donor = 捐赠人
menu_logout = 退出
menu_moderator = 管理员
menu_settings =  { nav_settings }

missing = :zh-simp missing text

nav_about = 关于
nav_admin = 网站管理
nav_cards = 卡牌
nav_chat = 聊天
nav_deck-builder = 牌组构筑
nav_features = 站点功能
nav_game-count = {$cnt} 局对战
nav_help = 帮助
nav_play = 对战
nav_settings = 设置
nav_stats = 统计
nav_tournaments = 比赛
nav_users = 用户
nav_welcome = 欢迎

preconstructed_worlds-2012-a = Worlds 2012: Ben Marsh (C) vs. Jeremy Zwirn (R)
preconstructed_worlds-2012-a-tag = Ben Marsh (C) vs. Jeremy Zwirn (R)
preconstructed_worlds-2012-a-ul = Worlds 2012: Weyland vs. Criminal
preconstructed_worlds-2012-b = Worlds 2012: Jeremy Zwirn (C) vs. Ben Marsh (R)
preconstructed_worlds-2012-b-tag = Jeremy Zwirn (C) vs. Ben Marsh (R)
preconstructed_worlds-2012-b-ul = Worlds 2012: Haas-Bioroid vs. Criminal
preconstructed_worlds-2012-info = Worlds 2012 was played with (up to 3 copies of) the Core Set as the only legal product. Jeremy Zwirn (Building a Better World, Gabriel Santiago) took first place against Ben Marsh (Engineering the Future, Gabriel Santiago) in the first ever Netrunner World Championship.
preconstructed_worlds-2013-a = Worlds 2013: Jens Erickson (C) vs. Andrew Veen (R)
preconstructed_worlds-2013-a-tag = Jens Erickson (C) vs. Andrew Veen (R)
preconstructed_worlds-2013-a-ul = Worlds 2013: HB FastAdv vs. Shaper Katman
preconstructed_worlds-2013-b = Worlds 2013: Andrew Veen (C) vs. Jens Erickson (R)
preconstructed_worlds-2013-b-tag = Andrew Veen (C) vs. Jens Erickson (R)
preconstructed_worlds-2013-b-ul = Worlds 2013: NBN Fast Adv vs. Andy Sucker
preconstructed_worlds-2013-info = 166 players attended worlds in 2013. The tournament was held in Minneapolis, MN, USA, and consisted of 6 swiss rounds into a top 32 cut. The legal cardpool consisted of cards up to Opening Moves.
preconstructed_worlds-2014-a = Worlds 2014: Dan D'Argenio (C) vs. Minh Tran (R)
preconstructed_worlds-2014-a-tag = Dan D'Argenio (C) vs. Minh Tran (R)
preconstructed_worlds-2014-a-ul = Worlds 2014: Honor and Perfection vs. Andromedium
preconstructed_worlds-2014-b = Worlds 2014: Minh Tran (C) vs. Dan D'Argenio (R)
preconstructed_worlds-2014-b-tag = Minh Tran (C) vs. Dan D'Argenio (R)
preconstructed_worlds-2014-b-ul = Worlds 2014: Personal Evolution vs. Daily QT Andy
preconstructed_worlds-2014-info = 238 players attended worlds in 2014. The tournament was held in Minneapolis, MN, USA, and consisted of 7 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Up and Over.
preconstructed_worlds-2015-a = Worlds 2015: Dan D'Argenio (C) vs. Timmy Wong (R)
preconstructed_worlds-2015-a-tag = Dan D'Argenio (C) vs. Timmy Wong (R)
preconstructed_worlds-2015-a-ul = Worlds 2015: Foodcoatshop vs. The Morning After
preconstructed_worlds-2015-b = Worlds 2015: Timmy Wong (C) vs. Dan D'Argenio (R)
preconstructed_worlds-2015-b-tag = Dan D'Argenio (C) vs. Timmy Wong (R)
preconstructed_worlds-2015-b-ul = Worlds 2015: Yellow Shell vs. Radisson Cheese Plate
preconstructed_worlds-2015-info = 269 players attended worlds in 2015. The tournament was held in Minneapolis, MN, USA, and consisted of 8 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Data and Destiny.
preconstructed_worlds-2016-a = Worlds 2016: Chris Dyer (C) vs. Benjamin Ni (R)
preconstructed_worlds-2016-a-tag = Chris Dyer (C) vs. Benjamin Ni (R)
preconstructed_worlds-2016-a-ul = Worlds 2016: Snekbite vs. Minh MaxX++
preconstructed_worlds-2016-b = Worlds 2016: Benjamin Ni (R) vs. Chris Dyer (C)
preconstructed_worlds-2016-b-tag = Benjamin Ni (R) vs. Chris Dyer (C)
preconstructed_worlds-2016-b-ul = Worlds 2016: Fiery Info vs. Papa Smurf
preconstructed_worlds-2016-info = 278 players attended worlds in 2016. The tournament was held in Minneapolis, MN, USA, and consisted of 9 swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Escalation.
preconstructed_worlds-2017-a = Worlds 2017: Jess Horig (C) vs. Grey Tongue (R)
preconstructed_worlds-2017-a-tag = Jess Horig (C) vs. Grey Tongue (R)
preconstructed_worlds-2017-a-ul = Worlds 2017: Stinson Reversed CI vs. Aesops Hayley
preconstructed_worlds-2017-b = Worlds 2017: Grey Tongue (C) vs. Jess Horig (R)
preconstructed_worlds-2017-b-tag = Grey Tongue (C) vs. Jess Horig (R)
preconstructed_worlds-2017-b-ul = Worlds 2017: No-Show Rewiring CI vs. Laguna Lock Hayley
preconstructed_worlds-2017-info = 233 players attended worlds in 2017. The tournament was held in Minneapolis, MN, USA, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Revised Core set.
preconstructed_worlds-2018-a = Worlds 2018: Joe Schupp (C) vs. Chris Dyer (R)
preconstructed_worlds-2018-a-tag = Joe Schupp (C) vs. Chris Dyer (R)
preconstructed_worlds-2018-a-ul = Worlds 2018: AMERICA CtM vs. Gooseberry MaxX
preconstructed_worlds-2018-b = Worlds 2018: Chris Dyer (C) vs. Joe Schupp (R)
preconstructed_worlds-2018-b-tag = Chris Dyer (C) vs. Joe Schupp (R)
preconstructed_worlds-2018-b-ul = Worlds 2018: Trust the Process vs. Dan D'Argenio KoS Val
preconstructed_worlds-2018-info = 403(!) players attended worlds in 2018. This is the final worlds championship to be run by FFG. The tournament was held in Minneapolis, MN, USA, and consisted of 9(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to Reign and Reverie
preconstructed_worlds-2019-a = Worlds 2019: Pinsel (C) vs. Testrunning (R)
preconstructed_worlds-2019-a-tag = Pinsel (C) vs. Testrunning (R)
preconstructed_worlds-2019-a-ul = Worlds 2019: Fully dedicated to efficiency vs. Trash Panda
preconstructed_worlds-2019-b = Worlds 2019: Testrunning (C) vs. Pinsel (R)
preconstructed_worlds-2019-b-tag = Testrunning (C) vs. Pinsel (R)
preconstructed_worlds-2019-b-ul = Worlds 2019: 2 Grid for 2 Place vs. Trash Panda
preconstructed_worlds-2019-info = 256 players played in the first even Project NISEI Netrunner World Championship in 2019. This tournament was held in Rotterdam, NL, and consisted of 8(?) swiss rounds into a top 16 cut. The legal cardpool consisted of cards up to the Uprising Booster Pack
preconstructed_worlds-2020-a = Worlds 2020: Limes (C) vs. tf34 (R)
preconstructed_worlds-2020-a-tag = Limes (C) vs. tf34 (R)
preconstructed_worlds-2020-a-ul = Worlds 2020: I don't like this deck vs. Engolo Freedom
preconstructed_worlds-2020-b = Worlds 2020: tf34 (R) vs. Limes (C)
preconstructed_worlds-2020-b-tag = tf34 (R) vs. Limes (C)
preconstructed_worlds-2020-b-ul = Worlds 2020: Malia CTM vs. Imp-pressive Hoshiko
preconstructed_worlds-2020-info = 294 players played in the first ever online world championship for Netrunner, run by Project NISEI 2020. Due to travel restrictions at the start of the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to Uprising.
preconstructed_worlds-2021-a = Worlds 2021: Patrick Gower (C) vs. Jonas (R)
preconstructed_worlds-2021-a-tag = Patrick Gower (C) vs. Jonas (R)
preconstructed_worlds-2021-a-ul = Worlds 2021: 44 Card PD vs. Watch Me Drip, Watch Me Maemi
preconstructed_worlds-2021-b = Worlds 2021: Jonas (C) vs. Patrick Gower (R)
preconstructed_worlds-2021-b-tag = Jonas (C) vs. Patrick Gower (R)
preconstructed_worlds-2021-b-ul = Worlds 2021: Is Gagarin Good? vs. Medium to Large Maxx
preconstructed_worlds-2021-info = 201 players played in the second online world championship for Netrunner, run by Project NISEI in 2021. Due to the ongoing disruption caused by the COVID-19 pandemic, this tournament was held online via Jinteki.net, and consisted of 8 swiss rounds on two distinct day-ones, into a top 16 cut. The legal cardpool consisted of cards up to System Gateway.
preconstructed_worlds-2022-a = Worlds 2022: William Huang (C) vs. skry (R)
preconstructed_worlds-2022-a-tag = William Huang (C) vs. skry (R)
preconstructed_worlds-2022-a-ul = Worlds 2022: SNACS vs. Liberté, Égalité, Humidité
preconstructed_worlds-2022-b = Worlds 2022: skry (C) vs. William Huang (R)
preconstructed_worlds-2022-b-tag = skry (C) vs. William Huang (R)
preconstructed_worlds-2022-b-ul = Worlds 2022: Dies to Doom Blade vs. ApocoLat
preconstructed_worlds-2022-info = 158 players played in the first world championship run by Null Signal Games (formerly Project NISEI), which was the first Netrunner world championship to be run in-person since the start of the COVID-19 pandemic. The tournament was held in Toronto, Canada, and consisted of 8(?) rounds into a top 16 cut. The legal cardpool consisted of cards up to Midnight Sun.
preconstructed_worlds-2023-a = Worlds 2023: William Huang (C) vs. cableCarnage (R)
preconstructed_worlds-2023-a-tag = William Huang (C) vs. cableCarnage (R)
preconstructed_worlds-2023-a-ul = Worlds 2023: The Worlds Grid vs. sableCarnage
preconstructed_worlds-2023-b = Worlds 2023: cableCarnage (C) vs. William Huang (R)
preconstructed_worlds-2023-b-tag = cableCarnage (C) vs. William Huang (R)
preconstructed_worlds-2023-b-ul = Worlds 2023: tableCarnage vs. You *do* always come back!
preconstructed_worlds-2023-info = 254 players played in the second Netrunner world championship run by Null Signal Games. The tournament was held in Barcelona, Spain, and consisted of 8 rounds into a top 16 cut. The legal cardpool consisted of cards up to The Automata Initiative.

pronouns = {$pronoun ->
    [any] 随意
    [blank] [空白]
    [ey] Ey/em
    [faefaer] undefined
    [he] 他（He/him）
    [heit] undefined
    [heshe] undefined
    [hethey] 他（He/they）
    [it] 它（It）
    [myodb] 不愿透露
    [ne] Ne/nem
    *[none] 未设定
    [she] 她（She/her）
    [sheit] undefined
    [shethey] 她（She/they）
    [they] They/them
    [ve] Ve/ver
    [xe] Xe/xem
    [xi] undefined
    [zehir] Ze/hir
    [zezir] Ze/zir
}

set_name = {$name ->
    [a23-seconds] 二十三秒
    [a-study-in-static] 静态研究
    [all] 全部
    [all-that-remains] 遗迹之地
    [alt-art] 异画系列
    [alternate] 替代
    [ashes-cycle] 余烬循环
    [blood-and-water] 水债血偿
    [blood-money] 不义之财
    [borealis-cycle] 北极光循环
    [breaker-bay] 碎浪湾
    [business-first] 商务为先
    [championship-2019] 冠军2019
    [championship-2020] 冠军2020
    [chrome-city] 铬金城
    [core-set] 核心系列
    [council-of-the-crest] 巅峰议会
    [creation-and-control] 创造与掌控
    [crimson-dust] 绯红之尘
    [cyber-exodus] 赛博迁徙
    [daedalus-complex] 代达罗斯
    [data-and-destiny] 数据与命运
    [democracy-and-dogma] 民主与教条
    [double-time] 双重时刻
    [down-the-white-nile] 白尼罗河畔
    [downfall] 坍落
    [draft] 轮抽系列
    [draft-cycle] 轮抽循环
    [earth-s-scion] 地球子孙
    [escalation] 事态升级
    [fear-and-loathing] 恐惧与憎恶
    [fear-the-masses] 群众可畏
    [first-contact] 初遇之际
    [flashpoint-cycle] 闪点循环
    [free-mars] 自由火星
    [future-proof] 未来考验
    [genesis-cycle] 创纪元循环
    [gnk-2019] GNK 2019
    [honor-and-profit] 荣誉与利益
    [humanity-s-shadow] 人性阴影
    [intervention] 介入冲突
    [kala-ghoda] 卡拉哥达
    [kampala-ascendent] 坎帕拉盛势
    [kitara-cycle] 基塔拉循环
    [kysra-alt-arts] Kysra 异画
    [liberation-cycle] 解放循环
    [lunar-cycle] 月行循环
    [magnum-opus] 巨作
    [magnum-opus-reprint] 巨作重印版
    [mala-tempora] 脑叶癫痫
    [martial-law] 军事管制
    [midnight-sun] 极昼
    [midnight-sun-booster-pack] 极昼推广包
    [mumbad-cycle] 孟巴德循环
    [napd-multiplayer] NAPD 多人
    [ntscape-navigator-alt-arts] Ntscape Navigator 异画
    [old-hollywood] 老莱坞
    [opening-moves] 起手开局
    [order-and-chaos] 秩序与混沌
    [parhelion] 幻日
    [plural-and-miniplural-alt-arts] Plural and MiniPlural 异画
    [previous-versions] 先前版本
    [quorum] 共商出路
    [rebellion-without-rehearsal] 即兴叛乱
    [red-sand-cycle] 红砂循环
    [reign-and-reverie] 统治与幻想
    [revised-core-set] 修订版核心系列
    [salsette-island] 撒尔塞特
    [salvaged-memories] 唤醒回忆
    [sansan-cycle] 圣加州循环
    [second-thoughts] 深思熟虑
    [signed-championship-2020] 签名版冠军2020
    [sovereign-sight] 主权之光
    [spin-cycle] 扭曲真相循环
    [station-one] 一号车站
    [system-core-2019] 系统核心2019
    [system-gateway] 核心网关
    [system-update-2021] 系统革新2021
    [terminal-directive-campaign] 终极指令战役
    [terminal-directive-cards] 终极指令卡牌
    [terminal-directive-cycle] 终极指令循环
    [the-automata-initiative] 自动机倡议
    [the-devil-and-the-dragon] 恶魔与龙
    [the-liberated-mind] 自由心智
    [the-source] 代码之源
    [the-spaces-between] 往来之隙
    [the-underway] 暗底区
    [the-universe-of-tomorrow] 未来域
    [the-valley] 生科谷
    [trace-amount] 重重追踪
    [true-colors] 真实面目
    [unreleased] 未发布
    [up-and-over] 攀越之路
    [uprising] 起义
    [uprising-booster-pack] 起义推广包
    [upstalk] 上行之夜
    [what-lies-ahead] 前途未卜
    [whispers-in-nalubaale] 纳鲁巴勒低语
    [world-champion-2015] 世界冠军2015
    [world-champion-2016] 世界冠军2016
    [world-champion-2017] 世界冠军2017
    *[unknown] undefined
}

settings_alt-art = 异画卡
settings_always = 总是
settings_api-keys = API密钥
settings_avatar = 头像
settings_background = 游戏背景
settings_block = 屏蔽
settings_blocked = 黑名单
settings_cancel = 取消
settings_card-backs = 卡背
settings_card-iamge = 卡牌图像
settings_card-images = 卡牌图像
settings_card-preview-zoom = 卡牌预览缩放
settings_card-text = 卡牌文本
settings_change-avatar = 在gravatar.com上更换
settings_change-email = 更换电子邮箱
settings_comp-only = 仅竞技厅
settings_connection = undefined
settings_create-api-key = 创建API密钥
settings_current-email = 旧邮箱
settings_deck-stats = 牌组统计
settings_delete-api-key = 删除
settings_desired-email = 新邮箱
settings_disable-websockets = undefined
settings_display-encounter-info = undefined
settings_email = 电子邮箱
settings_email-placeholder = 电子邮件地址
settings_email-title = 更换电子邮件地址
settings_enable-game-sounds = 开启游戏内音效
settings_enable-lobby-sounds = 开启大厅内音效
settings_enter-valid = 请输入合法的电子邮件地址
settings_ffg = FFG
settings_game-stats = 对战胜负统计
settings_get-log-top = 获取当前日志框顶部坐标
settings_get-log-width = 获取当前日志框宽度
settings_ghost-trojans = 对于被负载的程序显示鬼影
settings_high-res = 启用高分辨率卡牌图像
settings_invalid-email = 没有使用该邮箱地址的账号
settings_invalid-password = 用户名或密码无效
settings_language = 语言
settings_layout-options = 布局选项
settings_log-player-highlight = 日志玩家高亮
settings_log-player-highlight-none = 无
settings_log-player-highlight-red-blue = 公司：蓝色 / 潜袭者：红色
settings_log-size = 日志栏尺寸
settings_log-timestamps = undefined
settings_none = 关闭
settings_nsg = NSG
settings_pin-zoom = 在屏幕上保持缩放卡牌
settings_player-stats-icons = 使用图标显示玩家统计
settings_pronouns = 代词
settings_reset = 将所有卡牌重设为原始卡面
settings_runner-classic = 经典jnet布局（自上而下：程序、硬件、资源）
settings_runner-layout = 潜袭者布局（公司视角）
settings_runner-reverse = 反转布局（自上而下：资源、硬件、程序）
settings_set = 设置
settings_set-all = 将所有卡牌设为
settings_show-alt = 显示异画卡
settings_sides-overlap = 潜袭者和公司面板可以重叠
settings_sounds = 音效
settings_stacked-cards = 默认启用服务器堆叠
settings_toggle-log-timestamps = undefined
settings_update = 更换
settings_update-profile = 保存设置
settings_updated = 设置已保存——请刷新页面
settings_updating = 设置保存中……
settings_user-name = 用户名
settings_volume = 音量

settings_bg = {$slug ->
    [apex-bg] 尖峰
    [custom-bg] 自定义背景
    [find-the-truth-bg] 找出真相
    [freelancer-bg] 自由职业者
    [monochrome-bg] 纯黑
    [mushin-no-shin-bg] 无心之心
    [push-your-luck-bg] 豪赌一把
    [rumor-mill-bg] 谣言工厂
    [the-root-bg] 根基
    [traffic-jam-bg] 交通阻塞
    [worlds2020-bg] 世界2020
    *[unknown] undefined
}

settings_bespoke-sounds = {$sound ->
    [bespoke-sounds-header] 卡牌特定声音
    [archer] 射手
    [end-of-the-line] 穷途末路
    [harmonics] 和声组合（海洋怪声，回声，脉动，波动）
    *[unknown] undefined
}

side_name = {$side ->
    [all] 全部
    [any-side] 任意阵营
    [corp] 公司
    [runner] 潜袭者
    *[unknown] undefined
}

stats_all-games = 显示所有记录
stats_cards-accessed = 读取卡牌数量
stats_cards-click = 手动抽牌数量
stats_cards-drawn = 抽牌数量
stats_cards-rezzed = 激活卡牌数量
stats_cards-sabotaged = 破坏次数
stats_clear-stats = 清除统计数据
stats_clicks-gained = 获得时点数量
stats_completed = 完成
stats_corp-stats = 公司统计
stats_credits-click = 手动获取信用点数量
stats_credits-gained = 获得信用点数量
stats_credits-spent = 支付信用点数量
stats_damage-done = 造成伤害量
stats_download = 下载录像
stats_ended = 结束时间
stats_events-played = 事件打出数量
stats_format = 赛制
stats_game-stats = 胜负统计
stats_game-title = {$title} ({$cnt} 个回合)
stats_launch = 播放录像
stats_lobby = 大厅
stats_log-count = {$cnt} 条记录
stats_log-count-filtered = {$cnt} 条记录（过滤后）
stats_lost = 败北
stats_no-games = 没有对战记录
stats_no-log = 无日志信息
stats_not-completed = 未完成
stats_operations-played = 事务打出数量
stats_psi-game-total = 灵能赌博：赌博次数
stats_psi-game-total-bid-0 = 灵能赌博：出价0次数
stats_psi-game-total-bid-1 = 灵能赌博：出价1次数
stats_psi-game-total-bid-2 = 灵能赌博：出价2次数
stats_psi-game-total-wins = 灵能赌博：胜利次数
stats_rashida-count = 拉什达次数
stats_replay-shared = undefined
stats_runner-stats = 潜袭者统计
stats_runs-made = 潜袭次数
stats_share = 分享录像
stats_shared-games = 只显示已分享的录像
stats_shuffle-count = 洗牌次数
stats_started = 开始
stats_tags-gained = 获得锁定标记数量
stats_unavailable = 未保存录像
stats_unique-accesses = 读取独有卡牌数量
stats_view-games = 返回统计界面
stats_view-log = 查看记录
stats_win-method = 胜利方式
stats_winner = 胜者
stats_won = 胜利
