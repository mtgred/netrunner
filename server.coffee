crypto = require('crypto')
http = require('http')

express = require('express')
app = express()
server = http.createServer(app)

async = require('async')
bcrypt = require('bcrypt')
bodyParser = require('body-parser')
session = require('express-session')
MongoStore = require('connect-mongo')(session)
cookieParser = require('cookie-parser')
cors = require('cors')
favicon = require('serve-favicon')
jwt = require('jsonwebtoken')
methodOverride = require('method-override')
moment = require('moment')
cache = require('memory-cache')
mongoskin = require('mongoskin')
Trello = require('node-trello')
uuid = require('node-uuid')
nodemailer = require('nodemailer')
passport = require('passport')
LocalStrategy = require('passport-local').Strategy
io = require('socket.io')(server)
stylus = require('stylus')
zmq = require('zmq')

config = require('./config')

# MongoDB connection
appName = 'netrunner'
mongoUrl = process.env['MONGO_URL'] || "mongodb://127.0.0.1:27017/netrunner"
db = mongoskin.db(mongoUrl)

# Game lobby
games = {}
lobbyUpdate = false
lobbyUpdates = {"create" : {}, "update" : {}, "delete" : {}}

swapSide = (side) ->
  if side is "Corp" then "Runner" else "Corp"

refreshLobby = (type, gameid) ->
  lobbyUpdate = true
  if type is "delete"
    lobbyUpdates[type][gameid] = "0"
  else
    lobbyUpdates[type][gameid] = games[gameid]

removePlayer = (socket) ->
  game = games[socket.gameid]
  if game
    for player, i in game.players
      if player.id is socket.id
        game.players.splice(i, 1)
        break
    for spectator, i in game.spectators
      if spectator.id is socket.id
        game.spectators.splice(i, 1)
        break
    if game.started and game.players.length is 1
      requester.send(JSON.stringify({action: "finaluser-add", user: game.players[0], gameid: socket.gameid}))
    if game.players.length is 0 and game.spectators.length is 0
      # Store the list of player sockets for the game before we delete the game
      stats.sockets = []
      if game.endingPlayers
        for player in game.endingPlayers
          stats.sockets.push(player.id)
      delete games[socket.gameid]
      requester.send(JSON.stringify({action: "remove", gameid: socket.gameid}))
      refreshLobby("delete", socket.gameid)
      # Send a message to players telling browser to pull updated stats
      for id in stats.sockets
        stats.to(id).emit("netrunner", {channel: 'stats', msg: 'updatestats'})
    else
      refreshLobby("update", socket.gameid)
    socket.leave(socket.gameid)
    socket.gameid = false

  for k, v of games
    if (not v.started or v.players.length < 2) and (new Date() - v.date) > 3600000
      delete games[k]
      refreshLobby("delete", v.gameid)

rejoinGame = (socket, gameid, user, options) ->
  game = games[gameid]
  if game and game.started and game.players.length < 2
    side = if game.players.length is 1 then swapSide(game.players[0].side) else "Corp"
    user.id = socket.id
    game.players.push(user)
    # Replace the game end player list with the new list
    game.endingPlayers = game.players.slice(0)
    socket.join(gameid)
    socket.gameid = gameid
    socket.emit("netrunner", {type: "game", gameid: gameid})
    refreshLobby("update", gameid)
    requester.send(JSON.stringify({action: "finaluser-del", gameid: socket.gameid}))

joinGame = (socket, gameid, options) ->
  game = games[gameid]
  if game and game.players.length < 2
    side = if game.players.length is 1 then swapSide(game.players[0].side) else "Corp"
    game.players.push({user: socket.request.user, id: socket.id, side: side, options: options})
    socket.join(gameid)
    socket.gameid = gameid
    socket.emit("netrunner", {type: "game", gameid: gameid})
    refreshLobby("update", gameid)

getUsername = (socket) ->
  ((socket.request || {}).user || {}).username

users_blocked_from_game = (game) ->
  blocked_users = []
  for user in game.players
    if user.options['blocked-users']
      for blocked in user.options['blocked-users']
        blocked_users.push(blocked)
  blocked_users

user_allowed_in_game = (username, game) ->
  not_allowed = username in users_blocked_from_game(game)
  not not_allowed

##
## User/Deck Stats
##

save_stats = (option, room) ->
  (option is "always") or ((option is "competitive") and (room is "competitive"))

inc_deck = (deck, key) ->
  if deck
    db.collection('decks').update {_id: mongoskin.helper.toObjectID(deck)},
      {$inc: {"#{key}" : 1}}, (err) ->
        throw err if err

inc_game_complete = (username, side) ->
  if username
    db.collection('users').update {username: username},
      {$inc: {"stats.games-completed" : 1, "stats.games-completed-#{side}" : 1}}, (err) ->
        throw err if err

inc_game_end = (username, side, outcome) ->
  if username
    db.collection('users').update {username: username},
      {$inc: {"stats.#{outcome}": 1, "stats.#{outcome}-#{side}" : 1}}, (err) ->
        throw err if err

inc_game = (user, room, outcome) ->
  if user and user.user
    # deck stats
    if save_stats(user.options.deckstats, room)
      deckID = user['deck-id']
      if deckID
        inc_deck(deckID, "stats.#{outcome}")
        inc_deck(deckID, "stats.games-completed")

    # user game stats
    username = user.user.username
    side = user.identity.side.toLowerCase()
    inc_game_complete(username, side) # everyone gets game completion stats

    if save_stats(user.options.gamestats, room)
      inc_game_end(username, side, outcome)

inc_game_win = (winner, room) ->
  inc_game(winner, room, "wins")

inc_game_loss = (loser, room) ->
  inc_game(loser, room, "loses")

inc_game_final_user = (user, room) ->
  if user
    side = user.identity.side.toLowerCase()
    inc_game_complete(user.user.username, side)

  if save_stats(user.options.deckstats, room)
    deckID = user['deck-id']
    inc_deck(deckID, "stats.games-completed") if deckID

inc_game_start = (user, side, room) ->
  if user
    db.collection('users').update {username: user.user.username},
      {$inc: {"stats.games-started" : 1, "stats.games-started-#{side}" : 1}}, (err) ->
        console.log(err) if err

  if save_stats(user.options.deckstats, room)
    deckID = user['deck-id']
    inc_deck(deckID, "stats.games-started") if deckID

inc_corp_game_start = (user, room) ->
  inc_game_start(user, "corp", room)

inc_runner_game_start = (user, room) ->
  inc_game_start(user, "runner", room)

# ZeroMQ
clojure_hostname = process.env['CLOJURE_HOST'] || "127.0.0.1"
requester_connected = false
requester = zmq.socket('req')
requester.on 'connect', (fd, ep) ->
  requester_connected = true
  db.collection("cards").find().sort(_id: 1).toArray (err, data) ->
    requester.send(JSON.stringify({action: "initialize", cards: data}))

requester.on 'close', (fd,ep) ->
  requester_connected = false
requester.on 'disconnect', (fd,ep) ->
  requester_connected = false

requester.monitor(500, 0)
requester.connect("tcp://#{clojure_hostname}:1043")

sendGameResponse = (game, response) ->
  diffs = response.runnerdiff

  for player in game.players
    socket = io.sockets.connected[player.id]
    if player.side is "Corp"
      # The response will either have a diff or a state. we don't actually send both,
      # whichever is null will not be sent over the socket.
      lobby.to(player.id).emit("netrunner", {type: response.action,\
                                             diff: response.corpdiff, \
                                             state: response.corpstate})
    else if player.side is "Runner"
      lobby.to(player.id).emit("netrunner", {type: response.action, \
                                             diff: response.runnerdiff, \
                                             state: response.runnerstate})
  for spect in game.spectators
    lobby.to(spect.id).emit("netrunner", {type: response.action,\
                                          diff: response.spectdiff, \
                                          state: response.spectstate})

requester.on 'message', (data) ->
  response = JSON.parse(data)
  if response.action is "remove"
    if response.state
      g = {
        winner: response.state.winner
        reason: response.state.reason
        endDate: response.state["end-time"]
        turn: response.state.turn
        runnerAgenda: response.state.runner["agenda-point"]
        corpAgenda: response.state.corp["agenda-point"]
      }
      db.collection('gamestats').update {gameid: response.gameid}, {$set: g}, (err) ->
        throw err if err

      if response.state.corp.user and response.state.runner.user # have two users in the game
        room = response.state.room
        inc_corp_game_start(response.state.corp, room)
        inc_runner_game_start(response.state.runner, room)
        if response.state.winner # and someone won
          inc_game_win(response.state[response.state.winner], room)
          inc_game_loss(response.state[response.state.loser], room)
        else if response.state["final-user"] # someone left before the game was won
          final_side = response.state["final-user"].side
          inc_game_final_user(response.state[final_side], room)

  else
    if (games[response.gameid])
      sendGameResponse(games[response.gameid], response)

# Socket.io
io.set("heartbeat timeout", 30000)
io.use (socket, next) ->
  if socket.handshake.query.token
    jwt.verify socket.handshake.query.token, config.salt, (err, user) ->
      user = Object.assign({}, user)
      delete user.exp
      delete user.iat
      socket.request.user = user unless err
      next()
  else
    next()

stats = io.of('/stats').on 'connection', (socket) ->
  socket.on 'netrunner', (msg) ->
    stats.emit('netrunner', msg)

chat = io.of('/chat').on 'connection', (socket) ->
  socket.on 'netrunner', (msg) ->
    if socket.request.user
      msg.date = new Date()
      msg.username = socket.request.user.username
      msg.emailhash = socket.request.user.emailhash
      chat.emit('netrunner', msg)
      db.collection('messages').insert msg, (err, result) ->

lobby = io.of('/lobby').on 'connection', (socket) ->
  socket.emit("netrunner", {type: "games", games: games})

  socket.on 'disconnect', () ->
    gid = socket.gameid
    game = games[gid]
    if game
      if game.started and game.players.length > 1
        requester.send(JSON.stringify({action: "notification", gameid: gid, text: "#{getUsername(socket)} disconnected."}))
      removePlayer(socket)

  socket.on 'netrunner', (msg, fn) ->
    switch msg.action
      when "create"
        gameid = uuid.v1()
        game =
          date: new Date()
          gameid: gameid
          title: msg.title.substring(0,30)
          allowspectator: msg.allowspectator
          spectatorhands: msg.spectatorhands
          mutespectators: false
          password: if msg.password then crypto.createHash('md5').update(msg.password).digest('hex') else ""
          room: msg.room
          players: [{user: socket.request.user, id: socket.id, side: msg.side, options: msg.options}]
          spectators: []
        games[gameid] = game
        socket.join(gameid)
        socket.gameid = gameid
        socket.emit("netrunner", {type: "game", gameid: gameid})
        refreshLobby("create", gameid)

      when "leave-lobby"
        gid = socket.gameid
        removePlayer(socket)
        if socket.request.user
          socket.broadcast.to(gid).emit('netrunner', {type: "say", user: "__system__", text: "#{getUsername(socket)} left the game."})

      when "leave-game"
        gid = socket.gameid
        game = games[gid]
        if game
          if game.players.length > 1
            requester.send(JSON.stringify({action: "notification", gameid: gid, text: "#{getUsername(socket)} left the game."}))
          removePlayer(socket)

      when "concede"
        requester.send(JSON.stringify(msg))

      when "join"
        game = games[msg.gameid]

        unless game
          fn("invalid game")
          return

        unless socket.request.user
          fn("invalid user")
          return

        unless user_allowed_in_game(getUsername(socket), game)
          fn("not allowed")
          return

        if not game.password or game.password.length is 0 or (msg.password and crypto.createHash('md5').update(msg.password).digest('hex') is game.password)
          fn("join ok")
          joinGame(socket, msg.gameid, msg.options)
          socket.broadcast.to(msg.gameid).emit 'netrunner',
            type: "say"
            user: "__system__"
            notification: "ting"
            text: "#{getUsername(socket)} joined the game."
        else
          fn("invalid password")

      when "watch"
        game = games[msg.gameid]

        unless game
          fn("invalid game")
          return

        unless socket.request.user
          fn("invalid user")
          return

        unless game.allowspectator
          fn("not allowed")
          return

        unless user_allowed_in_game(getUsername(socket), game)
          fn("not allowed")
          return

        if not game.password or game.password.length is 0 or (msg.password and crypto.createHash('md5').update(msg.password).digest('hex') is game.password)
          if game
            fn("watch ok")
            game.spectators.push({user: socket.request.user, id: socket.id})
            socket.join(msg.gameid)
            socket.gameid = msg.gameid
            socket.emit("netrunner", {type: "game", gameid: msg.gameid, started: game.started})
            refreshLobby("update", msg.gameid)
            if game.started
              requester.send(JSON.stringify({action: "notification", gameid: msg.gameid, text: "#{getUsername(socket)} joined the game as a spectator."}))
            else
              socket.broadcast.to(msg.gameid).emit 'netrunner',
                type: "say"
                user: "__system__"
                text: "#{getUsername(socket)} joined the game as a spectator."
        else
          fn("invalid password")

      when "rejoin"
        game = games[msg.gameid]
        if game and game.started and game.players.length == 1
          in_game = game.originalPlayers.filter (p) -> p.user._id == socket.request.user._id
          if in_game.length > 0
            rejoinGame(socket, msg.gameid, in_game[0], null)
            requester.send(JSON.stringify({action: "rejoin", user: socket.request.user, gameid: socket.gameid, text: "#{socket.request.user.username} rejoined the game."}))

      when "mute-spectators"
        game = games[msg.gameid]
        if game
          game.mutespectators = msg.mutestate
          refreshLobby("update", msg.gameid)
          sendLobby()
          if game.mutespectators
            str = "muted"
          else
            str = "unmuted"
          text = "#{getUsername(socket)} #{str} spectators."
          if game.started
            requester.send(JSON.stringify({action: "notification", gameid: msg.gameid, text: text}))
          else
            socket.broadcast.to(msg.gameid).emit 'netrunner',
              type: "say"
              user: "__system__"
              text: text

      when "reconnect"
        game = games[msg.gameid]
        if game and game.started
          joinGame(socket, msg.gameid, null)
          requester.send(JSON.stringify({action: "notification", gameid: socket.gameid, text: "#{getUsername(socket)} reconnected."}))

      when "say"
        game = games[msg.gameid]
        unless user_allowed_in_game(getUsername(socket), game)
          fn("not allowed")
          return

        lobby.to(msg.gameid).emit("netrunner", {type: "say", user: socket.request.user, text: msg.text})

      when "swap"
        for player in games[socket.gameid].players
          player.side = swapSide(player.side)
          player.deck = null
        updateMsg = {"update" : {}}
        updateMsg["update"][socket.gameid] = games[socket.gameid]
        lobby.to(msg.gameid).emit('netrunner', {type: "games", gamesdiff: updateMsg})
        refreshLobby("update", msg.gameid)

      when "deck"
        game = games[socket.gameid]
        if game and game.players
          for player in game.players
            if player.user.username is getUsername(socket)
              player.deck = msg.deck
              break
          updateMsg = {"update" : {}}
          updateMsg["update"][socket.gameid] = games[socket.gameid]
          lobby.to(msg.gameid).emit('netrunner', {type: "games", gamesdiff: updateMsg})

      when "start"
        if !requester_connected
          lobby.to(msg.gameid).emit 'netrunner',
            type: "say"
            user: "__system__"
            notification: "ting"
            text: "Unable to connect to game server, please try again."
          lobby.to(msg.gameid).emit 'netrunner',
            type: "lobby-notification"
            text: "Unable to start game. Please try again."
            severity: "error"
        else
          game = games[socket.gameid]
          if game
            if game.players.length is 2
              corp = if game.players[0].side is "Corp" then game.players[0] else game.players[1]
              runner = if game.players[0].side is "Runner" then game.players[0] else game.players[1]
              g = {
                gameid: socket.gameid
                startDate: (new Date()).toISOString()
                title: game.title
                room: game.room
                corp: corp.user.username
                runner: runner.user.username
                corpIdentity: if corp.deck then corp.deck.identity.title else null
                runnerIdentity: if runner.deck then runner.deck.identity.title else null
              }
              db.collection('gamestats').insert g, (err, data) ->
                console.log(err) if err
            game.started = true
            game.originalPlayers = game.players.slice(0)
            game.endingPlayers = game.players.slice(0)
            msg = games[socket.gameid]
            msg.action = "start"
            msg.gameid = socket.gameid
            requester.send(JSON.stringify(msg))
            for player in game.players
              player.faction = if player.deck then player.deck.identity.faction else null
              player.identity = if player.deck then player.deck.identity.title else null
              if player.deck
                deckid = player.deck._id
                delete player.deck
                player.deck = {}
                player.deck._id = deckid
            refreshLobby("update", msg.gameid)

      when "do"
        if msg.command == "say"
          game = games[socket.gameid]
          if game and msg.side == "spectator" and game.mutespectators
            return
        try
          msg.user = socket.request.user
          requester.send(JSON.stringify(msg))
        catch err
          console.log(err)

sendLobby = () ->
  if lobby and lobbyUpdate
    lobby.emit('netrunner', {type: "games", gamesdiff: lobbyUpdates})
    lobbyUpdate = false
    lobbyUpdates["create"] = {}
    lobbyUpdates["update"] = {}
    lobbyUpdates["delete"] = {}

setInterval(sendLobby, 1000)

# Express config
app.use favicon(__dirname + "/resources/public/img/jinteki.ico")
app.set 'port', 1042
app.set 'ipaddr', "0.0.0.0"
app.use methodOverride() # provide PUT DELETE
app.use cookieParser()
app.use bodyParser.urlencoded(extended: false)
app.use (req, res, next) ->
  bodyParser.json() req, res, (err) ->
    if err then console.log(err)
    next()
app.use session
  secret: config.salt
  saveUninitialized: false
  resave: false
  store: new MongoStore(url: mongoUrl)
  cookie: { maxAge: 2592000000 } # 30 days
app.use passport.initialize()
app.use passport.session()
app.use stylus.middleware({src: __dirname + '/src', dest: __dirname + '/resources/public'})
app.use express.static(__dirname + '/resources/public')

# load version info from database
db.collection('config').findOne (err, config) ->
  if err || config == null
      console.log('Creating config information in database')
      db.createCollection('config')
      db.collection('config').insert {version: process.env['APP_VERSION'] || "0.1.0"}, (err) ->
        if err
          console.log('Could not create config in database')
        else
          app.locals.version = config && config.version || process.env['APP_VERSION'] || "0.1.0"
          console.log('App version ' + app.locals.version)
  else
    app.locals.version = config && config.version || process.env['APP_VERSION'] || "0.1.0"
    console.log('App version ' + app.locals.version)

# Auth
passport.use new LocalStrategy (username, password, done) ->
  db.collection('users').findOne {username: RegExp("^#{username}$", "i")}, (err, user) ->
    return done(err) if err or not user
    bcrypt.compare password, user.password, (err, valid) ->
      return done(err) if err
      return done(null, false) if not valid
      if not user.options then user.options = {}
      done(null, {username: user.username, emailhash: user.emailhash, _id: user._id,\
        isadmin: user.isadmin, options: user.options})

passport.serializeUser (user, done) ->
  done(null, user._id) if user

passport.deserializeUser (id, done) ->
  db.collection('users').findById id, (err, user) ->
    console.log err if err
    if not user.options then user.options = {}
    done(err, {username: user.username, emailhash: user.emailhash, _id: user._id, special: user.special,\
      isadmin: user.isadmin, options: user.options, stats: user.stats})

# Routes
app.options('*', cors())

app.post '/login', passport.authenticate('local'), (req, res) ->
  db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
    throw err if err
    res.status(200).json({user: req.user})

app.get '/logout', (req, res) ->
  req.logout()
  res.redirect('/')

app.post '/register', (req, res, next) ->
  if req.body.username.length > 20
    res.status(423).send({message: 'Usernames are limited to 20 characters'})
  else
    db.collection('users').findOne username: new RegExp("^#{req.body.username}$", "i"), (err, user) ->
      if user
        res.status(422).send({message: 'Username taken'})
      else
        email = req.body.email.trim().toLowerCase()
        db.collection('users').findOne email: new RegExp("^#{email}$", "i"), (err, user) ->
          if user
            res.status(424).send({message: 'Email already used'})
          else
            req.body.emailhash = crypto.createHash('md5').update(email).digest('hex')
            req.body.registrationDate = new Date()
            req.body.lastConnection = new Date()
            hashPassword req.body.password, (err, hash) ->
              req.body.password = hash
              db.collection('users').insert req.body, (err) ->
                if err
                  res.send("error: #{err}")
                else
                  req.login req.body, (err) ->
                    if err
                      next(err)
                    else
                      db.collection('decks').find({username: '__demo__'}).toArray (err, demoDecks) ->
                        throw err if err
                        for deck in demoDecks
                          delete deck._id
                          deck.username = req.body.username
                        if demoDecks.length > 0
                          db.collection('decks').insert demoDecks, (err, newDecks) ->
                            throw err if err
                            res.status(200).json({user: req.user, decks: newDecks})
                        else
                          res.status(200).json({user: req.user, decks: []})

app.post '/forgot', (req, res) ->
  async.waterfall [
    (done) ->
      crypto.randomBytes 20, (err, buf) ->
        token = buf.toString('hex')
        done(err, token)
    (token, done) ->
      db.collection('users').findOne { email: req.body.email }, (err, user) ->
        if (!user)
          res.status(421).send({message: 'No account with that email address exists.'})
        else
          # 1 hour expiration
          resetPasswordToken = token
          resetPasswordExpires = Date.now() + 3600000

          db.collection('users').update { email: req.body.email }, {$set: {resetPasswordToken: resetPasswordToken, resetPasswordExpires: resetPasswordExpires}}, (err) ->
            throw err if err
            done(err, token, user)
            # res.status(200).send({message: 'Password reset sent.'})
    (token, user, done) ->
      smtpTransport = nodemailer.createTransport {
        service: 'SendGrid',
        auth: {
          user: process.env['SENDGRID_USER'] || "",
          pass: process.env['SENDGRID_PASSWORD'] || ""
        }
      }
      mailOptions = {
        from: 'support@jinteki.net',
        to: user.email,
        subject: 'Jinteki Password Reset',
        text: 'You are receiving this because you (or someone else) have requested the reset of the password for your account.\n\n' +
          'Please click on the following link, or paste this into your browser to complete the process:\n\n' +
          'http://' + req.headers.host + '/reset/' + token + '\n\n' +
          'If you did not request this, please ignore this email and your password will remain unchanged.\n'
      }
      smtpTransport.sendMail mailOptions, (err, response) ->
        throw err if err
        res.status(200).send({message: 'An e-mail has been sent to ' + user.email + ' with further instructions.'})
  ]

app.get '/check/:username', (req, res) ->
  db.collection('users').findOne username: req.params.username, (err, user) ->
    if user
      res.status(422).send({message: 'Username taken'})
    else
      res.status(200).send({message: 'OK'})

app.get '/reset/:token', (req, res) ->
  db.collection('users').findOne resetPasswordToken: req.params.token, resetPasswordExpires: { $gt: Date.now() } , (err, user) ->
    if (!user)
      #req.flash 'error', 'Password reset token is invalid or has expired.'
      return res.redirect('/forgot')
    if user
      db.collection('users').update {username: user.username}, {$set: {lastConnection: new Date()}}, (err) ->
      token = jwt.sign(user, config.salt, {expiresIn: '6h'})
    res.render('reset.pug', { user: req.user })

app.post '/reset/:token', (req, res) ->
  async.waterfall [
    (done) ->
      db.collection('users').findOne resetPasswordToken: req.params.token, resetPasswordExpires: { $gt: Date.now() }, (err, user) ->
        if (!user)
          # req.flash('error', 'Password reset token is invalid or has expired.');
          return res.redirect('back');

        # To be implemented: checking password == confirm
        # if (req.body.password != req.body.confirm)
        #   res.status(412).send({message: 'Password does not match Confirm'})

        hashPassword req.body.password, (err, hash) ->
          password = hash
          resetPasswordToken = undefined;
          resetPasswordExpires = undefined

          db.collection('users').update { username: user.username }, {$set: {password: password, resetPasswordToken: resetPasswordToken, resetPasswordExpires: resetPasswordExpires}}, (err) ->
            req.logIn user, (err) ->
              done(err, user)
    (user, done) ->
      smtpTransport = nodemailer.createTransport {
        service: 'SendGrid',
        auth: {
          user: process.env['SENDGRID_USER'] || "",
          pass: process.env['SENDGRID_PASSWORD'] || ""
        }
      }
      mailOptions = {
        to: user.email,
        from: 'passwordreset@jinteki.net',
        subject: 'Your password has been changed',
        text: 'Hello,\n\n' +
          'This is a confirmation that the password for your account ' + user.email + ' has just been changed.\n'
      }
      smtpTransport.sendMail mailOptions, (err) ->
        # req.flash 'success', 'Success! Your password has been changed.'
        throw err if err
        done(err)
  ], (err) ->
    throw err if err
    res.redirect('/')

app.post '/update-profile', (req, res) ->
  if req.user
    db.collection('users').update {username: req.user.username}, {$set: {options: {background: req.body.background,\
      'show-alt-art': req.body['show-alt-art'], 'blocked-users': req.body['blocked-users'], \
      'alt-arts': req.body['alt-arts'], deckstats: req.body['deckstats'], gamestats: req.body['gamestats']}}},
      (err) ->
        console.log(err) if err
        res.status(200).send({message: 'OK', background: req.body.background, \
          altarts: req.body['alt-arts'], \
          blockedusers: req.body['blocked-users'], \
          deckstats: req.body['deckstats'], gamestats: req.body['gamestats']})
  else
    res.status(401).send({message: 'Unauthorized'})

app.post '/user/clearstats', (req, res) ->
  if req.user
    db.collection('users').update {username: req.user.username}, {$unset: {stats: ""}}, \
      (err) ->
        console.log(err) if err
        res.status(200).send({message: 'OK'})
  else
    res.status(401).send({message: 'Unauthorized'})

app.get '/user', (req, res) ->
  if req.user
    db.collection('users').find({username: req.user.username}).toArray (err, data) ->
      throw err if err
      res.status(200).json(data)
  else
    res.status(401).send({message: 'Unauthorized'})

hashPassword = (password, cb) ->
  bcrypt.hash password, 10, cb

app.get '/messages/:channel', (req, res) ->
  db.collection('messages').find({channel: req.params.channel}).sort(date: -1).limit(100).toArray (err, data) ->
    throw err if err
    res.status(200).json(data.reverse())

app.get '/data/decks', (req, res) ->
  if req.user
    db.collection('decks').find({username: req.user.username}).toArray (err, data) ->
      throw err if err
      res.status(200).json(data)
  else
    db.collection('decks').find({username: "__demo__"}).toArray (err, data) ->
      throw err if err
      delete deck._id for deck in data
      res.status(200).json(data)

app.post '/data/decks', (req, res) ->
  deck = req.body
  sanitized_cards = deck.cards.map (entry) ->
    sanitized_entries = {}
    for k, v of entry
      sanitized_entries[k] = v if k in ["qty", "card", "id", "art"]
    sanitized_entries
  deck.cards = sanitized_cards
  if req.user
    deck.username = req.user.username
    if deck._id
      id = deck._id
      delete deck._id
      db.collection('decks').update {_id: mongoskin.helper.toObjectID(id)}, {$set: deck}, (err) ->
        console.log(err) if err
        res.status(200).send({message: 'OK'})
    else
      db.collection('decks').insert deck, (err, data) ->
        console.log(err) if err
        res.status(200).send(data.ops[0])
  else
    res.status(401).send({message: 'Unauthorized'})

app.post '/data/decks/clearstats', (req, res) ->
  deck = req.body
  if req.user
    deck.username = req.user.username
    if deck._id
      id = deck._id
      delete deck._id
      db.collection('decks').update {_id: mongoskin.helper.toObjectID(id)}, deck, (err) ->
        console.log(err) if err
        res.status(200).send({message: 'OK'})
    else
      res.status(401).send({message: 'Deck ID does not exist'})
  else
    res.status(401).send({message: 'Unauthorized'})

app.post '/data/decks/delete', (req, res) ->
  deck = req.body
  if req.user
    db.collection('decks').remove {_id: mongoskin.helper.toObjectID(deck._id), username: req.user.username}, (err) ->
      res.status(200).send({message: 'OK'})
  else
    res.status(401).send({message: 'Unauthorized'})

app.get '/data/donators', (req, res) ->
  db.collection('donators').find({}).sort({amount: -1}).toArray (err, data) ->
    if data
      res.status(200).json(d.username or d.name for d in data)
    else
      res.status(200).json([])
    throw err if err

app.get '/data/news', (req, res) ->
  if process.env['TRELLO_API_KEY']
    cached = cache.get('news')
    if not cached
      t = new Trello(process.env['TRELLO_API_KEY'])
      t.get '/1/lists/5668b498ced988b1204cae9a/cards', {filter : 'open', fields : 'dateLastActivity,name,labels'}, (err, data) ->
        throw err if err
        data = ({title: d.name, date: d.date = moment(d.dateLastActivity).format("MM/DD/YYYY HH:mm")} \
          for d in data when d.labels.length == 0)
        cache.put('news', data, 60000) # 60 seconds timeout
        res.status(200).json(data)
    else
      res.status(200).json(cached)
  else
    res.status(200).json([{date: '01/01/2015 00:00', title: 'Get a Trello API Key and set your environment variable TRELLO_API_KEY to see announcements'}])

app.get '/data/:collection', (req, res) ->
  if req.params.collection != 'users' && req.params.collection != 'games'
    db.collection(req.params.collection).find().sort(_id: 1).toArray (err, data) ->
      throw err if err
      delete d._id for d in data
      res.status(200).json(data)
  else
    res.status(401).send({message: 'Unauthorized'})

app.get '/data/:collection/:field/:value', (req, res) ->
  if req.params.collection != 'users' && req.params.collection != 'games'
    filter = {}
    filter[req.params.field] = req.params.value
    db.collection(req.params.collection).find(filter).toArray (err, data) ->
      console.error(err) if err
      delete d._id for d in data
      res.status(200).json(data)
  else
    res.status(401).send({message: 'Unauthorized'})

app.get '/admin/announce', (req, res) ->
  if req.user and req.user.isadmin
    res.render('announce.pug', {user : req.user})
  else
    res.status(401).send({message: 'Unauthorized'})

app.post '/admin/announce', (req, res) ->
  if req.user and req.user.isadmin
    requester.send(JSON.stringify({action: "alert", command: req.body.message}))
    res.status(200).send({text: req.body.message, result: "ok"})
  else
    res.status(401).send({message: 'Unauthorized'})

app.get '/admin/init', (req, res) ->
  if req.user and req.user.isadmin
    db.collection("cards").find().sort(_id: 1).toArray (err, data) ->
      requester.send(JSON.stringify({action: "initialize", cards: data}))
      res.status(200).send({result: "ok"})
  else
    res.status(401).send({message: 'Unauthorized'})

app.get '/admin/version', (req, res) ->
  if req.user and req.user.isadmin
    res.render('version.pug', {user : req.user, version: app.locals.version})
  else
    res.status(401).send({message: 'Unauthorized'})

app.post '/admin/version', (req, res) ->
  if req.user and req.user.isadmin
    app.locals.version = req.body.version
    db.collection('config').update {}, {$set: {version: req.body.version}}, (err) ->
      res.status(200).send({text: req.body.version, result: "ok"})
  else
    res.status(401).send({message: 'Unauthorized'})

env = process.env['NODE_ENV'] || 'development'

if env == 'development'
  console.log "Dev environment"
  app.get '/*', (req, res) ->
    if req.user
      db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
      token = jwt.sign(req.user, config.salt)
    res.render('index.pug', { user: req.user, env: 'dev', token: token, version: app.locals.version})

if env == 'production'
  console.log "Prod environment"
  app.get '/*', (req, res) ->
    if req.user
      db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
      token = jwt.sign(req.user, config.salt, {expiresIn: '6h'})
    res.render('index.pug', { user: req.user, env: 'prod', token: token, version: app.locals.version})

# Server
terminate = () ->
  process.exit(1)
  console.log("#{Date(Date.now())}: Node server stopped.")

process.on('exit', terminate)

for signal in ['SIGHUP', 'SIGINT', 'SIGQUIT', 'SIGILL', 'SIGTRAP', 'SIGABRT',
               'SIGBUS', 'SIGFPE', 'SIGUSR1', 'SIGSEGV', 'SIGUSR2', 'SIGTERM']
  process.on(signal, terminate)

process.on 'uncaughtException', (err) ->
  console.log(err.stack)

server.listen app.get('port'), app.get('ipaddr'), ->
  console.log(new Date().toString() + ": Express server listening on port " + app.get('port'))
