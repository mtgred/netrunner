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
    if game.players.length is 0 and game.spectators.length is 0
      delete games[socket.gameid]
      requester.send(JSON.stringify({action: "remove", gameid: socket.gameid}))
      refreshLobby("delete", socket.gameid)
    else
      refreshLobby("update", socket.gameid)
    socket.leave(socket.gameid)
    socket.gameid = false

  for k, v of games
    if (not v.started or v.players.length < 2) and (new Date() - v.date) > 3600000

      delete games[k]
      refreshLobby("delete", v.gameid)

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

# ZeroMQ
clojure_hostname = process.env['CLOJURE_HOST'] || "127.0.0.1"
requester = zmq.socket('req')
requester.on 'connect', (fd, ep) ->
  db.collection("cards").find().sort(_id: 1).toArray (err, data) ->
    requester.send(JSON.stringify({action: "initialize", cards: data}))

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

chat = io.of('/chat').on 'connection', (socket) ->
  socket.on 'netrunner', (msg) ->
    msg.date = new Date()
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
        if not game.password or game.password.length is 0 or (msg.password and crypto.createHash('md5').update(msg.password).digest('hex') is game.password)
          if game
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

      when "reconnect"
        game = games[msg.gameid]
        if game and game.started
          joinGame(socket, msg.gameid, null)
          requester.send(JSON.stringify({action: "notification", gameid: socket.gameid, text: "#{getUsername(socket)} reconnected."}))

      when "say"
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
          msg = games[socket.gameid]
          msg.action = "start"
          msg.gameid = socket.gameid
          requester.send(JSON.stringify(msg))
          for player in game.players
            player.faction = if player.deck then player.deck.identity.faction else null
            player.identity = if player.deck then player.deck.identity.title else null
            delete player.deck
          refreshLobby("update", msg.gameid)

      when "do"
        try
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

app.locals.version = process.env['APP_VERSION'] || "0.1.0"

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
      isadmin: user.isadmin, options: user.options})

# Routes
app.options('*', cors())

app.post '/login', passport.authenticate('local'), (req, res) ->
  db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
    throw err if err
    res.status(200).json({user: req.user})

app.get '/logout', (req, res) ->
  req.logout()
  res.redirect('/')

app.post '/register', (req, res) ->
  db.collection('users').findOne username: new RegExp("^#{req.body.username}$", "i"), (err, user) ->
    if user
      res.status(422).send({message: 'Username taken'})
    else if req.body.username.length < 4 or req.body.username.length > 16
      res.status(423).send({message: 'Username too short/too long'})
    else
      email = req.body.email.trim().toLowerCase()
      req.body.emailhash = crypto.createHash('md5').update(email).digest('hex')
      req.body.registrationDate = new Date()
      req.body.lastConnection = new Date()
      hashPassword req.body.password, (err, hash) ->
        req.body.password = hash
        db.collection('users').insert req.body, (err) ->
          res.send("error: #{err}") if err
          req.login req.body, (err) -> next(err) if err
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
    res.render('reset.jade', { user: req.user })

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
      'alt-arts': req.body['alt-arts'], 'opponent-alt-art': req.body['opponent-alt-art']}}}, \
      (err) ->
        console.log(err) if err
        res.status(200).send({message: 'OK', background: req.body.background, altarts: req.body['alt-arts']})
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
  if req.user
    deck.username = req.user.username
    if deck._id
      id = deck._id
      delete deck._id
      db.collection('decks').update {_id: mongoskin.helper.toObjectID(id)}, deck, (err) ->
        console.log(err) if err
        res.status(200).send({message: 'OK'})
    else
      db.collection('decks').insert deck, (err, data) ->
        console.log(err) if err
        res.status(200).json(data[0])
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
    res.status(200).json(d.username or d.name for d in data)

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

app.get '/announce', (req, res) ->
  if req.user and req.user.isadmin
    res.render('announce.jade', {user : req.user})
  else
    res.status(401).send({message: 'Unauthorized'})

app.post '/announce', (req, res) ->
  if req.user and req.user.isadmin
    requester.send(JSON.stringify({action: "alert", command: req.body.message}))
    res.status(200).send({text: req.body.message, result: "ok"})
  else
    res.status(401).send({message: 'Unauthorized'})

env = process.env['NODE_ENV'] || 'development'

if env == 'development'
  console.log "Dev environment"
  app.get '/*', (req, res) ->
    if req.user
      db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
      token = jwt.sign(req.user, config.salt)
    res.render('index.jade', { user: req.user, env: 'dev', token: token, version: app.locals.version})

if env == 'production'
  console.log "Prod environment"
  app.get '/*', (req, res) ->
    if req.user
      db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
      token = jwt.sign(req.user, config.salt, {expiresIn: '6h'})
    res.render('index.jade', { user: req.user, env: 'prod', token: token, version: app.locals.version})

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
