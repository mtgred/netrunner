express = require('express')
app = express()
server = require('http').createServer(app)
io = require('socket.io')(server)
stylus = require('stylus')
config = require('./config')
mongoskin = require('mongoskin')
MongoStore = require('connect-mongo')(express)
crypto = require('crypto')
bcrypt = require('bcrypt')
passport = require('passport')
localStrategy = require('passport-local').Strategy
jwt = require('jsonwebtoken')
gameEngine = require('./game')

# MongoDB connection
mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'
mongoUrl = "mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}"
db = mongoskin.db(mongoUrl)

# Game lobby
gameid = 0
games = []

swapSide = (side) ->
  if side is "Corp" then "Runner" else "Corp"

removePlayer = (socket, username) ->
  for game, i in games
    for player, j in game.players
      if player.user.username is username
        game.players.splice(j, 1)
        socket.to(game.gameid).emit('netrunner', {type: "say", user: "__system__", text: "#{username} left the game."})
        break
    if game.players.length is 0
      games.splice(i, 1)
      break

# Socket.io
io.use (socket, next) ->
  if socket.handshake.query.token
    jwt.verify socket.handshake.query.token, config.salt, (err, user) ->
      socket.request.user = user unless err
  next()

chat = io.of('/chat').on 'connection', (socket) ->
  socket.on 'netrunner', (msg) ->
    msg.date = new Date()
    chat.emit('netrunner', msg)
    db.collection('messages').insert msg, (err, result) ->

lobby = io.of('/lobby').on 'connection', (socket) ->
  lobby.emit('netrunner', {type: "games", games: games})

  socket.on 'disconnect', () ->
    removePlayer(socket, socket.request.user.username) if socket.request.user
    lobby.emit('netrunner', {type: "games", games: games})

  socket.on 'netrunner', (msg) ->
    switch msg.action
      when "create"
        game = {date: new Date(), gameid: ++gameid, title: msg.title,\
                players: [{user: socket.request.user, side: "Corp"}]}
        games.push(game)
        socket.join(gameid)
        socket.emit("netrunner", {type: "game", gameid: gameid})
        lobby.emit('netrunner', {type: "games", games: games})

      when "leave"
        removePlayer(socket, socket.request.user.username)
        socket.leave(msg.gameid)
        lobby.emit('netrunner', {type: "games", games: games})

      when "join"
        for game in games
          if game.gameid is msg.gameid and game.players.length < 2 and game.players[0].user.username isnt socket.request.user.username
            game.players.push({user: socket.request.user, side: swapSide(game.players[0].side)})
            socket.join(game.gameid)
            socket.emit("netrunner", {type: "game", gameid: game.gameid})
            break
        lobby.emit('netrunner', {type: "games", games: games})
        socket.broadcast.to(msg.gameid).emit 'netrunner',
          type: "say"
          user: "__system__"
          text: "#{socket.request.user.username} joined the game."

      when "say"
        lobby.to(msg.gameid).emit("netrunner", {type: "say", user: socket.request.user, text: msg.text})

      when "swap"
        for game in games
          if game.gameid is msg.gameid
            for player in game.players
              player.side = swapSide(player.side)
              player.deck = null
            break
        lobby.to(msg.gameid).emit('netrunner', {type: "games", games: games})

      when "deck"
        for game in games
          if game.gameid is msg.gameid
            for player in game.players
              if player.user.username is socket.request.user.username
                player.deck = msg.deck
                break
            break
        lobby.to(msg.gameid).emit('netrunner', {type: "games", games: games})

      when "start"
        for game, i in games
          if game.gameid is msg.gameid
            state = gameEngine.main.exec("init", game)
            lobby.to(msg.gameid).emit("netrunner", {type: "start", state: state})
            games.splice(i, 1)
            break
        lobby.to(msg.gameid).emit('netrunner', {type: "games", games: games})

      when "do"
        try
          state = gameEngine.main.exec("do", msg)
          lobby.to(msg.gameid).emit("netrunner", {type: "state", state: state})
        catch err
          console.log(err)

# Express config
app.configure ->
  app.set 'port', process.env.OPENSHIFT_NODEJS_PORT || 3000
  app.set 'ipaddr', process.env.OPENSHIFT_NODEJS_IP || "0.0.0.0"
  app.use express.favicon()
  app.use express.methodOverride() # provide PUT DELETE
  app.use express.cookieParser()
  app.use express.urlencoded()
  app.use express.json()
  app.use express.session(store: new MongoStore(url: mongoUrl), secret: config.salt)
  app.use passport.initialize()
  app.use passport.session()
  app.use stylus.middleware({src: __dirname + '/src', dest: __dirname + '/resources/public'})
  app.use express.static(__dirname + '/resources/public')
  app.use app.router

# Auth
passport.use new localStrategy (username, password, done) ->
  db.collection('users').findOne {username: username}, (err, user) ->
    return done(err) if err or not user
    if bcrypt.compareSync(password, user.password)
      done(null, {username: user.username, emailhash: user.emailhash, _id: user._id})
    else
      return done(null, false)

passport.serializeUser (user, done) ->
  done(null, user._id) if user

passport.deserializeUser (id, done) ->
  db.collection('users').findById id, (err, user) ->
    console.log err if err
    done(err, {username: user.username, emailhash: user.emailhash, _id: user._id})

# Routes
app.post '/login', passport.authenticate('local'), (req, res) ->
  db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
    throw err if err
    res.json(200, {user: req.user})

app.get '/logout', (req, res) ->
  req.logout()
  res.redirect('/')

app.post '/register', (req, res) ->
  db.collection('users').findOne username: req.body.username, (err, user) ->
    if user
      res.send {message: 'Username taken'}, 422
    else
      email = req.body.email.trim().toLowerCase()
      req.body.emailhash = crypto.createHash('md5').update(email).digest('hex')
      req.body.registrationDate = new Date()
      req.body.lastConnection = new Date()
      bcrypt.hash req.body.password, 3, (err, hash) ->
        req.body.password = hash
        db.collection('users').insert req.body, (err) ->
          res.send "error: #{err}" if err
          req.login req.body, (err) -> next(err) if err
          db.collection('decks').find({username: '__demo__'}).toArray (err, demoDecks) ->
            throw err if err
            for deck in demoDecks
              delete deck._id
              deck.username = req.body.username
            db.collection('decks').insert demoDecks, (err, newDecks) ->
              throw err if err
              res.json(200, {user: req.user, decks: newDecks})

app.get '/check/:username', (req, res) ->
  db.collection('users').findOne username: req.params.username, (err, user) ->
    if user
      res.send {message: 'Username taken'}, 422
    else
      res.send {message: 'OK'}, 200

app.get '/messages/:channel', (req, res) ->
  db.collection('messages').find({channel: req.params.channel}).sort(date: 1).limit(50).toArray (err, data) ->
    throw err if err
    res.json(200, data)

app.get '/data/decks', (req, res) ->
  if req.user
    db.collection('decks').find({username: req.user.username}).toArray (err, data) ->
      throw err if err
      res.json(200, data)
  else
    db.collection('decks').find({username: "__demo__"}).toArray (err, data) ->
      throw err if err
      delete deck._id for deck in data
      res.json(200, data)

app.post '/data/decks', (req, res) ->
  deck = req.body
  if req.user
    deck.username = req.user.username
    if deck._id
      id = deck._id
      delete deck._id
      db.collection('decks').update {_id: mongoskin.helper.toObjectID(id)}, deck, (err) ->
        console.log(err) if err
        res.send {message: 'OK'}, 200
    else
      db.collection('decks').insert deck, (err, data) ->
        console.log(err) if err
        res.json(200, data[0])
  else
    res.send {message: 'Unauthorized'}, 401

app.post '/data/decks/delete', (req, res) ->
  deck = req.body
  if req.user
    db.collection('decks').remove {_id: mongoskin.helper.toObjectID(deck._id), username: req.user.username}, (err) ->
      res.send {message: 'OK'}, 200
  else
    res.send {message: 'Unauthorized'}, 401

app.get '/data/:collection', (req, res) ->
  db.collection(req.params.collection).find().sort(_id: 1).toArray (err, data) ->
    throw err if err
    delete d._id for d in data
    res.json(200, data)

app.get '/data/:collection/:field/:value', (req, res) ->
  filter = {}
  filter[req.params.field] = req.params.value
  db.collection(req.params.collection).find(filter).toArray (err, data) ->
    console.error(err) if err
    delete d._id for d in data
    res.json(200, data)

app.configure 'development', ->
  console.log "Dev environment"
  app.get '/*', (req, res) ->
    if req.user
      db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
      token = jwt.sign(req.user, config.salt)
    res.render('index.jade', { user: req.user, env: 'dev', token: token})

app.configure 'production', ->
  console.log "Prod environment"
  app.get '/*', (req, res) ->
    if req.user
      db.collection('users').update {username: req.user.username}, {$set: {lastConnection: new Date()}}, (err) ->
      token = jwt.sign(req.user, config.salt, {expiresInMinutes: 360})
    res.render('index.jade', { user: req.user, env: 'prod', token: token})

# Server
terminate = () ->
  process.exit(1)
  console.log("#{Date(Date.now())}: Node server stopped.")

process.on('exit', terminate)

for signal in ['SIGHUP', 'SIGINT', 'SIGQUIT', 'SIGILL', 'SIGTRAP', 'SIGABRT',
               'SIGBUS', 'SIGFPE', 'SIGUSR1', 'SIGSEGV', 'SIGUSR2', 'SIGTERM']
  process.on(signal, terminate)

server.listen app.get('port'), app.get('ipaddr'), ->
  console.log("Express server listening on port " + app.get('port'))
