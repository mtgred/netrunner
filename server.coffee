express = require('express')
app = express()
server = require('http').createServer(app)
stylus = require('stylus')
config = require('./config')
mongoskin = require('mongoskin')
MongoStore = require('connect-mongo')(express)
crypto = require('crypto')
bcrypt = require('bcrypt')
passport = require('passport')
localStrategy = require('passport-local').Strategy

# MongoDB connection
mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'
mongoUrl = "mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}"

db = mongoskin.db(mongoUrl)

# Socket.io
io = require('socket.io').listen(server)
io.enable('browser client minification')
io.enable('browser client etag')
io.enable('browser client gzip')
io.set('log level', 1)
io.set('transports', ['websocket'])

io.sockets.on 'connection', (socket) ->
  socket.on 'netrunner', (msg) ->
    msg.date = new Date()
    io.sockets.emit('netrunner', msg)
    db.collection('messages').insert msg, (err, result) ->

# Express config
app.configure ->
  app.set 'port', process.env.OPENSHIFT_NODEJS_PORT || 3000
  app.set 'ipaddr', process.env.OPENSHIFT_NODEJS_IP || "0.0.0.0"
  app.use express.favicon()
  app.use express.methodOverride() # provide PUT DELETE
  app.use express.cookieParser()
  app.use express.bodyParser()
  app.use express.session(store: new MongoStore(url: mongoUrl), secret: config.salt)
  app.use passport.initialize()
  app.use passport.session()
  app.use stylus.middleware({src: __dirname + '/src', dest: __dirname + '/resources'})
  app.use express.static(__dirname + '/resources')
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
  res.json(200, req.user)

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
      bcrypt.hash req.body.password, 3, (err, hash) ->
        req.body.password = hash
        db.collection('users').insert req.body, (err) ->
          res.send "error: #{err}" if err
          req.login req.body, (err) -> next(err) if err
          res.json(200, req.user)

app.get '/check/:username', (req, res) ->
  db.collection('users').findOne username: req.params.username, (err, user) ->
    if user
      res.send {message: 'Username taken'}, 422
    else
      res.send {message: 'OK'}, 200

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

app.get '/messages/:channel', (req, res) ->
  db.collection('messages').find({channel: req.params.channel}).sort(date: 1).limit(50).toArray (err, data) ->
    throw err if err
    res.json(200, data)

app.configure 'development', ->
  console.log "Dev environment"
  app.get '/*', (req, res) ->
    res.render('index.jade', { user: req.user, env: 'dev'})

app.configure 'production', ->
  console.log "Prod environment"
  app.get '/*', (req, res) ->
    res.render('index.jade', { user: req.user, env: 'prod'})

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
