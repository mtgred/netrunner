express = require('express')
app = express()
server = require('http').createServer(app)
stylus = require('stylus')
coffeeMiddleware = require('coffee-middleware')

# db = require('mongoskin').db('localhost:27017/netrunner')

io = require('socket.io').listen(server)
io.enable('browser client minification')
io.enable('browser client etag')
io.enable('browser client gzip')
io.set('log level', 1)
io.set('transports', ['websocket'])

io.sockets.on 'connection', (socket) ->
  socket.on 'message', (msg) ->
    io.sockets.emit "netrunner", msg

app.configure ->
  app.set 'port', process.env.OPENSHIFT_NODEJS_PORT || 3000
  app.set 'ipaddr', process.env.OPENSHIFT_NODEJS_IP || "127.0.0.1"
  app.use express.favicon()
  app.use express.methodOverride() # provide PUT DELETE
  app.use express.cookieParser()
  app.use express.bodyParser()
  app.use stylus.middleware({src: __dirname + '/src', dest: __dirname + '/resources'})
  app.use coffeeMiddleware({src: __dirname + '/src', dest: __dirname + '/resources'})
  app.use express.static(__dirname + '/resources')
  app.use app.router

app.configure 'development', ->
  console.log "Dev environment"
  app.get '/*', (req, res) ->
    res.render('index.jade', { env: 'dev'})

app.configure 'production', ->
  console.log "Prod environment"
  app.get '/*', (req, res) ->
    res.render('index.jade', { env: 'prod'})

terminate = () ->
  process.exit(1)
  console.log("#{Date(Date.now())}: Node server stopped.")

process.on('exit', terminate)

for signal in ['SIGHUP', 'SIGINT', 'SIGQUIT', 'SIGILL', 'SIGTRAP', 'SIGABRT',
               'SIGBUS', 'SIGFPE', 'SIGUSR1', 'SIGSEGV', 'SIGUSR2', 'SIGTERM']
  process.on(signal, terminate)

server.listen app.get('port'), app.get('ipaddr'), ->
  console.log("Express server listening on port " + app.get('port'))
