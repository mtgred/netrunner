FROM clojure:lein-alpine
LABEL maintainer="John Warwick <jwarwick@gmail.com>"

RUN apk --no-cache add \
    git \
    nodejs \
    nodejs-npm

RUN npm install -g bower stylus

WORKDIR /
RUN git clone https://github.com/mtgred/netrunner.git
WORKDIR /netrunner
RUN lein deps
RUN bower --allow-root install
RUN stylus src/css -o resources/public/css/
RUN lein cljsbuild once prod
RUN lein uberjar

ENTRYPOINT ["java", "-jar", "target/netrunner-standalone.jar"]


