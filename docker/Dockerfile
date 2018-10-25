FROM clojure:lein-alpine as builder

RUN apk --no-cache add \
    git \
    nodejs \
    nodejs-npm

RUN npm install -g bower stylus

WORKDIR /netrunner
COPY . .
RUN bower --allow-root install
RUN stylus src/css -o resources/public/css/
RUN lein deps
RUN lein cljsbuild once prod
RUN lein uberjar


FROM openjdk:8-alpine

WORKDIR /netrunner
COPY --from=builder /netrunner/target/netrunner-standalone.jar .
COPY --from=builder /netrunner/dev.edn .
WORKDIR /netrunner/resources
COPY --from=builder /netrunner/resources .
WORKDIR /netrunner/data
COPY --from=builder /netrunner/data .
WORKDIR /netrunner/src
COPY --from=builder /netrunner/src .

WORKDIR /netrunner
ENTRYPOINT ["java", "-jar", "netrunner-standalone.jar"]
