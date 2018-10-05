FROM clojure:lein-alpine as build

RUN apk --no-cache add \
    git \
    nodejs \
    nodejs-npm

RUN npm install -g bower stylus

WORKDIR /netrunner
COPY . .
RUN lein deps
RUN bower --allow-root install
RUN stylus src/css -o resources/public/css/
RUN lein cljsbuild once prod
RUN lein uberjar

# Final image
FROM clojure:lein-alpine
LABEL maintainer="John Warwick <jwarwick@gmail.com>"

WORKDIR /netrunner
COPY --from=build /netrunner/dev.edn /netrunner/dev.edn
COPY --from=build /netrunner/data /netrunner/data
COPY --from=build /netrunner/resources /netrunner/resources
COPY --from=build /netrunner/target/netrunner-standalone.jar /netrunner/target/netrunner-standalone.jar

ENTRYPOINT ["java", "-jar", "target/netrunner-standalone.jar"]
