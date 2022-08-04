FROM clojure

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

RUN chmod -R 777 /root/

# Install npm+dependencies
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash -
RUN apt-get install nodejs
COPY package.json /usr/src/app/
COPY package-lock.json /usr/src/app/
RUN npm ci

# Install clojure dependencies
COPY project.clj /usr/src/app/
RUN lein deps