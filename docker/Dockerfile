FROM clojure:latest

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

RUN chmod -R 777 /root/

# Install npm+dependencies
ENV NODE_VERSION=19.6.0
RUN apt update && apt install -y curl
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
ENV NVM_DIR=/root/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
COPY package.json /usr/src/app/
COPY package-lock.json /usr/src/app/
RUN npm ci

# Install clojure dependencies
COPY project.clj /usr/src/app/
RUN lein deps
