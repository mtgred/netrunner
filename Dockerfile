FROM node:latest

RUN apt-get update && apt-get install -y libzmq3-dev

VOLUME /netrunner

WORKDIR /netrunner
