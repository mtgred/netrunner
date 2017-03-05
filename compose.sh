#!/bin/bash

set -e

usage () {
    cat<<EOF
Usage: $0 [-b]

  -b   build netrunner-node image
EOF
}

while getopts ':b' o; do
    case "${o}" in
        b)
            BUILD=true
            ;;
        *)
            usage
            ;;
    esac
done

if [[ -n ${BUILD} ]]; then
    docker build -t netrunner-node .
fi

docker-compose up npm-install
docker-compose up -d mongo
sleep 30

docker-compose up fetch-cards

docker-compose up -d coffee-server stylus-css

docker-compose up lein-netrunner lein-cljs
