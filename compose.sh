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

docker-compose up fetch-cards

docker-compose up lein-netrunner coffee-server stylus-css lein-cljs
