#!/bin/bash

set -e

docker-compose up npm

docker-compose up netrunner coffee stylus lein
