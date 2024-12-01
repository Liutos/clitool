#!/bin/bash
bash ./build.sh

# 清理旧的镜像。
docker images | grep '<none>' | awk '{ print $3 }' | xargs docker rmi

docker run --rm \
-e MYSQL_DATABASE_NAME="${MYSQL_DATABASE_NAME}" \
-e MYSQL_HOST="${MYSQL_HOST}" \
-e MYSQL_PASSWORD="${MYSQL_PASSWORD}" \
-e MYSQL_USERNAME="${MYSQL_USERNAME}" \
-p 4242:4242 \
-v `pwd`:/app/ \
cl-accounting:latest
