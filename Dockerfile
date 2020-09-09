FROM node:14.10.0-stretch-slim

WORKDIR /ecs-instance-type-selector
COPY . .

RUN npm install elm
RUN npm install create-elm-app

EXPOSE 3000
CMD elm-app start
