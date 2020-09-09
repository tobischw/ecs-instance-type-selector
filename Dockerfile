FROM node:alpine

WORKDIR /ecs-instance-type-selector
COPY . .

RUN npm install elm -g --unsafe-perm=true
RUN npm install -g create-elm-app --unsafe-perm=true
RUN npm install -g serve --unsafe-perm=true

EXPOSE 3000
CMD elm-app build && serve -s build -p 3000
