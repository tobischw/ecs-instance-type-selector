FROM node:alpine

WORKDIR /ecs-instance-type-selector
COPY . .

RUN npm install elm -g --unsafe-perm=true
RUN npm install -g create-elm-app --unsafe-perm=true

EXPOSE 3000
CMD ["elm-app" , "start"]
