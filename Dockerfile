FROM ubuntu

WORKDIR /ecs-instance-type-selector
COPY . .

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y nodejs && \
    apt-get install -y npm

RUN npm install elm -g --unsafe-perm=true
RUN npm install -g create-elm-app --unsafe-perm=true

CMD ["elm-app" , "start"]
EXPOSE 3000
