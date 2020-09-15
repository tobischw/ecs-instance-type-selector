FROM node:14.10.0-stretch-slim

WORKDIR /ecs-instance-type-selector
COPY . .

RUN npm install -g serve --unsafe-perm=true

EXPOSE 3000
CMD serve -s build
