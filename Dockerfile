FROM node:12-alpine

expose 6789

RUN yarn global add parcel-bundler elm

WORKDIR /project

COPY . .

RUN yarn install --production

RUN pwd && ls -a


RUN parcel build src/index.html


CMD ["node", "server.js"]
