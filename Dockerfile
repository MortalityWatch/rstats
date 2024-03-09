FROM eddelbuettel/r2u:22.04

WORKDIR /opt/rstats

RUN echo "Updating deps... $CACHEBUST"
RUN apt-get update
ADD dependencies.txt .
RUN apt-get install -y $(cat dependencies.txt)

ENV NODE_VERSION="20.x"

# Install NodeJS 20
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VERSION} | bash -
RUN apt-get install -y nodejs

# R deps
ADD dependencies_r.txt .
ADD install_r_deps.sh .
RUN /opt/rstats/install_r_deps.sh

COPY package.json package-lock.json .
RUN npm install
COPY src/ .

EXPOSE 80
CMD [ "npm", "start"]
