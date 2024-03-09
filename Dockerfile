FROM eddelbuettel/r2u:22.04

WORKDIR /opt/rstats

RUN echo "Updating deps... $CACHEBUST"
RUN apt-get update
ADD dependencies.txt .
RUN apt-get install -y $(cat dependencies.txt)

# R deps
ADD dependencies_r.txt .
ADD install_r_deps.sh .
RUN /opt/rstats/install_r_deps.sh

COPY src/ .

EXPOSE 3000
CMD [ "Rscript", "serve.r"]
