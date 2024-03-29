#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
  while IFS= read -r pkg; do
    Rscript -e "install.packages('${pkg}', Ncpus=4, repos='https://cloud.r-project.org/')"
  done <"dependencies_r.txt"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  while IFS= read -r pkg; do
    libs="${libs} ${pkg}"
  done <"dependencies_r.txt"
  install.r "$libs"
fi
