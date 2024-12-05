FROM rocker/verse

RUN r -e "install.packages(c('pak', 'renv'))"

COPY renv.lock renv.lock
RUN r -e "renv::restore()"

