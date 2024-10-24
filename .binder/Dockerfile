FROM rocker/binder:4.4.1
LABEL maintainer='Enno Winkler'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/Enno-W/BAEW/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2024-06-15/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
