FROM rocker/binder:4.4.1
LABEL maintainer='your_name'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/Enno-W/BAEW/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2024-10-22/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
