FROM rocker/tidyverse:latest
# FROM rocker/r-base:latest

WORKDIR /project

# R package dependencies
# RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libgdal-dev libudunits2-dev
RUN apt-get update && apt-get install -y libgdal-dev libudunits2-dev

# renv
RUN mkdir -p renv
COPY Code/CaseStudy2/renv.lock renv.lock
COPY Code/CaseStudy2/.Rprofile .Rprofile
COPY Code/CaseStudy2/renv/activate.R renv/activate.R
COPY Code/CaseStudy2/renv/settings.dcf renv/settings.dcf

RUN R -e "renv::restore()"

# DIMEX source code
COPY Code/CaseStudy2 .

CMD Rscript cli.R --help
