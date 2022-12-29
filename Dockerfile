FROM rocker/r-ver:4.1.0

WORKDIR /project

CMD R -e "print('Hello, World!')"