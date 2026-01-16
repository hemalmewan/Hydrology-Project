FROM rocker/geospatial:latest

##install system libraries required for R packages
RUN apt-get update && apt-get install -y \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

##install R packages
RUN R -e "install.packages(c('shiny','shinydashboard','zoo','DBI','RPostgres','pool','DT','terra','tmap','future','furrr','Kendall'), repos='https://cran.rstudio.com/')"

##copy app code and data into the docker image
COPY app.R /srv/shiny-server/


##Expose the port for the shiny app
EXPOSE 3838

##run the shiny app
CMD ["R","-e","shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]