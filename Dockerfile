# Base R Shiny image
FROM rocker/shiny:4.4.0


RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libmysqlclient21  \
    iputils-ping

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean


## app folder
COPY /shiny_app ./app

# Copy the health check script to the container
COPY shiny_app/healthcheck.r /usr/local/bin/healthcheck.R
RUN chmod +x /usr/local/bin/healthcheck.R

ENV R_CONFIG_ACTIVE=production

RUN R -e "install.packages(c('shiny', 'DT', 'DBI', 'RSQLite', 'shinyjs', 'shinycssloaders', 'lubridate', 'shinyFeedback', 'dplyr', 'dbplyr', 'polished', 'RMariaDB', 'EHRtemporalVariability', 'config','pool', 'glue', 'stringr', 'httr', 'jsonlite', 'xml2', 'utils'))"

# expose port
EXPOSE 3838


#running healthcheck
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 CMD ["Rscript", "/usr/local/bin/healthcheck.R"]


# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]