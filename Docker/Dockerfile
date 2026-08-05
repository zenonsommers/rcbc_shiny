FROM rocker/verse:4.5.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libwebp-dev \
    libcurl4-openssl-dev \
    libuv1-dev \
    libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

COPY . .

RUN R -e 'renv::restore()'

CMD ["Rscript", "-e", "shiny::runApp('app.R', port = as.integer(Sys.getenv('SHINY_PORT', '3838')), host = '0.0.0.0')"]
