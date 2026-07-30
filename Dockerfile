# Base R Shiny image (includes R + Shiny, runs app as PID 1)
FROM rocker/shiny:4.5.1

# Install system dependencies required by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libwebp-dev \
    libcurl4-openssl-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /home/rstudio/rcbc-shiny

# Copy renv infrastructure files first (for layer caching)
RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install renv globally so .Rprofile source works with Rscript
RUN R -s -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Now run renv::restore() (renv will be found via .Rprofile)
RUN R -s -e "renv::restore()"

# Copy the rest of the application
COPY . .

# Expose the port the Shiny app will run on
EXPOSE 3838

# Set working directory and env var for Rscript to find renv
ENV R_PROFILE_USER=/home/rstudio/rcbc-shiny/.Rprofile

# Run the Shiny app as PID 1 (persists as long as container runs)
CMD ["Rscript", "-e", "library(shiny); shiny::runApp('/home/rstudio/rcbc-shiny', port=3838, host='0.0.0.0')"]
