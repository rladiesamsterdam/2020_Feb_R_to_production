
# Select base-image
FROM rocker/r-apt:bionic

# Update apt
RUN apt-get update

# Install binary packages
RUN apt-get install -y -qq r-cran-shiny r-cran-shinydashboard r-cran-tidyverse r-cran-magrittr r-cran-lubridate r-cran-leaflet r-cran-dt

# Make a new directory for storing your app. Call it "app"
RUN mkdir /app

# Copy app to new directory "app"
COPY KNMIapp.R /app/

# Create a subfolder input in directory "app"
RUN mkdir /app/data

# Copy input folder to subfolder input in directory "app"
COPY data/*.tsv /app/data/

# select port
EXPOSE 12092

# Change directory
WORKDIR /app

# Run app
CMD ["Rscript", "/app/KNMIapp.R"]
