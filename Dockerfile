FROM rocker/tidyverse:4.3.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "\
  install.packages(c( \
    'data.table', \
    'corrplot', \
    'janitor', \
    'cluster', \
    'rpart', \
    'rpart.plot', \
    'randomForest', \
    'writexl' \
  ), repos='https://cloud.r-project.org') \
"

RUN mkdir -p /app /data /data/outputs /data/outputs/plots
RUN touch /data/outputs/test.txt
RUN touch /data/outputs/plots/test.txt

VOLUME ["/data/outputs"]

COPY scripts/main.R                     /app/main.R
COPY scripts/api_weather_merge.R        /app/api_weather_merge.R
COPY scripts/data_cleaing.R             /app/data_cleaning.R
COPY scripts/data_cleaning_2.R          /app/data_cleaning_2.R
COPY scripts/exploratory_data_analysis.R /app/exploratory_data_analysis.R
COPY scripts/descriptive_modelling.R    /app/descriptive_modelling.R
COPY scripts/classification_model.R     /app/classification_model.R
COPY data/Crime_Data_from_2020_to_Present.csv /data/Crime_Data_from_2020_to_Present.csv

ENV DATA_DIR=/data
ENV OUTPUT_DIR=/data/outputs
ENV SCRIPTS_DIR=/app

CMD ["Rscript", "/app/main.R"]