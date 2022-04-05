FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libpng-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc python unixodbc-dev zlib1g-dev python3 python3-pip python3-venv && rm -rf /var/lib/apt/lists/*
# RUN pip install virtualenv
# RUN virtualenv venv
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
RUN pip install pandas==1.1.0
RUN pip install numpy==1.19.1
RUN pip install xgboost==1.1.1
RUN pip install scikit-learn==0.23.1
RUN pip install dask[dataframe]==0.19.4
RUN pip install lunardate==0.2.0
RUN pip install convertdate==2.2.1
RUN pip install matplotlib==3.2.1
RUN pip install python-dateutil==2.8.1
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("stringi",upgrade="never", version = "1.7.6")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.2")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.1")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.8")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.4")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("reticulate",upgrade="never", version = "1.24")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("readODS",upgrade="never", version = "1.7.0")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.0")'
RUN Rscript -e 'remotes::install_version("odbc",upgrade="never", version = "1.3.3")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("forcats",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.20")'
RUN Rscript -e 'remotes::install_version("aws.s3",upgrade="never", version = "0.3.21")'
RUN Rscript -e 'remotes::install_version("arrow",upgrade="never", version = "7.0.0")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
ADD https://github.com/nantesmetropole/school_meal_forecast_xgboost/archive/refs/heads/dev.zip ./
RUN unzip dev.zip
RUN mv school_meal_forecast_xgboost-dev/app/ .
RUN mv school_meal_forecast_xgboost-dev/tests/ .
RUN mv school_meal_forecast_xgboost-dev/main.py .
RUN rm -rf school_meal_forecast_xgboost-dev
RUN rm dev.zip
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');meal4cast::run_app()"
