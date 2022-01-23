FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev python3 python3-pip && rm -rf /var/lib/apt/lists/*
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
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
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
