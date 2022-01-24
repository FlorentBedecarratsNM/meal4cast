# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ---- 
## This will build a tar.gz that can be installed locally, 
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile(extra_sysreqs = c("python3", "python3-pip",
                                        "python3-venv"))
## The following lines need to be added to the dockerfile
# python3 python3-pip ?

insert_lines <- function(target_file = "Dockerfile", before = "EXPOSE", suffix = "", 
                         to_add = "blabla") {
  my_file <- readr::read_lines(target_file)
  before_line <- which(stringr::str_starts(my_file, before))
  out <- c(my_file[1:before_line-1], to_add, my_file[before_line:length(my_file)])
  writeLines(out, paste0(target_file, suffix), )
}

insert_lines(before = "EXPOSE ", 
             to_add = c("ADD https://github.com/nantesmetropole/school_meal_forecast_xgboost/archive/refs/heads/dev.zip ./",
                        "RUN unzip dev.zip",
                        "RUN mv school_meal_forecast_xgboost-dev/app/ .",
                        "RUN mv school_meal_forecast_xgboost-dev/tests/ .",
                        "RUN mv school_meal_forecast_xgboost-dev/main.py .",
                        "RUN rm -rf school_meal_forecast_xgboost-dev",
                        "RUN rm dev.zip"))

insert_lines(before = "RUN echo", 
             to_add = c("# RUN pip install virtualenv",
                        "# RUN virtualenv venv",
                        "RUN python3 -m venv /opt/venv",
                        'ENV PATH="/opt/venv/bin:$PATH"',
                        "RUN pip install pandas==1.1.0",
                        "RUN pip install numpy==1.19.1",
                        "RUN pip install xgboost==1.1.1",
                        "RUN pip install scikit-learn==0.23.1",
                        "RUN pip install dask[dataframe]==0.19.4",
                        "RUN pip install lunardate==0.2.0",
                        "RUN pip install convertdate==2.2.1",
                        "RUN pip install matplotlib==3.2.1",
                        "RUN pip install python-dateutil==2.8.1"))

## If you want to deploy to ShinyProxy
golem::add_dockerfile_shinyproxy()

## If you want to deploy to Heroku
golem::add_dockerfile_heroku()
