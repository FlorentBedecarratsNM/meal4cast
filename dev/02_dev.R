# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "magrittr" )
usethis::use_pipe(export = TRUE)
usethis::use_package( "fs" )
usethis::use_package( "reticulate" )
usethis::use_package( "arrow" )
usethis::use_package( "purrr" )
usethis::use_package( "aws.s3" )
usethis::use_package( "dplyr" )
usethis::use_package( "stringr" )
usethis::use_package( "tidyr" )
usethis::use_package( "stringi" )
usethis::use_package( "lubridate" )
usethis::use_package( "forcats" )
usethis::use_package( "shinyalert" )
usethis::use_package( "readr" )
usethis::use_package( "bslib" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "admin_list_files" ) # Name of the module
golem::add_module( name = "model_fetch_xgboost" ) # Name of the module
golem::add_module( name = "model_source_xgboost" ) # Name of the module
golem::add_module( name = "sync_s3_output" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module
golem::add_module( name = "ui_01_browse_data" ) # Name of the module
golem::add_module( name = "ui_02_load_data" ) # Name of the module
golem::add_module( name = "ui_03_run_model" ) # Name of the module
golem::add_module( name = "ui_04_monitor" ) # Name of the module


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_fct( "run_xgboost" )
golem::add_fct( "load_results" )
golem::add_fct( "check_results_fresh" )
golem::add_fct( "load_traindata" )
golem::add_fct( "check_traindata_fresh" )
golem::add_fct( "gen_piv" )
golem::add_fct( "compute_availability" )
golem::add_fct( "transform_fusion" )
golem::add_fct( "load_fusion" )
golem::add_fct( "gen_schoolyears" )
golem::add_fct( "update_mapping_cafet_freq" )
golem::add_fct( "not_in" )




golem::add_utils( "set_config_variables" )
golem::add_utils( "gen_opendata_url" )
golem::add_utils( "create_folder" )

golem::add_fct( "fetch_xgb_model" ) 

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("meal4cast")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

