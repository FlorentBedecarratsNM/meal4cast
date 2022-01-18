#!/bin/sh

# Create variables
PROJ_NAME=meal4cast
WORK_DIR=/home/rstudio/$PROJ_NAME
REPO_URL=https://github.com/FlorentBedecarratsNM/$PROJ_NAME # As initial
# DATA_DIR=${WORK_DIR}/data # for later

# Git
git clone $REPO_URL $WORK_DIR
chown -R rstudio:users $WORK_DIR

# Folders to store data and documentation
# mkdir $DATA_DIR # for later

# copy files
# mc cp s3/fbedecarrats/diffusion/{FD_MOBPRO_2018.csv,commune2021.csv,Intercommunalite-Metropole_au_01-01-2021.xlsx,Varmod_MOBPRO_2018.csv} $DATA_DIR # for later

# launch RStudio in the right project
# Copied from InseeLab UtilitR
    echo \
    "
    setHook('rstudio.sessionInit', function(newSession) {
        if (newSession && identical(getwd(), path.expand('~')))
        {
            message('On charge directement le bon projet :-) ')
            rstudioapi::openProject('~/$PROJ_NAME')
            rstudioapi::applyTheme('Merbivore')
            }
            }, action = 'append')
            " >> /home/rstudio/.Rprofile