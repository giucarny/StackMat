# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Setup Script for Stacking Data 
# Author: G.Carteny
# last update: 2021-05-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Admin # =============================================================================================

# Install and load the relevant packages # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

want = c("tidyverse", "reshape2", "gridExtra", "magrittr", "haven", "data.table", "labelled", "utils")

have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)

options(scipen = 99)
rm(list = ls())


# Download data and scripts # =========================================================================

# Wrapping function for downloading relevant files # - - - - - - - - - - - - - - - - - - - - - - - - -

dwnld.files <- function() {
  utils::download.file(url = "https://github.com/giucarny/StackMat/archive/refs/heads/master.zip"
                       , destfile = "StackMat-master.zip")
  utils::unzip(zipfile = 'StackMat-master.zip')
  file.remove('StackMat-master.zip')
}

# Check whether directories and/or files do exist, otherwise download them # - - - - - - - - - - - - - 

if (!dir.exists(paths = 'StackMat-master')) {
  dwnld.files()
} else {
  if (!dir.exists(paste0(getwd(), '/StackMat-master/data/')) | 
      !dir.exists(paste0(getwd(), '/StackMat-master/scripts/'))) {
    dwnld.files()
  } else {
    data.dir.fls <- paste0(paste0(getwd(), '/StackMat-master/data/'), 
                           c('EES2019_Questionnaire.pdf', 'ZA7581_v1-0-0.dta'))
    script.dir.fls <- paste0(paste0(getwd(), '/StackMat-master/scripts/'), 
                             c('EES2019_stacking_script.R'))
    if (!all(file.exists(data.dir.fls)) | !all(file.exists(script.dir.fls))) {
      dwnld.files()
    }
  }
}


rm(list=ls())

