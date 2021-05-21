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


if (!dir.exists(paths = 'StackMat-master')) {
  utils::download.file(url = "https://github.com/giucarny/StackMat/archive/refs/heads/master.zip"
                       , destfile = "StackMat-master.zip")
  utils::unzip(zipfile = 'StackMat-master.zip')
  file.remove('StackMat-master.zip')
  
} else {
  fls <- c('')
}




