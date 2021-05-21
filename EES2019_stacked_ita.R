# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Data (EES 2019 Voter Study, Italian Sample) 
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


# Download the data # =================================================================================

if (!file.exists('ZA7581_v1-0-0.dta')) {
  
}

utils::download.file(
  url= 'https://drive.google.com/file/d/1yNrZOWV2HiKtiBYfMd9PP7jHNTeu_bq1/view?usp=sharing',
  destfile = 'ZA7581_v1-0-0.dta'
)

haven::read_dta('ZA7581_v1-0-0.dta')