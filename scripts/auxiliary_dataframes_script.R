# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Auxiliary Dataframe(s) for Stata (EES ~ AUX ~ CHES)
# Author: G.Carteny
# last update: 2021-05-30
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Admin # =============================================================================================

# Create a string vector containing the name of the required packages # - - - - - - - - - - - - - - - -
want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "fs")

# Load the packages if installed, otherwise install and load them # - - - - - - - - - - - - - - - - - -
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)

# Prevent scientific notation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
options(scipen = 99)

# Remove all the objects from the environment # - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rm(list = ls())

# Set wd # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

if (grepl('StackMat-master', getwd()) | grepl('StackMat', getwd())) {
  getwd()
} else if (dir.exists(paste0(getwd(), '/Stackmat-master/'))) {
  setwd(paste0(getwd(), '/Stackmat-master/'))
} else {
  warning('Set the working directory in the ~/Stackmat or ~/Stackmat-master/ folder')
}


# Load data # =========================================================================================

# EES 2019 voter study dataset # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
EES2019 <- 
  haven::read_dta(paste0(getwd(), '/data/' ,'ZA7581_v1-0-0.dta')) 

# Load an auxiliary data frame, containing party id variables (EES, CHES, CMP), vote shares (for first 
# and second order elections),... # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_aux <- data.table::fread(paste0(getwd(), '/data/' ,'ZA7581_cp_mod.csv'), na.strings = '') 

# Filter auxiliary dataframe # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_aux %<>% 
  dplyr::filter(year==2019,           # Select the European Parliament elections year
                fs_order==2)          # Select only second order elections           


# Load the CHES dataset - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

CHES <- haven::read_dta(paste0(getwd(), '/data/' ,"1999-2019_CHES_dataset_means(v1).dta"))


# Select relevant variables for auxiliary dataframe for Stata # ======================================

# Select CHES relevant variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

id_vars <- c('country', 'party_id', 'party') # 'eastwest', 'eumember', <- if you want some broad contextuals

contxt_vars <- c('lrgen', 'eu_position') # , 'sociallifestyle', 'redistribution', 'immigrate_policy'

# cntxt_vars_newnms <- c('q13_ches', 'q24_ches')

CHES2019 <- CHES %>% 
  filter(year==2019) %>%
  dplyr::select(all_of(id_vars),
                all_of(contxt_vars))
  

rm(CHES, id_vars) 


# Select the EES relevant variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 %<>%
  dplyr::select(respid, countrycode, Q7) 


# Select the auxiliary dataframe relevant variables # - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_aux %<>%
  dplyr::select(countrycode, Q7, party_id)


# Join the dataframes # =============================================================================

EES2019 <- left_join(EES2019, EES2019_aux)

EES2019 <- left_join(EES2019, CHES2019)

# Re-select and rescale relevant variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 %<>% 
  dplyr::select(respid, countrycode, Q7, all_of(contxt_vars)) %>%
  zap_labels(.) %>%
  dplyr::mutate(across(all_of(contxt_vars), ~./10))

# names(EES2019)[names(EES2019) %in% contxt_vars] <- cntxt_vars_newnms

rm(list=ls(pattern='contxt_vars'))

# Save the data frame # =============================================================================

haven::write_dta(data = EES2019, path(paste0(getwd(), '/data/' ,"EES_CHES_2019_aux.dta")))

rm(list=ls(pattern="CHES|aux"))

# EES codebook for PID recoding # =====================================================================

EES_codebook <- 
  data.table::fread(paste0(getwd(), '/data/' ,'ZA7581_cp.csv')) %>%
  haven::zap_labels(.) %>%
  dplyr::mutate(Q25 = q25, 
                Q25_rec = Q7,
                Q10_PTV = case_when(Q10_PTV=='' ~ NA_character_,
                                    T ~ Q10_PTV)) %>% 
  dplyr::select(countrycode, Q10_PTV, Q25, Q25_rec) %>%
  na.omit() %>% 
  dplyr::select(countrycode, Q25, Q25_rec) %>%
  dplyr::mutate(across(names(.), ~as.numeric(.)))


# Save the data frame # =============================================================================

haven::write_dta(data = EES_codebook, path(paste0(getwd(), '/data/' ,"EES_2019_Q25_aux.dta")))

rm(list=ls(pattern="CHES|aux"))
