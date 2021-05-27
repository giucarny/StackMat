# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Data (EES 2019 Voter Study, Italian Sample) 
# Author: G.Carteny
# last update: 2021-05-25
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Admin # =============================================================================================

# Create a string vector containing the name of the required packages # - - - - - - - - - - - - - - - -
want = c("tidyverse", "magrittr", "haven", "data.table", "labelled")

# Load the packages if installed, otherwise install and load them # - - - - - - - - - - - - - - - - - -
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)

# Prevent scientific notation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
options(scipen = 99)

# Remove all the objects from the environment # - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rm(list = ls())

# Load auxiliary functions # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (grepl('StackMat-master', getwd())==F) {
  setwd(paste0(getwd(), '/StackMat-master/'))
  source(paste0(getwd(), '/scripts/', 'EES2019_stacking_functions.R'))
} else {
  if (grepl('scripts', getwd())==F) {
    source(paste0(getwd(), '/scripts/', 'EES2019_stacking_functions.R'))
  } else {
    source('EES2019_stacking_functions.R')
  }
}


# Load data # =========================================================================================

# Load the EES 2019 voter study dataset (Stata version) # - - - - - - - - - - - - - - - - - - - - - - -
EES2019 <- haven::read_dta(paste0(getwd(), '/data/' ,'ZA7581_v1-0-0.dta'))

EES2019 <- haven::zap_labels(EES2019) # Remove labels


# Select country-specific data frames for stacking # ==================================================

EES2019_it <- EES2019 %>% dplyr::filter(countrycode==1380) # EES2019 Italian voter study
rm(EES2019)


# Select the relevant variables # =====================================================================

EES2019_it %<>% dplyr::select(respid, D3, D4_1, EDU, 
                              Q7, starts_with('q10'), 
                              Q11, starts_with('q13'),  
                              Q23, starts_with('q24'), Q25)


# Create additional variables # =======================================================================

# Create an 'age' variable - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
EES2019_it %<>% 
  dplyr::mutate(year = 2019) %>%     
  dplyr::mutate(age = year-D4_1) %>%  
  dplyr::filter(age>17) %>%
  dplyr::select(-c('D4_1'))

# Rename gender and education variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
names(EES2019_it)[names(EES2019_it)=='D3'] <- 'gndr'
names(EES2019_it)[names(EES2019_it)=='EDU'] <- 'edu'

# Select the relevant parties # =======================================================================

rel_prties <- 1501:1507 %>% as.numeric()

# LR distance # =======================================================================================

# Drop variable related to non-relevant parties - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>% dplyr::select(-c(q13_8, q13_9))


# Recode missing values - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it %<>% 
  dplyr::mutate(Q11 = case_when(Q11 > 10 ~ NA_real_, 
                                T ~ Q11))            

EES2019_it %<>% 
  dplyr::mutate(across(starts_with('q13'), ~case_when(.>10 ~ NA_real_,
                                                      T ~ .)))

# Generate mean values of party positions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  mutate(across(starts_with('q13'), list(mean = ~mean(., na.rm=T))))


# Generate LR distance variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it <- cbind(EES2019_it,
                    gendist(data = EES2019_it, 
                            indices = 1:7, 
                            stub = 'q13'))


# Drop the variables used for computing the distances - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::select(-c(Q11, 
                   ends_with('mean'),
                   paste0('q13_', seq(1,7,1))))


# Rename the generated variables for stacking - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

names(EES2019_it)[endsWith(colnames(EES2019_it), 'dist')] <- paste0('q13_', seq(1501, 1507, 1))


# EU integration distance # ===========================================================================

# Drop variable related to non-relevant parties - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>% dplyr::select(-c(q24_8, q24_9))


# Recode missing values - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it %<>% 
  dplyr::mutate(Q23 = case_when(Q23 > 10 ~ NA_real_, 
                                T ~ Q23))            

EES2019_it %<>% 
  dplyr::mutate(across(starts_with('q24'), ~case_when(.>10 ~ NA_real_,
                                                      T ~ .)))

# Generate mean values of party positions on EU integration - - - - - - - - - - - - - - - - - - - - - -

EES2019_it %<>%
  mutate(across(starts_with('q24'), list(mean = ~mean(., na.rm=T))))


# Generate EU integration distance variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it <- cbind(EES2019_it,
                    gendist(data = EES2019_it, 
                            indices = 1:7, 
                            stub = 'q24'))


# Drop the variables used for computing the distances - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::select(-c(Q23, 
                   ends_with('mean'),
                   paste0('q24_', seq(1,7,1))))


# Rename the generated variables for stacking - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

names(EES2019_it)[endsWith(colnames(EES2019_it), 'dist')] <- paste0('q24_', seq(1501, 1507, 1))


# Party identification # ==============================================================================

# Recode the party identification variable - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>% 
  dplyr::mutate(Q25 = case_when(Q25 >=1508  ~ NA_real_, 
                                Q25 < 100 ~ NA_real_,
                                T ~ Q25))  

EES2019_it <- cbind(EES2019_it, 
                    gendicovar(data = EES2019_it,
                               indices = 1501:1507, 
                               stub = 'Q25'))


# Dependent variables =================================================================================

# Recode the EP elections vote choice variable - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::mutate(Q7 = case_when(Q7>=1508 ~ NA_real_, 
                               Q7<100 ~ NA_real_,
                               T ~ Q7))

# Replace values bigger than 10 in the PTV var.s - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>% 
  dplyr::mutate(across(starts_with('q10_'), ~case_when(.>10 ~ NA_real_, 
                                                       T ~ .)))

# Drop the empty PTV variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it %<>% dplyr::select(-c(paste0('q10_', seq(8,10,1))))

# Rename the PTV variables for stacking - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

names(EES2019_it)[startsWith(colnames(EES2019_it), 'q10_')] <- paste0('q10_', seq(1501, 1507, 1))


# Generate a set of dichotomous variables from the EP vote choice one - - - - - - - - - - - - - - - -

EES2019_it <- cbind(EES2019_it, 
                    gendicovar(data = EES2019_it,
                               indices = 1501:1507, 
                               stub = 'Q7'))

rm(list=ls(pattern='df'))


# Sociodemographic yhats ==============================================================================

# Recode edu variable - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it %<>%
  dplyr::mutate(edu = case_when(edu>3 ~ NA_real_, 
                                T ~ edu))

# Create a set of dummy variables for edu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::mutate(edu1 = case_when(edu==1 ~ 1, 
                                 edu==2 | edu==3 ~ 0, 
                                 T ~ edu), 
                edu2 = case_when(edu==2 ~ 1, 
                                 edu==1 | edu==3 ~ 0, 
                                 T ~ edu), 
                edu3 = case_when(edu==3 ~ 1, 
                                 edu==1 | edu==2 ~ 0, 
                                 T ~ edu)) 


# Recode gender (category 3 with too few observations) - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it %<>% 
  dplyr::mutate(gndr = case_when(gndr==3 ~ NA_real_, 
                                 T ~ gndr))

# Age yhats - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Generate yhats for a dichotomous dependent variable

depvars <- paste0('Q7_', rel_prties)

for(i in depvars) {
  EES2019_it <- genyhats(data=EES2019_it, 
                         depvar=i, 
                         regtype='log',
                         indvar = c("age"),
                         newname = 'age')  
}
rm(i, depvars)






# Stack the observations ==============================================================================

EES2019_it_stacked <- genstacks(data = EES2019_it, 
                                idvar = 'respid', 
                                stubs = c('q10', 'q13', 'Q7', 'genderage_yhat'),
                                keepvar = c('gndr', 'age'))

# Mutate the dataset ==================================================================================

# Rename the variables, generate a party-voter 'dyad' variable, keep some variables, and reorder # - - 

EES2019_it_stacked %<>%
  dplyr::mutate(respid = Var1, 
                party = Var2, 
                ptv = q10, 
                lr_dist = q13,
                stacked_vc = Q7, 
                dyad = paste0(respid, "-", party)) %>%
  dplyr::select(dyad, respid, party, ptv, stacked_vc, lr_dist, genderage_yhat, gndr, age)




