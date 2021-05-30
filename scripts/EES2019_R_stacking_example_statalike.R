# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Data (EES 2019 Voter Study, Italian Sample) 
# Author: G.Carteny
# last update: 2021-05-27
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

# Load auxiliary functions # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

if (grepl('StackMat-master', getwd()) | grepl('StackMat', getwd())) {
  source(paste0(getwd(), '/scripts/', 'EES2019_stacking_functions.R'))
} else if (dir.exists(paste0(getwd(), '/Stackmat-master/'))) {
  setwd(paste0(getwd(), '/Stackmat-master/'))
  source(paste0(getwd(), '/scripts/', 'EES2019_stacking_functions.R'))
} else {
  warning('Set the working directory in the ~/Stackmat or ~/Stackmat-master/ folder')
}


# Load data # =========================================================================================

# Load the EES 2019 voter study dataset (Stata version) # - - - - - - - - - - - - - - - - - - - - - - -
EES2019 <- 
  haven::read_dta(paste0(getwd(), '/data/' ,'ZA7581_v1-0-0.dta')) %>%
  haven::zap_labels(.)


# Load an auxiliary dataset (CHES) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_aux <- 
  haven::read_dta(paste0(getwd(), '/data/' ,'EES_CHES_2019_aux.dta')) %>%
  haven::zap_labels(.)


# Join the datasets by 'respid', 'countrycode', and 'Q7'

EES2019 <- left_join(EES2019, EES2019_aux)

# Select country-specific data frames for stacking # ==================================================

EES2019_it <- EES2019 %>% dplyr::filter(countrycode==1380) # EES2019 Italian voter study
rm(EES2019)


# Select the relevant variables # =====================================================================

EES2019_it %<>% dplyr::select(respid, D3, D4_1, EDU, 
                              Q7, starts_with('q10'), 
                              Q11, starts_with('q13'),  
                              Q23, starts_with('q24'), 
                              Q25, Q26,
                              lrgen, eu_position)


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



# Recode the pid strength variable # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>% 
  dplyr::mutate(Q26 = case_when(Q26==0 ~ 0, 
                                Q26==1 ~ 3,
                                Q26==3 ~ 1, 
                                T ~ Q26)) 


# Generate the stacked pid strenght variable # - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it <- cbind(EES2019_it, 
                    genstackedvar(EES2019_it, indices = 1501:1507, stub1 = 'Q26', stub2 = 'Q25'))


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

# Rescale the PTV variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>% 
  dplyr::mutate(across(starts_with('q10_'), ~./10))

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


# Generate rescaled versions of the original individual variables - - - - - - - - - - - - - - - - - - -

EES2019_it %<>%
  dplyr::mutate(Q11 = Q11/10) %>%
  dplyr::mutate(across(starts_with('q13'), ~./10))

# Generate mean values of party positions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::mutate(across(starts_with('q13'), list(mean = ~mean(., na.rm=T))))

names(EES2019_it)[endsWith(colnames(EES2019_it), 'mean')] <- paste0('q13_', 'mean_', seq(1501, 1507, 1))


# Generate mean values of party positions based on ches var # - - - - - - - - - - - - - - - - - - - -

lrgen_df <- 
  genstackedvar(EES2019_it, indices = 1501:1507, stub1 = 'lrgen', stub2 = 'Q7') %>%
  as_tibble() %>%
  mutate(across(names(.), ~case_when(.==0 ~ NA_real_, T~.))) %>%
  mutate(across(names(.), ~mean(., na.rm = T))) 

EES2019_it <- cbind(EES2019_it, lrgen_df)

rm(lrgen_df)


# Generate LR distance variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_it <- cbind(EES2019_it,
                    gendist(data = EES2019_it, 
                            indices = 1501:1507, 
                            stub1 = 'Q11',
                            stub2 = 'q13_mean_'))

# Rename the generated variables for stacking 
names(EES2019_it)[endsWith(colnames(EES2019_it), 'dist')] <- paste0('q13_', 'dist_', seq(1501, 1507, 1))

EES2019_it <- cbind(EES2019_it,
                    gendist(data = EES2019_it, 
                            indices = 1501:1507, 
                            stub1 = 'Q11',
                            stub2 = 'lrgen_stack_'))

# Rename the generated variables for stacking 
names(EES2019_it)[endsWith(colnames(EES2019_it), 'dist')] <- paste0('lrgen_', 'dist_', seq(1501, 1507, 1))


# Drop the variables used for computing the distances - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::select(-c(paste0('q13_', seq(1,7,1))))



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


# Generate rescaled versions of the original individual variables - - - - - - - - - - - - - - - - - - -

EES2019_it %<>%
  dplyr::mutate(Q23 = Q23/10) %>%
  dplyr::mutate(across(starts_with('q24'), ~./10))

# Generate mean values of party positions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::mutate(across(starts_with('q24'), list(mean = ~mean(., na.rm=T))))

names(EES2019_it)[endsWith(colnames(EES2019_it), 'mean')] <- paste0('q24_', 'mean_', seq(1501, 1507, 1))

# Generate mean values of party positions based on ches var # - - - - - - - - - - - - - - - - - - - -

eu_position_df <- 
  genstackedvar(EES2019_it, indices = 1501:1507, stub1 = 'eu_position', stub2 = 'Q7') %>%
  as_tibble() %>%
  mutate(across(names(.), ~case_when(.==0 ~ NA_real_, T~.))) %>%
  mutate(across(names(.), ~mean(., na.rm = T))) 

EES2019_it <- cbind(EES2019_it, eu_position_df)

rm(eu_position_df)


# Generate EU integration distance variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it <- cbind(EES2019_it,
                    gendist(data = EES2019_it, 
                            indices = 1501:1507, 
                            stub1 = 'Q23',
                            stub2 = 'q24_mean_'))

# Rename the generated variables for stacking
names(EES2019_it)[endsWith(colnames(EES2019_it), 'dist')] <- paste0('q24_', 'dist_', seq(1501, 1507, 1))


EES2019_it <- cbind(EES2019_it,
                    gendist(data = EES2019_it, 
                            indices = 1501:1507, 
                            stub1 = 'Q23',
                            stub2 = 'eu_position_stack_'))

# Rename the generated variables for stacking
names(EES2019_it)[endsWith(colnames(EES2019_it), 'dist')] <- paste0('eu_position_', 'dist_', seq(1501, 1507, 1))


# Drop the variables used for computing the distances - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>%
  dplyr::select(-c(paste0('q24_', seq(1,7,1))))


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

depvars <- paste0('Q7_stack_', rel_prties)

for(i in depvars) {
  EES2019_it <- genyhats(data=EES2019_it, 
                         depvar=i, 
                         regtype='logit',
                         indvar = c("age"),
                         newname = 'age_dich')  
}
rm(i, depvars)

# Generate yhats for a continuous dependent variable

depvars <- paste0('q10_', rel_prties)

for(i in depvars) {
  EES2019_it <- genyhats(data=EES2019_it, 
                         depvar=i, 
                         regtype = 'linear',
                         indvar = c("age"),
                         newname = 'age_cont')  
}
rm(i, depvars)


# Age gender education yhats - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Generate yhats for a dichotomous dependent variable

depvars <- paste0('Q7_stack_', rel_prties)

for(i in depvars) {
  EES2019_it <- genyhats(data=EES2019_it, 
                         depvar=i, 
                         regtype='logit',
                         indvar = c("age", "gndr", "edu1", "edu2", "edu3"),
                         newname = 'socdem_dich')  
}
rm(i, depvars)

# Generate yhats for a continuous dependent variable

depvars <- paste0('q10_', rel_prties)

for(i in depvars) {
  EES2019_it <- genyhats(data=EES2019_it, 
                         depvar=i, 
                         regtype = 'linear',
                         indvar = c("age", "gndr", "edu1", "edu2", "edu3"),
                         newname = 'socdem_cont')  
}
rm(i, depvars)


# Drop the education dummy variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it %<>% dplyr::select(-c(edu1, edu2, edu3))


# Stack the observations ==============================================================================

# names(EES2019_it)[names(EES2019_it)=="Q7"] <- 'votech'

EES2019_it_stacked <- genstacks(data = EES2019_it, 
                                idvar = 'respid', 
                                stubs = c('q10_',
                                          'q13_mean_', 'q13_dist_',
                                          'lrgen_stack_', 'lrgen_dist_',
                                          'q24_mean_', 'q24_dist_',
                                          'eu_position_stack_', 'eu_position_dist_',
                                          'Q7_stack_', 'Q25_stack_', 'Q26_stack_',
                                          'age_cont_yhat_', 'age_dich_yhat_', 
                                          'socdem_dich_yhat_', 'socdem_cont_yhat_'),
                                keepvar = c('Q7', 'Q25', 'Q26', 'Q11','Q23', 'age', 'gndr', 'edu'))

# Mutate the dataset ==================================================================================

# Rename the variables

EES2019_it_stacked %<>%
  dplyr::mutate(party = stack, 
                stackid = paste0(respid, "-", party),
                votech = Q7,
                pid = Q25, 
                pid_str = Q26,
                lr_self = Q11,
                lr_party = q13_mean_,
                lr_party_ches = lrgen_stack_,
                euint_self = Q23,
                euint_party = q24_mean_,
                euint_party_ches = eu_position_stack_,
                ptv = q10_, 
                stacked_votech = Q7_stack_, 
                lr_dist = q13_dist_,
                lr_dist_ches = lrgen_dist_,
                euint_dist = q24_dist_,
                euint_dist_ches = eu_position_dist_,
                stacked_pid = Q25_stack_,
                stacked_pid_str = Q26_stack_,
                age_cont_yhat = age_cont_yhat_,
                age_dich_yhat = age_dich_yhat_,
                socdem_cont_yhat = socdem_cont_yhat_,
                socdem_dich_yhat = socdem_dich_yhat_,
                pid = Q25,
                ) %>%
  dplyr::select(respid, party, stackid, votech, pid, pid_str, age, gndr, edu,
                lr_self, lr_party, lr_party_ches, euint_self, euint_party, euint_party_ches, 
                ptv, stacked_votech, lr_dist, euint_dist, stacked_pid, stacked_pid_str,
                age_dich_yhat, age_cont_yhat, socdem_dich_yhat, socdem_cont_yhat)


# Save the stacked data frame # =======================================================================

fwrite(EES2019_it_stacked, file=paste0(getwd(), '/data/', 'EES2019_it_stacked_R.csv'), na = 99)

