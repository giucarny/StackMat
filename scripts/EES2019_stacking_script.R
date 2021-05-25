# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES 2019 - Stacked Data 
# Author: G.Carteny
# last update: 2021-04-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Notes: - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 2021-04-14: Only italian df, for now 
# 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# ADMIN #==============================================================================================

want = c("tidyverse", "magrittr", "haven", "labelled")

have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
rm(have,want,junk)

options(scipen = 99)
rm(list = ls())
gitwd <- "C:/Users/giuse/Documents/GIT/MZES/EES"
eeswd <- "D:/Work/Data/EES/2019"


# Loading the data # ==================================================================================

setwd(eeswd)

EES2019 <- haven::read_dta('ZA7581_v1-0-0.dta')

# Creating additional variables # =====================================================================

# cntry df # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

cntry_df <- data.frame(countrycode = EES2019$countrycode %>% unique(),
                       country_name = EES2019$countrycode %>% val_labels() %>% attr(., 'names'))

EES2019 <- left_join(EES2019, cntry_df)

rm(cntry_df)

# year var # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019$year <- 2019

# age var # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


EES2019 %<>% mutate(age = year-D4_1) %>% filter(age>17) # removing respondents younger than 18yrs

# Selecting the Italian dataset # =====================================================================

EES2019it <- EES2019 %>% 
  filter(country_name=='Italy')


# Select Relevant parties # ==================================================================================
# As a first step I'll just selected those parties for which the ptv are available (~7)


# Load a pre-mutated dataset for getting the IT parties as coded in EES2019 # - - - - - - - - - - - - - - - -
parties_it <- fread('ZA7581_cp_it.csv') %>% as_tibble()

# Get the parties according to a criterion # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
prties <- parties_it %>% filter(!is.na(Unifed_party_code)) %>% dplyr::select(countrycode, Q9, `English name`)


# Switching from wide to long format, with voter-party dyads as obs. # - - - - - - - - - - - - - - - - - - - -

multiply.fun <- function(dataset, rw) {
  df <- dataset %>% dplyr::select(respid)
  y <- c()
  for(v in 1:nrow(prties)) {
    x <- paste0(df[rw,1],'-',prties[v,2])  
    y <- c(y,x)
  }
  y %<>% as.data.frame()
  names(y) <- 'dyad'
  y$respid <- c(rep(df[rw,1], nrow(prties)))
  y$party <- prties[,2] %>% unlist %>% as.numeric
  
  y %<>% mutate(party = as.numeric(party),
                respid = as.numeric(respid))
  
  return(y)
}

EES2019it_stckd <- 
  lapply(dataset=EES2019it, 
         X=1:nrow(EES2019it), 
         FUN=multiply.fun) %>% 
  do.call('rbind',.) %>% 
  as_tibble()

rm(list=ls(pattern='fun'))
rm(prties)

# Dependent variable # ================================================================================

prtych.df <- EES2019it %>% dplyr::select(respid, Q9) %>% mutate(Q9 = as.numeric(Q9))
names(prtych.df)[2] <- 'votech'

EES2019it_stckd <- left_join(EES2019it_stckd, prtych.df)

EES2019it_stckd %<>% mutate(stacked_vc = case_when(votech==party ~ 1, T ~ 0))

rm(prtych.df)

# PTV # ===============================================================================================

parties_it$Q10_PTV[parties_it$Q10_PTV==""] <- NA
ptvs_prties <- parties_it %>% dplyr::select(Q10_PTV, Q9) %>% na.omit() %>% as.data.frame()
names(ptvs_prties) <- c('ptv', 'party')
ptvs_prties %<>% mutate(ptv = tolower(ptv))

df <- 
  EES2019it %>% 
  dplyr::select(respid, all_of(ptvs_prties$ptv)) %>% 
  mutate(across(all_of(ptvs_prties$ptv), ~as.numeric(.))) %>% 
  mutate(across(all_of(ptvs_prties$ptv), ~case_when(.>10 ~ NA_real_, T~.))) %>%
  pivot_longer(cols = all_of(ptvs_prties$ptv), names_to = 'ptv', values_to = 'ptv_val')

df <- 
  left_join(df, ptvs_prties) %>% 
  mutate(ptv = ptv_val) %>% 
  dplyr::select(respid, party, ptv)  

EES2019it_stckd <- left_join(EES2019it_stckd, df)

rm(df, ptvs_prties)



# EES based left-right # ==============================================================================

# Self left-right # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

df <- EES2019it %>% dplyr::select(respid, Q11) 
names(df)[2] <- 'lefrig_self'

df %<>% 
  mutate(lefrig_self = as.numeric(lefrig_self)) %>% 
  mutate(lefrig_self = case_when(lefrig_self > 10 ~ NA_real_, T ~ lefrig_self))

EES2019it_stckd <- left_join(EES2019it_stckd, df)


# Parties left-right # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

parties_it$Q13_left_right[parties_it$Q13_left_right==""] <- NA
ees_lefrig_parts_prties <- 
  parties_it %>% dplyr::select(Q13_left_right, Q9) %>% 
  na.omit() %>% as.data.frame()

names(ees_lefrig_parts_prties) <- c('ees_lefrig_part', 'party')
ees_lefrig_parts_prties %<>% mutate(ees_lefrig_part = tolower(ees_lefrig_part))

df <- 
  EES2019it %>% 
  dplyr::select(respid, all_of(ees_lefrig_parts_prties$ees_lefrig_part)) %>% 
  mutate(across(all_of(ees_lefrig_parts_prties$ees_lefrig_part), ~as.numeric(.))) %>% 
  mutate(across(all_of(ees_lefrig_parts_prties$ees_lefrig_part), ~case_when(.>10 ~ NA_real_, T~.))) %>%
  pivot_longer(cols = all_of(ees_lefrig_parts_prties$ees_lefrig_part), 
               names_to = 'ees_lefrig_part', values_to = 'ees_lefrig_part_val')

df <- 
  left_join(df, ees_lefrig_parts_prties) %>% 
  mutate(ees_lefrig_part = ees_lefrig_part_val) %>% 
  dplyr::select(respid, party, ees_lefrig_part)  

EES2019it_stckd <- left_join(EES2019it_stckd, df)

rm(ees_lefrig_parts_prties)

# Create the lrdist var # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019it_stckd %<>% 
  mutate(ees_lrdist = lefrig_self - ees_lefrig_part) %>% 
  mutate(ees_lrdist_abs = abs(ees_lrdist))



# CHES based left-right # =============================================================================

cheswd <- "D:/Work/Data/CHES"
setwd(cheswd)

CHES <- haven::read_dta("1999-2019_CHES_dataset_means(v1).dta")

# cntry df # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This is an ad hoc solution that has to be standardized
CHESit <- CHES %>% filter(country==8) %>% filter(year==2019)

CHES.prty.df <- CHESit %>% 
  dplyr::select(party_id, party) %>% 
  mutate(party_id=as.numeric(party_id),
         party=as.character(party)) %>% 
  distinct()

CHESit.lrgen <- CHESit %>% 
  dplyr::select(party_id, party, lrgen) %>%
  mutate(party_id=as.numeric(party_id),
         party=as.character(party), 
         lrgen=as.numeric(lrgen))

parties_it %>% dplyr::select(Q9, "English name")

parties_it %<>% 
  mutate(party=case_when(Q9==1501 ~ 'PD',
                         Q9==1506 ~ 'FI',
                         Q9==1507 ~ 'LN',
                         Q9==1508 ~ 'M5S',
                         Q9==1502 ~ 'SI',
                         Q9==1503 ~ 'RI',
                         Q9==1510 ~ 'FdI',
                         T~NA_character_))

df <- parties_it %>% 
  dplyr::select(Q9, party) %>% 
  na.omit()

CHESit.lrgen <- left_join(CHESit.lrgen, df)

names(CHESit.lrgen)[names(CHESit.lrgen)=='party'] <- 'party_nm'
names(CHESit.lrgen)[names(CHESit.lrgen)=='Q9'] <- 'party' 
names(CHESit.lrgen)[names(CHESit.lrgen)=='lrgen'] <- 'ches_lefrig_part' 

CHESit.lrgen %<>% dplyr::select(party, ches_lefrig_part) %>% na.omit()


EES2019it_stckd <- left_join(EES2019it_stckd, CHESit.lrgen)

# Create the lrdist var # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019it_stckd %<>% 
  mutate(ches_lrdist = lefrig_self - ches_lefrig_part) %>% 
  mutate(ches_lrdist_abs = abs(ches_lrdist))


# Sociodemographic Synthetic Vars # ===================================================================

vrbls.1 <- c('respid')
vrbls.2 <- c('D3', 'age', 'D4_1', 'D6', 'D8', 'D7', 'Q9', 'EDU', 'Q9') # 'D6a',


df <- EES2019it %>% 
  dplyr::select(c(all_of(vrbls.1),
                  all_of(vrbls.2))) # %>% mutate(across(all_of(vrbls.2),~as.numeric(.)))

ren_df <- data.frame(old_names = c('D3', 'D4_1', 'D7', 'D8', 'D6', 'Q9', 'EDU', 'Q9'),
                     new_names = c('gndr', 'birthdt', 'subjses', 'urbrur', 'occptn_1', 'votech', 'edu', 'votech'))

for(s in 1:nrow(ren_df)) {
  names(df)[names(df)==ren_df[s,1]] <- ren_df[s,2]
}
rm(s)

vrbls.2 <- ren_df$new_names

rm(ren_df)

# occptn recode # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

occptn_1_lbls <- df$occptn_1 %>% val_labels()

df %<>%
  mutate(occptn_1 = as.numeric(occptn_1)) %>% 
  mutate(occptn_1 = case_when(occptn_1 == 7 | occptn_1 == 99 ~ NA_real_,
                              T ~ occptn_1))

val_labels(df$occptn_1) <- occptn_1_lbls

rm(occptn_1_lbls)


# Education # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

df %<>% 
  mutate(edu = as.numeric(edu)) %>% 
  mutate(edu = case_when(edu>3 ~ NA_real_, T ~ edu))

# Vote choice # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

partychoices <- EES2019it_stckd$party %>% unique()

df %<>% 
  mutate(votech = as.numeric(votech)) %>%
  mutate(votech = case_when(votech==90 | votech==1505 | votech==1504 | votech==1509 ~ 0,
                            votech>90 & votech<100 ~ NA_real_,
                            T~votech))


depvar.fun <- function(data, prty) {
  x <- mutate(data, "votech_{prty}" := case_when(votech==prty ~ 1, T ~ 0))
  x %<>% dplyr::select(length(x))
  return(x)
}

df <- cbind(df, lapply(data = df, X=partychoices, FUN=depvar.fun) %>% do.call('cbind',.)) %>% as_tibble()


# other recodes # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tofactor.vrbls <- c('urbrur', 'subjses', 'occptn_1', 'votech', 'edu')

df %<>% 
  mutate(across(all_of(vrbls.2), ~as.numeric(.))) %>% 
  mutate(votech = case_when(votech<100 ~ NA_real_, T~votech)) %>% 
  mutate(across(all_of(tofactor.vrbls), ~as.factor(.))) %>% 
  mutate(across(starts_with('votech'), ~as.factor(.)))


# Regression models # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


synth.vars.fun <- function(data, depvar, indvar) {
  
  frmla <- as.formula(paste(depvar, paste(indvar, collapse = " + "), sep = " ~ "))
  
  ics <- data %<>% dplyr::select(all_of(depvar), all_of(indvar))
  
  for(i in 2:length(ics)) {
    vec <- ics[[i]] %>% as.numeric()
    
    cl <- class(ics[[i]])
    
    if(cl=='numeric') {
      ics[[i]] <- ifelse(is.na(vec), mean(vec, na.rm=T) , vec)
    } else if (cl=='factor') {
      ics[[i]] <- ifelse(is.na(vec), median(vec, na.rm=T) , vec)
    }
  }
  
  x <- glm(frmla, data = ics, family = "binomial") 
  
  outcome <- data.frame(respos = predict(x) %>% attr(., 'names') %>% as.numeric(),
                        yhat = predict(x, type='response'))
  
  respid <- data.frame(respos = 1:nrow(df),
                       respid = df$respid)
  
  outcomedf <- left_join(respid, outcome)  
  
  outcomedf %<>% dplyr::select(respid, yhat)
  
  names(outcomedf)[names(outcomedf)=='yhat'] <- paste0('yhat_', depvar)
  
  df <- left_join(df, outcomedf)
  
  return(df)
}

depvars <- names(df)[str_detect(names(df), pattern='votech_')]#  %>% as.list()

for(s in 1:length(depvars)) {
  df <- synth.vars.fun(data=df, depvar=depvars[[s]], indvar = c("edu", "age", "urbrur", "occptn_1"))
}
rm(s)

df %<>% dplyr::select(respid, starts_with('yhat'))

names(df) %<>% gsub('yhat_votech_','',.)

df %<>% 
  pivot_longer(cols=starts_with('1'), names_to='party', values_to='sociodemo_yhat') %>% 
  mutate(party = as.numeric(party))

EES2019it_stckd <- left_join(EES2019it_stckd, df)


# Cleaning environment # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

rm(list=ls(pattern = 'vrbls'))
rm(list=ls(pattern = 'CHES'))
rm(list=ls(pattern = 'fun')); 
rm(df,partychoices)

# Save the output # ====================================================================================

datawd <- 'D:/Work/Data/EES/2019'

setwd(datawd)

fwrite(EES2019it_stckd, file='EES2019it_stacked.csv')