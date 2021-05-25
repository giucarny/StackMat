# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Data (EES 2019 Voter Study, Italian Sample) 
# Author: G.Carteny
# last update: 2021-05-21
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

# Set the working directory # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setwd(paste0(getwd(), '/StackMat-master/'))


# Load data # =========================================================================================

# Load the EES 2019 voter study dataset (Stata version) # - - - - - - - - - - - - - - - - - - - - - - -
EES2019 <- haven::read_dta(paste0(getwd(), '/data/' ,'ZA7581_v1-0-0.dta'))


# Load an auxiliary data frame, containing party id variables (EES, CHES, CMP), vote shares (for first 
# and second order elections),... # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_aux <- data.table::fread(paste0(getwd(), '/data/' ,'ZA7581_cp_mod.csv'), na.strings = '') 



# Create additional variables # =======================================================================

# Add to the dataset a variable w/ country names  # - - - - - - - - - - - - - - - - - - - - - - - - - -
# The following lines create a data frame containing the country codes (first column) and the country 
# names, extracting the latter from the country code labels (second column)

cntry_df <- data.frame(countrycode = EES2019$countrycode %>% unique(),
                       country_name = EES2019$countrycode %>% val_labels() %>% attr(., 'names'))

# This auxiliary data frame is then merged w/ the EES data frame, and then removed from the environment

EES2019 <- left_join(EES2019, cntry_df)
rm(cntry_df)


# Create an 'age' variable # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# In order to create an age variable for each respondent...

EES2019 %<>% 
  dplyr::mutate(year = 2019) %>%     # (1) Create a variable specifying the EES voter study year
  dplyr::mutate(age = year-D4_1) %>% # (2) Create an 'age' variable, substracting the birth year from the survey year 
  dplyr::filter(age>17)              # (3) Remove respondents younger than 18yrs



# Select country-specific data frames for stacking # ==================================================

# Select the EES 2019 country-specific voter study to be stacked # - - - - - - - - - - - - - - - - - -

EES2019_it <- EES2019 %>% dplyr::filter(country_name=='Italy')
rm(EES2019)


# Select the related data from the auxiliary data frame # - - - - - - - - - - - - - - - - - - - - - - -

EES2019_aux_it <- EES2019_aux %>% 
  dplyr::filter(country_short=='IT',  # Select the Italian dataset
                year==2019,           # Select the European Parliament elections year
                fs_order==2)          # Select only second order elections           
rm(EES2019_aux)


# Select the relevant parties # =======================================================================

# This is the first key passage of the stacking procedure: the selection of the relevant parties. 
# The criterion used in the following lines is rather simple/simplicistic: select only those parties 
# about which the EES provides the propensity to vote (PTV) variable.
# There are several ways in which we can obtain the list of parties that satisfy the criterion.
# In the following lines we rely on the auxiliary data frame created few lines above. 

prty <- EES2019_aux_it %>%
  dplyr::filter(!is.na(Q10_PTV)) %>%  # Drop parties for which is not available the EES2019 PTV variable
  dplyr::select(Q7) %>%               # Get the party id from the EES EP elections vote choice variable
  unlist() %>% as.numeric()           # Transform the object from a data frame to a vector



# Stack the observations # ============================================================================

# This is the second key passage, in which a data frame containing one row for each combination of the 
# supplied vectors is created, namely the stacked data matrix.

respid <- EES2019_it %>% 
  dplyr::select(respid) %>%  # Select the respondent identification codes column   
  unlist() %>% as.numeric()  # Transform the column in a numeric vector

EES2019_it_stckd <- 
  expand.grid(prty, respid) %>%                         # Combine the two vectors 
  dplyr::mutate(respid = Var2, party = Var1) %>%        # Rename the new columns 
  dplyr::mutate(dyad = paste0(respid, '-', party)) %>%  # Create a variable for the voter-party combinations (dyads)
  dplyr::select(dyad, respid, party)                    # Select and reorder the relevant columns

rm(prty, respid) # Remove the vectors created earlier


# Create a dichotomous dependent variable # ===========================================================

# This block generates a dichotomous dependent variable 

prtych.df <- 
  EES2019_it %>% 
  dplyr::select(respid, Q7) %>%               # Select the respondents' id and the EP elections vote choice variables  
  dplyr::mutate(vote_ch = as.numeric(Q7)) %>% # Mutate and rename the EP elections vote choice var.
  dplyr::select(-c(Q7))                       # Deselect the original vote choice variables


EES2019_it_stckd <- 
  dplyr::left_join(EES2019_it_stckd, prtych.df, by='respid') # Join the data frames by the respondent id variable

# The stacked data frame now has a column (vote_ch) with the party voted by each respondent. 
# Thus the value of the vote choice is constant across all the party-voter dyads referring to the same 
# respondent. 
# The following conditional statement, then, assigns the value 1 when the vote choice refers to one of
# the parties selected to build the party-voter dyads, otherwise the value will be 0. 

EES2019_it_stckd %<>%         
  dplyr::mutate(stacked_vc = case_when(vote_ch==party ~ 1,     
                                       T ~ 0)) %>% 
  dplyr::select(-c(vote_ch))                               # Eliminate the original vote choice variable

rm(prtych.df)


# PTV # ===============================================================================================

# This block generates the so-called propensity to vote (PTV) variables. 

# First, a collateral data frame is created starting from the auxiliary data frame created earlier. 
# The resulting data frame is thus constituted by a column collecting to the PTV variable name in the 
# original EES 2019 voter study dataset and a second column referring to the parties selected for building 
# the party-voter dyads.

ptvs_prties <- 
  EES2019_aux_it %>% 
  dplyr::select(Q10_PTV,                # Keep the columns identifying the name of the PTV variables and... 
                Q7) %>%                 # ...the column referring to the party codes
  dplyr::mutate(ptv = tolower(Q10_PTV), # Convert the variable names to lower case, as in the EES dataset
                party = Q7) %>%         # Create a party variable (essentially, we are renaming the variable)
  na.omit() %>%                         # Drop all the rows containing missing values
  dplyr::select(ptv, party)             # Keep only the new PTV and party variables
  

# Now we turn to the EES dataset. First, we create a temporary dataset containing (a) the repondents' 
# identification codes and (b) the EES PTV variables. 

df <- 
  EES2019_it %>% 
  dplyr::select(respid, all_of(ptvs_prties$ptv)) 

# Then, after recoding the data (eliminating the value labels and assigning NA to missing values), 
# the dataset is reshaped from wide to long format.

df %<>%
  dplyr::mutate(across(all_of(ptvs_prties$ptv), ~as.numeric(.))) %>% 
  dplyr::mutate(across(all_of(ptvs_prties$ptv), ~case_when(.>10 ~ NA_real_, T~.))) %>%
  tidyr::pivot_longer(cols = all_of(ptvs_prties$ptv), names_to = 'ptv', values_to = 'ptv_val')

# Once reshaped, we join, on the one hand, the data frame juxtaposing the name of the ptv variables and
# party identification codes ('ptvs_parties') and the long-shaped data frame created above, on the 
# other hand. 
# By doing so, also in the long-shaped data frame each ptv variable is put side by side to the party
# identification code. Then, the variables are renamed and finally joined with the main stacked data
# frame. 

df <- 
  left_join(df, ptvs_prties) %>% 
  dplyr::mutate(ptv = ptv_val) %>% 
  dplyr::select(respid, party, ptv)  

EES2019_it_stckd <- 
  dplyr::left_join(EES2019_it_stckd, df) %>%
  as_tibble()

rm(df, ptvs_prties)


# EES based left-right # ==============================================================================

# This block creates the first dependent variable of vote-choice or vote-utility (namely, the PTV var).

# First, we select respondents' left-right self-placement, dropping the value labels and then recoding]
# the missing values/

df <- 
  EES2019_it %>% 
  dplyr::select(respid, Q11)             # Select the respondents' id var and the EES variable for respondents' LR self-placement

names(df)[names(df)=='Q11'] <- 'lr_self' # Rename the EES variable for respondents' LR self-placement

df %<>% 
  mutate(lr_self = as.numeric(lr_self)) %>% 
  mutate(lr_self = case_when(lr_self > 10 ~ NA_real_, # If the values are higher than 10 recode as missing...
                                 T ~ lr_self))        # ...otherwise keep the original values

EES2019_it_stckd <- left_join(EES2019_it_stckd, df)   # Join the dataframe just created w/ the stacked data frame


# At this point the stacked data frame has a column specifying individuals' LR self-pos. that is costant
# within individuals, this within each voter-party dyad.

# The second step consists in creating a variable summarizing the distance between individuals' and each
# relevant party on the LR scale. As a consequence what we need is a measure indicating the LR position 
# of each relevant party on the LR dimension. Party positions can be computed in several ways, from
# different data sources (e.g. survey respondents' assessments of party positions, expert surveys
# measures, positions derived by party manifestos content analysis,...). 

# In this example we rely on party positions' derived by respondents' assessments. Also in this case
# party positions can be estimated in several ways. In the following lines the positions on the LR 
# scale of each relevant party are estimated by computing the mean of all the EES respondents'  
# perceptions of each relevant party position on the left–right dimension.

EES2019_aux_it$Q13_left_right[EES2019_aux_it$Q13_left_right==""] <- NA


# As in the case of the PTVs, we juxtapose the name of the EES variables identifying individuals' perceptions
# of party positions with the identification codes of the parties related to each LR variable, using the 
# auxiliary data frame loaded earlier.

prties_lr_df <- 
  EES2019_aux_it %>% 
  dplyr::select(Q13_left_right,                                # Keep the columns identifying the name of the party LR variables and...  
                Q7) %>%                                        # ...the column referring to the party codes
  dplyr::mutate(Q13_left_right = tolower(Q13_left_right)) %>%  # Convert the LR party variable names to lower case, as in the EES dataset
  na.omit()                                                    # Drop all the rows containing missing values
 
names(prties_lr_df) <- c('party_lr', 'party')                  # Rename the variables 


# Now we turn to the EES dataset. First, we create a temporary dataset containing (a) the repondents' 
# identification codes and (b) the party-specific LR variables. 

df <- 
  EES2019_it %>% 
  dplyr::select(respid, all_of(prties_lr_df$party_lr)) 

# Then, we recode the data dropping the value labels and assigning NA to missing values. 

df %<>% 
  dplyr::mutate(across(all_of(prties_lr_df$party_lr), ~as.numeric(.))) %>% 
  dplyr::mutate(across(all_of(prties_lr_df$party_lr), ~case_when(.>10 ~ NA_real_, T~.))) 


# Then we compute the average LR position for each column (that is, each relevant party), 
# we transpose the matrix and convert into a R data frame.

df %<>% 
  dplyr::summarise(across(all_of(prties_lr_df$party_lr), ~mean(., na.rm = T))) %>%
  t() %>%
  as.data.frame()


# Then we mutate the dataframe using the rownames for creating a column containing the LR party 
# position variable names, then dropping the data frame row names. Finally we join this data 
# frame and the data frame juxtaposing the name of the LR party variables and party identification 
# codes ('prties_lr_df'). 

# Consequently the result will be a dataframe w/ one column containing the party identification
# codes and a second column containing the mean of all the EES respondents' perceptions of each 
# relevant party position on the left–right dimension.

df %<>% data.frame(party_lr = rownames(df))
names(df)[names(df)=='V1'] <- 'party_lr_val'
rownames(df) <- NULL


# Now, we join, on the one hand, this data frame and the auxiliary dataframe created earlier 
# juxtaposing the party LR position variable names and the party identification codes.


df <- 
  dplyr::left_join(df, prties_lr_df) %>%         # Join the dataset by the common variables ('party_lr)
  dplyr::mutate(party_lr = party_lr_val) %>%     # Rename the variable containing the party LR position values
  dplyr::select(party, party_lr)          # keep only the selected columns




# At this point we can join this data frame with the main stacked data frame. 
# The resulting stacked dataframe, thus, contains both individuals' LR self-placements and the mean 
# values of all the EES respondents' perceptions of each relevant party position on the LR dimension,
# namely 'lr_self' and 'party_lr'

EES2019_it_stckd <- dplyr::left_join(EES2019_it_stckd, df)

# Finally, we can take the absolute value of  individuals' LR self-positioning minus the average party 
# positions on the LR dimension. The resulting variable, thus, consists in the distance between 
# voters and parties on the LR dimension

EES2019_it_stckd %<>% 
  dplyr::mutate(lr_dist = abs(lr_self-party_lr))

# Then we drop individuals' LR self-positioning and party average LR positions.

EES2019_it_stckd %<>% dplyr::select(-c(lr_self, party_lr))

rm(df, prties_lr_df)



