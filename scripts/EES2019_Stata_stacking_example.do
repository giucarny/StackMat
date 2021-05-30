* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
* Title: Script for Stacking Data (EES 2019 Voter Study, Italian Sample) 
* Author: G.Carteny
* last update: 2021-05-27
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

* Install the 'StackMe' package if not already done ============================

// 1. Install the ‘github‘ Stata package

* net install github, from("https://haghish.github.io/github/")

// 2. Once this is done, you can install the latest version of 'StackMe'

* github install ldesio/stackme

// NB: if you get any error it could be useful to uninstall previous versions of
// and rerun the previous steps

* ado uninstall stackme


* Admin ========================================================================
// Check current directory
pwd

// set the working directory
cd "C:\Users\giuse\Documents\GIT\StackMat\data"
* ssc install unique


* Load data ====================================================================

use "ZA7581_v1-0-0.dta", clear
//use "EES_CHES_2019_aux.dta", clear

* Merge with auxiliary dataset (w/ CHES party-specific scores & other vars) ====


merge 1:m respid countrycode Q7 using EES_CHES_2019_aux
drop _merge

merge 1:m respid countrycode Q25 using EES_2019_Q25_aux
drop _merge

drop Q25
ren Q25_rec Q25

foreach var of varlist _all {
	label var `var' ""
}


* Select country-specific data frames for stacking =============================

keep if countrycode==1380 // EES2019 Italian voter study

* Select the relevant variables ================================================

keep respid countrycode Q7 q10* Q11 q13* D3 D4_1 Q23 q24* Q25 Q26 EDU ///
lrgen eu_position


* Create additional variables ==================================================
// generate an age variable - - - - - - - - - - - - - - - - - - - - - - - - - - 
gen year = 2019
gen age = 2019 - D4_1
keep if age>17
drop D4_1

// Rename the gender variable - - - - - - - - - - - - - - - - - - - - - - - - - 
ren D3 gndr
ren EDU edu


*  Select the relevant parties =================================================

* relevant parties: 1501 1502 1503 1504 1505 1506 1507

* Party identification =========================================================

// recode the party identification variable (Q25) to make party codes parallel 
// with the party choice variable (Q7)



// Recode the party identification variable - - - - - - - - - - - - - - - - - - 
replace Q25=. if Q25<100 
replace Q25=. if Q25>=1508

// Generate a set of dichotomous variables from the PID variable - - - - - - - -
forvalues j = 1501/1507 {
	generate Q25_stack_`j' = Q25
	replace Q25_stack_`j' = 1 if Q25_stack_`j'==`j' 
	replace Q25_stack_`j' = 0 if Q25_stack_`j'!=`j' & Q25_stack_`j'!=1
	replace Q25_stack_`j' = . if missing(Q25)
}


// Recode the PID-strength variable - - - - - - - - - - - - - - - - - - - - - - 

recode Q26 (0=0) (2=2) (3=1) (1=3)

// Generate a set of PID-strength variables for stacking - - - - - - - - - - - -

forvalues i = 1501/1507 {
    generate Q26_stack_`i' = Q26 if Q25==`i'
	replace Q26_stack_`i'=0 if missing(Q26_stack_`i')
}

replace Q26=. if missing(Q25)

* Dependent variables ==========================================================

// Recode the EP elections vote choice variable - - - - - - - - - - - - - - - - 
replace Q7=. if Q7<100 
replace Q7=. if Q7>=1508

// Replace values bigger than 10 in the PTV var.s - - - - - - - - - - - - - - - 
forvalues j = 1/7 {
replace q10_`j'=. if q10_`j'>10 
}

// Rescale the PTV values - - - - - - - - - - - - - - - - - - - - - - - - - - -
forvalues j = 1/7 {
replace q10_`j'= q10_`j'/10
}

// Drop the empty PTV variables - - - - - - - - - - - - - - - - - - - - - - - -
drop q10_8 q10_9 q10_10

// Rename the PTV variables for the stacking procedure - - - - - - - - - - - - -
local oldnm q10_1 q10_2 q10_3 q10_4 q10_5 q10_6 q10_7
local newnm q10_1501 q10_1502 q10_1503 q10_1504 q10_1505 q10_1506 q10_1507
rename (`oldnm') (`newnm')

// Generate a set of dichotomous variables from the EP vote choice one - - - - -
forvalues j = 1501/1507 {
    generate Q7_`j' = Q7
	generate Q7_stack_`j' = Q7
	replace Q7_stack_`j' = 1 if Q7_stack_`j'==`j' 
	replace Q7_stack_`j' = 0 if Q7_stack_`j'!=`j' & Q7_stack_`j'!=1
	replace Q7_stack_`j' = . if missing(Q7_`j')
}

* LR distance ==================================================================

// Drop variable related to non-relevant parties - - - - - - - - - - - - - - - -
drop q13_8 q13_9

// Recode missing values and rescale LR self placement - - - - - - - - - - - - -
replace Q11=. if Q11>10

forvalues j = 1/7 {
replace q13_`j'=. if q13_`j'>10 
}

replace Q11 = Q11/10


// Rescale individual perceptions of party positions - - - - - - - - - - - - - -
forvalues i = 1/7 {
    replace q13_`i' = q13_`i'/10	
}

// Generate mean values of party positions - - - - - - - - - - - - - - - - - - -
forvalues i = 1/7 {
egen q13_mean_150`i' = mean(q13_`i')	
}

// Generate mean values of party positions for ches var - - - - - - - - - - - - 
forvalues i = 1/7 {
gen lrgen2_stack_150`i' = lrgen if Q7==150`i'	
egen lrgen_stack_150`i' = mean(lrgen2_stack_150`i')
drop lrgen2_stack_150`i'
}

drop lrgen

// Generate LR distance variables - - - - - - - - - - - - - - - - - - - - - - -
forvalues j = 1/7 {
    gen q13_dist_150`j' = abs(q13_mean_150`j' - Q11)
}

// Generate LR distance variables w/ ches var - - - - - - - - - - - - - - - - - 
forvalues j = 1/7 {
    gen lrgen_dist_150`j' = abs(lrgen_stack_150`j' - Q11)
}

// Drop the variables used for computing the distances - - - - - - - - - - - - -
forvalues j = 1/7 {
drop q13_`j'
}



* EU integration distances =====================================================

// Drop variable related to non-relevant parties - - - - - - - - - - - - - - - -
drop q24_8 q24_9

// Recode missing values - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
replace Q23=. if Q23>10


forvalues j = 1/7 {
replace q24_`j'=. if q24_`j'>10 
}

replace Q23 = Q23/10

// Rescale individual perceptions of party positions - - - - - - - - - - - - - -
forvalues i = 1/7 {
    replace q24_`i' = q24_`i'/10	
}


// Generate mean values of party positions on EU integration - - - - - - - - - -
forvalues i = 1/7 {
egen q24_mean_150`i' = mean(q24_`i')	
}

// Generate mean values of party positions for ches var - - - - - - - - - - - - 
forvalues i = 1/7 {
gen eu_position2_stack_150`i' = eu_position if Q7==150`i'	
egen eu_position_stack_150`i' = mean(eu_position2_stack_150`i')
drop eu_position2_stack_150`i'
}

drop eu_position


// Generate EU integration distance variables - - - - - - - - - - - - - - - - - 
forvalues j = 1/7 {
gen q24_dist_150`j' = abs(q24_mean_150`j' - Q23)
}

// Generate EU integration distance variables w/ ches var - - - - - - - - - - - 
forvalues j = 1/7 {
    gen eu_position_dist_150`j' = abs(eu_position_stack_150`j' - Q23)
}

// Drop the variables used for computing the distances - - - - - - - - - - - - -
forvalues j = 1/7 {
drop q24_`j'
}

* Sociodemographic yhats =======================================================

* Recode edu variable - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
replace edu = . if edu > 3

* Create a set of dummy variables for education - - - - - - - - - - - - - - - - 
forvalues i = 1/3 {
gen edu`i' = edu 
}
recode edu1 (1 = 1) (2 3 = 0)
recode edu2 (2 = 1) (1 3 = 0)
recode edu3 (3 = 1) (1 2 = 0)

* Recode gender (category 3 with too few observations) - - - - - - - - - - - - -
replace gndr = . if gndr==3

* age - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Generate yhats for a dichotomous dependent variable
forvalues j = 1/7 {
genyhats age_dich_yhat_150`j': age, dep(Q7_stack_150`j') log adjust(no)	
}

// Generate yhats for a continuous dependent variable
forvalues j = 1/7 {
genyhats age_cont_yhat_150`j': age, dep(q10_150`j') adjust(no)	
}

* age gender education - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Generate yhats for a dichotomous dependent variable
forvalues j = 1/7 {
genyhats socdem_dich_yhat_150`j': age gndr edu1 edu2 edu3, dep(Q7_stack_150`j') log adjust(no)	
}

// Generate yhats for a continuous dependent variable
forvalues j = 1/7 {
genyhats socdem_cont_yhat_150`j': age gndr edu1 edu2 edu3, dep(q10_150`j')  adjust(no)	
}


* drop the education dummy variables - - - - - - - - - - - - - - - - - - - - - -
drop edu1 edu2 edu3


* Stack the observations =======================================================
 
// 'genstacks' is the 'StackMe' function for stacking the data frame obs.
* help genstacks
genstacks q10_ q13_mean_ q13_dist_ q24_mean_ q24_dist_ ///
lrgen_stack_ lrgen_dist_ eu_position_stack_ eu_position_dist_ ///
Q25_stack_ Q26_stack_ /// 
Q7_ Q7_stack_ ///
age_dich_yhat_ age_cont_yhat_ ///
socdem_dich_yhat_ socdem_cont_yhat_, rep 



* Mutate the dataset (in line w/ codebook) =====================================

* Identification variables - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Drop and rename some variables 
drop genstacks_stack genstacks_nstacks
ren genstacks_item party

* Generate a party-voter 'stackid' variable 
tostring respid, gen(respid2)
tostring party, gen(party2)
gen stackid = respid2 + "-" + party2
drop respid2 party2


* Voting behavior and Background variables - - - - - - - - - - - - - - - - - - -
ren Q7_ votech
ren Q25 pid
ren Q26 pid_str

* Voter-Party distance variables - - - - - - - - - - - - - - - - - - - - - - - -
ren Q11 lr_self
ren q13_mean_ lr_party
ren Q23 euint_self
ren q24_mean_ euint_party
ren lrgen_stack_ lr_party_ches
ren eu_position_stack_ euint_party_ches

* Generic and synthetic variables - - - - - - - - - - - - - - - - - - - - - - - 
ren q10_ ptv
ren Q7_stack_ stacked_vc
ren q13_dist_ lr_dist
ren lrgen_dist_ lr_dist_ches
ren q24_dist_ euint_dist
ren eu_position_dist euint_dist_ches
ren Q25_stack_ stacked_pid
ren Q26_stack_ stacked_pid_str
ren age_dich_yhat_ age_dich_yhat
ren age_cont_yhat_ age_cont_yhat
ren socdem_dich_yhat_ socdem_dich_yhat
ren socdem_cont_yhat_ socdem_cont_yhat

keep respid party stackid ///
votech pid pid_str age gndr edu /// 
lr_self lr_party lr_party_ches ///
euint_self euint_party euint_party_ches ///
ptv stacked_vc lr_dist euint_dist lr_dist_ches euint_dist_ches ///
stacked_pid stacked_pid_str ///
age_dich_yhat age_cont_yhat socdem_dich_yhat socdem_cont_yhat

order respid party stackid ///
votech pid pid_str age gndr edu /// 
lr_self lr_party lr_party_ches ///
euint_self euint_party euint_party_ches ///
ptv stacked_vc lr_dist lr_dist_ches euint_dist euint_dist_ches stacked_pid stacked_pid_str ///
age_dich_yhat age_cont_yhat socdem_dich_yhat socdem_cont_yhat

* Recode all the missing values and save the dataset ===========================

recode age (.=999)

foreach x of varlist * {
    if substr("`: type `x''",1,3) != "str" {
        recode `x' (. = 99)
    }
 }


export delimited using "EES2019_it_stacked_stata.csv",  nolabel replace