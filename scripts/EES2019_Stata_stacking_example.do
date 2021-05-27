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


* Select country-specific data frames for stacking =============================

keep if countrycode==1380 // EES2019 Italian voter study


* Select the relevant variables ================================================

keep respid Q7 q10* Q11 q13* D3 D4_1 Q23 q24* Q25 EDU


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


* LR distance ==================================================================

// Drop variable related to non-relevant parties - - - - - - - - - - - - - - - -
drop q13_8 q13_9

// Recode missing values - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
replace Q11=. if Q11>10

forvalues j = 1/7 {
replace q13_`j'=. if q13_`j'>10 
}

// Generate mean values of party positions - - - - - - - - - - - - - - - - - - -
forvalues i = 1/7 {
egen q13_`i'mean = mean(q13_`i')	
}

// Generate LR distance variables - - - - - - - - - - - - - - - - - - - - - - -
forvalues j = 1/7 {
gen q13_`j'dist = 1-((abs(10-1) - abs(q13_`j'mean - Q11))/abs(10-1))
}

// Drop the variables used for computing the distances - - - - - - - - - - - - -
drop Q11
drop *mean
forvalues j = 1/7 {
drop q13_`j'
}

// Rename the generated variables for stacking - - - - - - - - - - - - - - - - -
local oldnm q13_1dist q13_2dist q13_3dist q13_4dist q13_5dist q13_6dist q13_7dist
local newnm q13_1501 q13_1502 q13_1503 q13_1504 q13_1505 q13_1506 q13_1507

ren (`oldnm') (`newnm')


* EU integration distances =====================================================

// Drop variable related to non-relevant parties - - - - - - - - - - - - - - - -
drop q24_8 q24_9

// Recode missing values - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
replace Q23=. if Q23>10

forvalues j = 1/7 {
replace q24_`j'=. if q24_`j'>10 
}

// Generate mean values of party positions on EU integration - - - - - - - - - -
forvalues i = 1/7 {
egen q24_`i'mean = mean(q24_`i')	
}

// Generate EU integration distance variables - - - - - - - - - - - - - - - - - 
forvalues j = 1/7 {
gen q24_`j'dist = 1-((abs(10-1) - abs(q24_`j'mean - Q23))/abs(10-1))
}

// Drop the variables used for computing the distances - - - - - - - - - - - - -
drop Q23
drop *mean
forvalues j = 1/7 {
drop q24_`j'
}

// Rename the generated variables for stacking - - - - - - - - - - - - - - - - -
local oldnm q24_1dist q24_2dist q24_3dist q24_4dist q24_5dist q24_6dist q24_7dist
local newnm q24_1501 q24_1502 q24_1503 q24_1504 q24_1505 q24_1506 q24_1507

ren (`oldnm') (`newnm')


* Party identification =========================================================

// Recode the party identification variable - - - - - - - - - - - - - - - - - - 
replace Q25=. if Q25<100 
replace Q25=. if Q25>=1508

// Generate a set of dichotomous variables from the PID variable - - - - - - - -
forvalues j = 1501/1507 {
    generate Q25_`j' = Q25
	replace Q25_`j' = 1 if Q25_`j'==`j' 
	replace Q25_`j' = 0 if Q25_`j'!=`j' & Q25_`j'!=1
}


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
	replace Q7_`j' = 1 if Q7_`j'==`j' 
	replace Q7_`j' = 0 if Q7_`j'!=`j' & Q7_`j'!=1
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
genyhats age_dich_yhat_150`j': age, dep(Q7_150`j') log adjust(no)	
}

// Generate yhats for a continuous dependent variable

forvalues j = 1/7 {
genyhats age_cont_yhat_150`j': age, dep(q10_150`j') adjust(no)	
}

* age gender education - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Generate yhats for a dichotomous dependent variable
forvalues j = 1/7 {
genyhats socdem_dich_yhat_150`j': age gndr edu1 edu2 edu3, dep(Q7_150`j') log adjust(no)	
}

// Generate yhats for a continuous dependent variable

forvalues j = 1/7 {
genyhats socdem_cont_yhat_150`j': age gndr edu1 edu2 edu3, dep(q10_150`j') adjust(no)	
}


* drop the education dummy variables - - - - - - - - - - - - - - - - - - - - - -

drop edu1 edu2 edu3


* Stack the observations =======================================================
 
// 'genstacks' is the 'StackMe' function for stacking the data frame obs.
* help genstacks

genstacks q10_ Q7_ q13_ q24_ Q25_ /// 
age_dich_yhat_ age_cont_yhat_ ///
socdem_dich_yhat_ socdem_cont_yhat_, rep


* Mutate the dataset ===========================================================

* Drop and rename some variables - - - - - - - - - - - - - - - - - - - - - - - -
drop Q7 genstacks_stack genstacks_nstacks
ren genstacks_item party
ren q10_ ptv
ren Q7_ stacked_vc
ren q13_ lr_dist
ren q24_ eu_dist
ren Q25_ pid
ren age_dich_yhat_ age_dich_yhat
ren age_cont_yhat_ age_cont_yhat
ren socdem_dich_yhat_ socdem_dich_yhat
ren socdem_cont_yhat_ socdem_cont_yhat

* Generate a party-voter 'dyad' variable - - - - - - - - - - - - - - - - - - - -
tostring respid, gen(respid2)
tostring party, gen(party2)
gen dyad = respid2 + "-" + party2
drop respid2 party2

* Select the variables to keep and reorder them - - - - - - - - - - - - - - - - 
keep dyad respid party ptv stacked_vc lr_dist eu_dist pid age_* socdem* age gndr edu
order dyad respid party ptv stacked_vc lr_dist eu_dist pid age_* socdem* age gndr edu


