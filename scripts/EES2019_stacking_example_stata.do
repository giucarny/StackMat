* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
* Title: Script for Stacking Data (EES 2019 Voter Study, Italian Sample) 
* Author: G.Carteny
* last update: 2021-05-26
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// Notes 
* The workflow to create a stacked data matrix on Stata is differently from the 
* one created on R. The stacking process in Stata is more straightforwardly 
* created by, first, recoding and/or creating the relevant variables, and then 
* stacking the observations. 
* The library for creating the stacked data matrix is the 'StackMe' library. 
* However, to access the main informations about such library one musts search 
* for the former version of such library, named 'ptvtools'.

* help ptvtools


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

keep respid Q7 q10* Q11 q13* D3 D4_1


* Create additional variables ==================================================
// generate an age variable - - - - - - - - - - - - - - - - - - - - - - - - - - 
gen year = 2019
gen age = 2019 - D4_1
keep if age>17
drop D4_1

// Rename the gender variable - - - - - - - - - - - - - - - - - - - - - - - - - 
ren D3 gndr

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
gen q13_`j'dist = abs(Q11 - q13_`j'mean)
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

* Dependent variables ==========================================================

// Recode the EP elections vote choice variable - - - - - - - - - - - - - - - - 
replace Q7=. if Q7<100 
replace Q7=. if Q7>=1508

// Replace values bigger than 10 in the PTV var.s - - - - - - - - - - - - - - - 
forvalues j = 1/7 {
replace q10_`j'=. if q10_`j'>10 
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

forvalues j = 1/7 {
genyhats yhat_150`j': age gndr, dep(Q7_150`j') log adjust(no)	
}


* Stack the observations =======================================================
 
// 'genstacks' is the 'StackMe' function for stacking the data frame obs.
* help genstacks

genstacks q10_ Q7_ q13_ yhat_, rep


* Mutate the dataset ===========================================================

* Drop and rename some variables - - - - - - - - - - - - - - - - - - - - - - - -
drop Q7 genstacks_stack genstacks_nstacks
ren genstacks_item party
ren q10_ ptv
ren Q7_ stacked_vc
ren q13_ lr_dist
ren yhat_ genderage_yhat

* Generate a party-voter 'dyad' variable - - - - - - - - - - - - - - - - - - - -
tostring respid, gen(respid2)
tostring party, gen(party2)
gen dyad = respid2 + "-" + party2
drop respid2 party2

* Select the variables to keep and reorder them - - - - - - - - - - - - - - - - 
keep dyad respid party ptv stacked_vc lr_dist genderage_yhat gndr age
order dyad respid party ptv stacked_vc lr_dist genderage_yhat gndr age


