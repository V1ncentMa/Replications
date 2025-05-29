* CODE BELOW ASSUMES ALL DATASETS ARE IN THE SAME FOLDER
* USER NOTE: THROUGHOUT THE CODE BELOW UPDATE THE PATHS IN USE AND SAVE LINES
* WE STRONGLY RECOMMEND CREATING A SEPARATE TEMPORARY FOLDER TO SAVE PREPPED DATASET

* PREP DATA FOR EACH CYCLE AND GENERATE THE APPROPRIATE NUMBER OF REPLICATE WEIGHTS USING THE RIZZO METHOD

set trace on

use "C:\Users\m1810\Desktop\Paper\HINTS\HINTS 5 Cycle 2 (2018)\hints5_cycle2_public.dta", clear 

gen nwgt0 = person_finwt0

forvalues n1 = 1 / 50 {
         local x1=`n1' + 50
         local x2=`n1' + 100
         gen nwgt`n1' = person_finwt`n1'
         gen nwgt`x1' = person_finwt0
         gen nwgt`x2' = person_finwt0
}
save "C:\Users\m1810\Desktop\Paper\Paper Replications\Sociodemographic Factors Associated With Using eHealth for Information Seeking in the United States\hints5_cycle2_public.dta", replace

use "C:\Users\m1810\Desktop\Paper\HINTS\HINTS 5 Cycle 4 (2020)\\hints5_cycle4_public.dta", clear 

* RENAMES ALL VARIABLES TO LOWERCASE FOR MERGING WITH PREVIOUS CYCLES THAT CONTAIN DIFFERENT TEXT CASE FORMATS 

rename _all, lower 

gen nwgt0 = person_finwt0

forvalues n1 = 1 / 50 {
         local x1=`n1' + 50
         local x2=`n1' + 100
         gen nwgt`n1' = person_finwt0
         gen nwgt`x1' = person_finwt`n1'
         gen nwgt`x2' = person_finwt0
}
save "C:\Users\m1810\Desktop\Paper\Paper Replications\Sociodemographic Factors Associated With Using eHealth for Information Seeking in the United States\hints5_cycle4_public.dta", replace

use "C:\Users\m1810\Desktop\Paper\HINTS\HINTS 6 (2022)\hints6_public.dta", clear 

* RENAMES ALL VARIABLES TO LOWERCASE FOR MERGING WITH PREVIOUS CYCLES THAT CONTAIN DIFFERENT TEXT CASE FORMATS 

rename _all, lower 

gen nwgt0 = person_finwt0

forvalues n1 = 1 / 50 {
         local x1=`n1' + 50
         local x2=`n1' + 100
         gen nwgt`n1' = person_finwt0
         gen nwgt`x1' = person_finwt0
         gen nwgt`x2' = person_finwt`n1'
}
save "C:\Users\m1810\Desktop\Paper\Paper Replications\Sociodemographic Factors Associated With Using eHealth for Information Seeking in the United States\hints6_public.dta", replace

* STACK TOGETHER DATA FROM EACH SURVEY CYCLE AND GENERATE USER FORMAT TO DELINEATE THE DIFFERENT SURVEY CYCLES

use "C:\Users\m1810\Desktop\Paper\Paper Replications\Sociodemographic Factors Associated With Using eHealth for Information Seeking in the United States\hints5_cycle2_public.dta", clear

append using "C:\Users\m1810\Desktop\Paper\Paper Replications\Sociodemographic Factors Associated With Using eHealth for Information Seeking in the United States\hints5_cycle4_public.dta" "C:\Users\m1810\Desktop\Paper\Paper Replications\Sociodemographic Factors Associated With Using eHealth for Information Seeking in the United States\hints6_public.dta" , generate(cycles) force

tab cycles

label define cycles 0 "HINTS 5 Cycle 2" 1 "HINTS 5 Cycle 4" 2 "HINTS 6" 

label values cycles cycles

tab cycles

save "C:\Users\m1810\Desktop\Paper\Paper Replications\Sociodemographic Factors Associated With Using eHealth for Information Seeking in the United States\hints_pooled.dta", replace