


// Set survey
svyset _n [pw=PERSON_FINWT0], ///
    jkrweight(PERSON_FINWT1-PERSON_FINWT50, multiplier(0.98)) ///
    vce(jackknife) mse

// Table 2: Generalized ordered logit model 
svy: gologit2 ClimateChgHarmHealth_f i.gender_f i.age_f i.race_group_f i.edu_group_f i.income_group_f i.cancer_history_f i.region_group_f

// Table 2: Calculate marginal effect
margins, dydx(*)

// Table 3: Generalized ordered logit model 
svy: gologit2 ClimateChgHarmHealth_f i.CancerTrustScientists_f i.HealthRecsConflict_f i.HealthRecsChange_f i.gender_f i.age_f i.race_group_f i.edu_group_f i.income_group_f i.cancer_history_f i.region_group_f

// Table 3: Calculate marginal effect
margins, dydx(*)

