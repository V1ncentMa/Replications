#### Cook the raw data --------------------------------------------------

raw_sel <- raw %>%
  
  # Select variables
  select(
    
    # Weights
    starts_with("PERSON_FINWT"),
    
    # How much do you think climate change will harm your health?
    ClimateChgHarmHealth,
    
    # In general, how much would you trust information about cancer from scientists?
    CancerTrustScientists,
    
    # How often do health recommendations from experts seem to conflict or contradict one another?
    HealthRecsConflict,
    
    # How often do health recommendations from experts seem to change over time?
    HealthRecsChange,
    
    # Gender
    BirthGender,
    
    # Race
    RaceEthn5,
    
    # Age
    AgeGrpB,
    
    # Education
    EducA,
    
    # Income
    HHInc,
    
    # Cancer history
    EverHadCancer,
    
    # Region
    CENSREG
    
  ) %>%
  
  # Variable construction
  mutate(
    
    # Deal with NAs
    across(
      everything(),
      ~ replace(.x, .x < 0, NA)
    ),
    
    # How much do you think climate change will harm your health?
    ClimateChgHarmHealth = case_when(
      ClimateChgHarmHealth == 5 ~ NA,
      ClimateChgHarmHealth == 1 ~ 4,
      ClimateChgHarmHealth == 2 ~ 3,
      ClimateChgHarmHealth == 3 ~ 2,
      ClimateChgHarmHealth == 4 ~ 1
    ),
    ClimateChgHarmHealth_a_lot = ifelse(ClimateChgHarmHealth == 4, 1, 0),
    ClimateChgHarmHealth_some = ifelse(ClimateChgHarmHealth == 3, 1, 0),
    ClimateChgHarmHealth_a_little = ifelse(ClimateChgHarmHealth == 2, 1, 0),
    ClimateChgHarmHealth_not_at_all = ifelse(ClimateChgHarmHealth == 1, 1, 0),
    
    # In general, how much would you trust information about cancer from scientists?
    CancerTrustScientists = case_when(
      CancerTrustScientists == 2 | CancerTrustScientists == 3 |  CancerTrustScientists == 4 ~ 0,
      CancerTrustScientists == 1 ~ 1,
    ),
    
    # How often do health recommendations from experts seem to conflict or contradict one another?
    HealthRecsConflict = case_when(
      HealthRecsConflict == 1 |  HealthRecsConflict == 2 ~ 0,
      HealthRecsConflict == 3 |  HealthRecsConflict == 4 ~ 1
    ),
    
    # How often do health recommendations from experts seem to change over time?
    HealthRecsChange = case_when(
      HealthRecsChange == 1 |  HealthRecsChange == 2 ~ 0,
      HealthRecsChange == 3 |  HealthRecsChange == 4 ~ 1
    ),
    
    # Gender
    BirthGender = case_when(
      BirthGender == 1 ~ 1,
      BirthGender == 2 ~ 0
    ),
    Male = ifelse(BirthGender == 1, 1, 0),
    Female = ifelse(BirthGender == 0, 1, 0),
    
    # Race
    race_group = case_when(
      RaceEthn5 == 1 ~ 1,
      RaceEthn5 == 2 ~ 2,
      RaceEthn5 == 3 ~ 3,
      RaceEthn5 == 4 | RaceEthn5 == 5 ~ 4
    ),
    NH_white = ifelse(RaceEthn5 == 1, 1, 0),
    NH_black = ifelse(RaceEthn5 == 2, 1, 0),
    hispanic = ifelse(RaceEthn5 == 3, 1, 0),
    other_race = ifelse(RaceEthn5 == 4 | RaceEthn5 == 5, 1, 0),
    
    # Age
    age_18_34 = ifelse(AgeGrpB == 1, 1, 0),
    age_35_49 = ifelse(AgeGrpB == 2, 1, 0),
    age_50_64 = ifelse(AgeGrpB == 3, 1, 0),
    age_65_74 = ifelse(AgeGrpB == 4, 1, 0),
    age_75_plus = ifelse(AgeGrpB == 5, 1, 0),
    
    # Education
    edu_group = case_when(
      EducA == 1 | EducA == 2 ~ 1,
      EducA == 3 ~ 2,
      EducA == 4 ~ 3
    ),
    edu_high_school_less = ifelse(EducA == 1 | EducA == 2, 1, 0),
    edu_some_college = ifelse(EducA == 3, 1, 0),
    edu_college_grad = ifelse(EducA == 4, 1, 0),
    
    # Income
    income_35000_less = ifelse(HHInc == 1 | HHInc == 2, 1, 0),
    income_35000_75000 = ifelse(HHInc == 3 | HHInc == 4, 1, 0),
    income_75000_plus = ifelse(HHInc == 5, 1, 0),
    income_group = case_when(
      HHInc == 1 | HHInc == 2 ~ 1,
      HHInc == 3 | HHInc == 4 ~ 2,
      HHInc == 5 ~ 3
    ),
    
    # Cancer history
    Cancer_survivor = ifelse(EverHadCancer == 1, 1, 0),
    Never_had_cancer = ifelse(EverHadCancer == 2, 1, 0),
    
    # Census region
    region_group = case_when(
      CENSREG == 1 ~ 1,
      CENSREG == 2 ~ 2,
      CENSREG == 3 ~ 3,
      CENSREG == 4 ~ 4
    ),
    CR_northeast = ifelse(CENSREG == 1, 1, 0),
    CR_midwest = ifelse(CENSREG == 2, 1, 0),
    CR_south = ifelse(CENSREG == 3, 1, 0),
    CR_west = ifelse(CENSREG == 4, 1, 0),
    
  ) %>%
  
  # Data selection
  filter(
    
    # Exclude observations with missing values in ClimateChgHarmHealth
    !is.na(ClimateChgHarmHealth)
  )




#### Factorization --------------------------------------------------

raw_sel <- raw_sel %>%
  mutate(
    
    # How much do you think climate change will harm your health?
    ClimateChgHarmHealth_f = factor(
      ClimateChgHarmHealth,
      levels = c(1, 2, 3, 4),
      labels = c("Not at all", "A little", "Some", "A lot"),
      ordered = TRUE
    ),
    
    # In general, how much would you trust information about cancer from scientists?
    CancerTrustScientists_f = relevel(
      factor(
        CancerTrustScientists,
        levels = c(0, 1),
        labels = c("No", "Yes")
      ),
      ref = "No"
    ),
    
    # How often do health recommendations from experts seem to conflict or contradict one another?
    HealthRecsConflict_f = relevel(
      factor(
        HealthRecsConflict,
        levels = c(0, 1),
        labels = c("Rarely/Never", "Often/Very often")
      ),
      ref = "Rarely/Never"
    ),
    
    # How often do health recommendations from experts seem to change over time?
    HealthRecsChange_f = relevel(
      factor(
        HealthRecsChange,
        levels = c(0, 1),
        labels = c("Rarely/Never", "Often/Very often")
      ),
      ref = "Rarely/Never"
    ),
    
    # Gender
    gender_f = relevel(
      factor(
        BirthGender,
        levels = c(1, 0),
        labels = c("Male", "Female"),
      ),
      ref = "Male"
    ),
    
    # Age group
    age_f = relevel(
      factor(
        AgeGrpB,
        levels = c(1, 2, 3, 4, 5),
        labels = c("18-34", "35-49", "50-64", "65-74", "75+"),
      ),
      ref = "18-34"
    ),
    
    # Race
    race_group_f = relevel(
      factor(
        race_group,
        levels = c(1, 2, 3, 4),
        labels = c("NH_white", "NH_black", "Hispanic", "Other"),
      ),
      ref = "NH_white"
    ),
    
    # Education
    edu_group_f = relevel(
      factor(
        edu_group,
        levels = c(1, 2, 3),
        labels = c("High school or less", "Some college", "College graduate"),
      ),
      ref = "High school or less"
    ),
    
    # Income
    income_group_f = relevel(
      factor(
        income_group,
        levels = c(1, 2, 3),
        labels = c("Less than $35,000", "$35,000 to $75,000", "$75,000 or more"),
      ),
      ref = "Less than $35,000"
    ),
    
    # Cancer history
    cancer_history_f = relevel(
      factor(
        EverHadCancer,
        levels = c(1, 2),
        labels = c("No", "Yes"),
      ),
      ref = "No"
    ),
    
    # Region
    region_group_f = relevel(
      factor(
        region_group,
        levels = c(1, 2, 3, 4),
        labels = c("Northeast", "Midwest", "South", "West"),
      ),
      ref = "Northeast"
    )
    
  )




#### Generate survey design object --------------------------------------------------
hints_survey <- as_survey_rep(
  
  # Assign Data frame
  .data = raw_sel,
  
  # Assign the final weight for the composite sample
  weights = PERSON_FINWT0,
  
  # Assign the replicate weights
  repweights = num_range(prefix = "PERSON_FINWT", range = 1:50),
  
  # Assign sampling type
  type = "JKn",
  scale = 0.98,
  # Assign replicate scales
  rscales = rep(1, times = 50)
)




#### Define Functions --------------------------------------------------

# Weighted percentage calculator
# This function will return you the weighted percentage for data frame 'df' when the column 'col' equals to value 'value'
# Formal parameter 'df' for data frame, 'col' for column name (string) and 'value' for the column value
wpc_calculator <- function(df, col, value) {
  
  # Calculation
  weighted_percentage <- round(
    sum((df[[col]] == value) * df[["PERSON_FINWT0"]], na.rm = TRUE) / sum(df[["PERSON_FINWT0"]]) * 100,
    1
  )
  
  # Return the result
  return(weighted_percentage)
}


# Counter
# This function will return you the number of observations for data frame 'df' when the column 'col' equals to value 'value'
# Formal parameter 'df' for data frame (object), 'col' for column name (string) and 'value' for the column value (numeric)
n_calculator <- function(df, col, value) {
  
  # Return the quantity of observations
  return(nrow(df %>% filter(!!sym(col) == value)))
}

