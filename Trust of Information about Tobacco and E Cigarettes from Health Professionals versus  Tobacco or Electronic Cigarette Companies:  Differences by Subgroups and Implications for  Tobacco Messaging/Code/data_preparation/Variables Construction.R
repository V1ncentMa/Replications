#### Cook the raw data --------------------------------------------------


# Select and rename variables
raw_sel <- raw %>% 
  select(
    stratum,
    starts_with("person_"),
    raceethn,
    educa,
    selfgender,
    incomeranges,
    age,
    ruc2013,
    smoke100,
    smokenow,
    tobaccotried_ecig,
    starts_with("tobaccotrust"),
    starts_with("ecigtrust")
  ) %>%
  rename(
    weight = person_finwt0,
    race = raceethn,
    edu = educa,
    income = incomeranges,
    gender = selfgender,
    geographic_area = ruc2013
  ) %>%
  mutate(
    
    # Age
    age = replace(age, age < 0, NA),
    
    # Race
    race = ifelse(race == 2, 0, 1),
    
    # Education
    education = case_when(
      edu == 2 | edu == 1 ~ 1,
      edu > 2 ~ 0
    ),
    
    # Gender
    gender = case_when(
      gender == 1 ~ 1,
      gender == 2 ~ 0
    ),
    
    # Income
    income = case_when(
      income == 1 | income == 2 | income == 3 ~ 1,
      income > 3 ~ 0
    ),
    
    # Geographic area
    geographic_area = ifelse(geographic_area < 4, 0, 1),
    
    # Tobacco trust
    # Substep 1: clean
    tobaccotrustdoctor = replace(
      tobaccotrustdoctor,
      tobaccotrustdoctor == -9 | tobaccotrustdoctor == -5,
      NA
    ),
    tobaccotrustgovhealth = replace(
      tobaccotrustgovhealth,
      tobaccotrustgovhealth == -9 | tobaccotrustgovhealth == -5,
      NA
    ),
    tobaccotrusthealthorgs = replace(
      tobaccotrusthealthorgs,
      tobaccotrusthealthorgs == -9 | tobaccotrusthealthorgs == -5,
      NA
    ),
    tobaccotrusttobaccoco = replace(
      tobaccotrusttobaccoco,
      tobaccotrusttobaccoco == -9 | tobaccotrusttobaccoco == -5,
      NA
    ),
    
    # Substep 2: recode
    tobaccotrustdoctor = ifelse(tobaccotrustdoctor == 4, 1,
                                ifelse(tobaccotrustdoctor == 3, 2,
                                       ifelse(tobaccotrustdoctor == 2, 3,
                                              ifelse(tobaccotrustdoctor == 1, 4, NA)))),
    tobaccotrustgovhealth = ifelse(tobaccotrustgovhealth == 4, 1,
                                   ifelse(tobaccotrustgovhealth == 3, 2,
                                          ifelse(tobaccotrustgovhealth == 2, 3,
                                                 ifelse(tobaccotrustgovhealth == 1, 4, NA)))),
    tobaccotrusthealthorgs = ifelse(tobaccotrusthealthorgs == 4, 1,
                                    ifelse(tobaccotrusthealthorgs == 3, 2,
                                           ifelse(tobaccotrusthealthorgs == 2, 3,
                                                  ifelse(tobaccotrusthealthorgs == 1, 4, NA)))),
    tobaccotrusttobaccoco = ifelse(tobaccotrusttobaccoco == 4, 1,
                                   ifelse(tobaccotrusttobaccoco == 3, 2,
                                          ifelse(tobaccotrusttobaccoco == 2, 3,
                                                 ifelse(tobaccotrusttobaccoco == 1, 4, NA)))),
    
    # Substep 3: generate 'relative trust'
    tobacco_rel_trust = ifelse((tobaccotrustdoctor + tobaccotrustgovhealth + tobaccotrusthealthorgs) / 3 <= tobaccotrusttobaccoco, 1, 0),
    
    # E-cigarette trust
    # Substep 1: clean
    ecigtrustdoctor = replace(ecigtrustdoctor,
                              ecigtrustdoctor == -9 | ecigtrustdoctor == -5,
                              NA),
    ecigtrustgovhealth = replace(ecigtrustgovhealth,
                                 ecigtrustgovhealth == -9 | ecigtrustgovhealth == -5,
                                 NA),
    ecigtrusthealthorgs = replace(ecigtrusthealthorgs,
                                  ecigtrusthealthorgs == -9 | ecigtrusthealthorgs == -5,
                                  NA),
    ecigtrustecigco = replace(ecigtrustecigco,
                              ecigtrustecigco == -9 | ecigtrustecigco == -5,
                              NA),
    
    # Substep 2: recode
    ecigtrustdoctor = ifelse(ecigtrustdoctor == 4, 1,
                             ifelse(ecigtrustdoctor == 3, 2,
                                    ifelse(ecigtrustdoctor == 2, 3,
                                           ifelse(ecigtrustdoctor == 1, 4, NA)))),
    ecigtrustgovhealth = ifelse(ecigtrustgovhealth == 4, 1,
                                ifelse(ecigtrustgovhealth == 3, 2,
                                       ifelse(ecigtrustgovhealth == 2, 3,
                                              ifelse(ecigtrustgovhealth == 1, 4, NA)))),
    ecigtrusthealthorgs = ifelse(ecigtrusthealthorgs == 4, 1,
                                 ifelse(ecigtrusthealthorgs == 3, 2,
                                        ifelse(ecigtrusthealthorgs == 2, 3,
                                               ifelse(ecigtrusthealthorgs == 1, 4, NA)))),
    ecigtrustecigco = ifelse(ecigtrustecigco == 4, 1,
                             ifelse(ecigtrustecigco == 3, 2,
                                    ifelse(ecigtrustecigco == 2, 3,
                                           ifelse(ecigtrustecigco == 1, 4, NA)))),
    
    # Substep 3: generate 'relative trust'
    ecig_rel_trust = ifelse((ecigtrustdoctor + ecigtrustgovhealth + ecigtrusthealthorgs) / 3 <= ecigtrustecigco, 1, 0),
    
    # Tobacco related behavior
    # Substep 1: generate current smoker
    current_smoker = ifelse(smoke100 == 1 & (smokenow == 1 | smokenow == 2), 1, 0),
    
    # Substep 2: generate former smoker
    former_smoker = ifelse(smoke100 == 1 & smokenow == 3, 1, 0),
    
    # Substep 3: generate never smoker
    never_smoker = ifelse(smoke100 == 2, 1, 0),
    
    # Substep 4: generate 'smoking status'
    smoking_status = ifelse(current_smoker == 1, 1,
                            ifelse(former_smoker == 1, 2,
                                   ifelse(never_smoker == 1, 3, NA))),
    
    # E-cigarette related behavior
    ecig_use = ifelse(tobaccotried_ecig == 1, 1,
                      ifelse(tobaccotried_ecig == 2, 0, NA))
  )




#### Factorization --------------------------------------------------

raw_sel <- raw_sel %>%
  mutate(
    
    # race
    race_f = factor(race,
                    levels = c(1, 0),
                    labels = c("Minority", "NH-White")),
    
    # education
    education_f = factor(education,
                         levels = c(0, 1),
                         labels = c("> High school", "<= High school")),
    
    # income
    income_f = factor(income,
                      levels = c(0, 1),
                      labels = c(">= $20,000", "< $20,000")),
    
    # gender
    gender_f = factor(gender,
                      levels = c(0, 1),
                      labels = c("Female", "Male")),
    
    # geographic area
    geographic_area_f = factor(geographic_area,
                               levels = c(0, 1),
                               labels = c("Metropolitan", "Non-metropolitan")),
    
    # E-cigarette relative trust
    ecig_rel_trust_f = factor(ecig_rel_trust,
                              levels = c(0, 1),
                              labels = c("Health professionals more than e-cigarette companies", "E-cigarette companies as much or more than health professionals")),
    
    # Tobacco relative trust
    tobacco_rel_trust_f = factor(tobacco_rel_trust,
                                 levels = c(0, 1),
                                 labels = c("Health professionals more than tobacco companies", "Tobacco companies as much or more than health professionals")),
    
    # smoking status
    smoking_status_f = factor(smoking_status,
                              levels = c(1, 2, 3),
                              labels = c("Current smoker", "Former smoker", "Never smoker")),
    
    # E-cigarette use
    ecig_use_f = factor(ecig_use,
                        levels = c(0, 1),
                        labels = c("Never", "Ever"))
  )

#### Define Functions --------------------------------------------------


# Weighted percentage calculator
# This function will return you the weighted percentage for data frame 'df' when the column 'col' equals to value 'value'
# Formal parameter 'df' for data frame, 'col' for column name (string) and 'value' for the column value
wpc_calculator <- function(df, col, value) {
  
  # Remove missing values
  df <- df[!is.na(df[[col]]), ]
  
  # Calculation
  weighted_percentage <- round(
    sum((df[[col]] == value) * df[["weight"]]) / sum(df[["weight"]]) * 100,
    1
  )
  
  # Return the result
  return(weighted_percentage)
}

