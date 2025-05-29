
#### Cook the Raw Data --------------------------------------------------

raw <- hints_pooled %>%
  
  select(
    
    # Weight
    starts_with("person_finwt"),
    
    # Cycles
    cycles,
    
    # In the past 12 months, have you used a computer, smartphone, or other electronic means to do any of the following?, Looked for health or medical information for yourself?
    electronic_selfhealthinfo,
    
    # In the past 12 months, have you used a computer, smartphone, or other electronic means to do any of the following?, Used email or the internet to communicate with a doctor or doctor’s office?
    electronic_talkdoctor,
    
    # In the past 12 months, have you used a computer, smartphone, or other electronic means to do any of the following?, Looked up medical test results.
    electronic_testresults,
    
    # In the past 12 months, have you used a computer, smartphone, or other electronic means to do any of the following?, Looked for health or medical information for yourself? (2022)
    electronic2_healthinfo,
    
    # In the past 12 months, have you used a computer, smartphone, or other electronic means to do any of the following?, Send a message to a health care provider or a health care provider’s office. (2022)
    electronic2_messagedoc,
    
    # In the past 12 months, have you used a computer, smartphone, or other electronic means to do any of the following?”, View medical test results (2022)
    electronic2_testresults,
    
    # Gender
    selfgender,
    birthgender,
    
    # Age
    age,
    
    # Race
    raceethn5,
    
    # Education
    educa,
    
    # Income
    hhinc,
    
    # Health status
    generalhealth
    
  ) %>%
  
  mutate(
    
    # Unify dependent variables
    electronic_selfhealthinfo = ifelse(cycles == 2, electronic2_healthinfo, electronic_selfhealthinfo),
    electronic_talkdoctor = ifelse(cycles == 2, electronic2_messagedoc, electronic_talkdoctor),
    electronic_testresults = ifelse(cycles == 2, electronic2_testresults, electronic_testresults),
    
    # Deal with the NA value
    across(
      everything(),
      ~ replace(.x, .x < 0, NA)
    ),
    
    # ehealth use
    ehealth_use = ifelse(
      electronic_selfhealthinfo == 1 | electronic_talkdoctor == 1 | electronic_testresults == 1, 1,
      ifelse(
        electronic_selfhealthinfo == 2 & electronic_talkdoctor == 2 & electronic_testresults == 2, 0, NA
      )
    ),
    
    # Sex (2018--selfgender, 2020--selfgender, 2022--birthgender)
    selfgender = ifelse(cycles == 2, birthgender, selfgender),
    selfgender = case_when(
      selfgender == 1 ~ 0,
      selfgender == 2 ~ 1
    ),
    
    # Age cohort
    age_cohort = case_when(
      
      # Millennials
      (cycles == 0 & age >= 22 & age <= 37) | (cycles == 1 & age >= 24 & age <= 39) | (cycles == 2 & age >= 26 & age <= 41) ~ 1,
      
      # Generation X
      (cycles == 0 & age >= 38 & age <= 53) | (cycles == 1 & age >= 40 & age <= 55) | (cycles == 2 & age >= 42 & age <= 57) ~ 2,
      
      # Baby boomers (***** Error here !!! *****)
      (cycles == 0 & age > 54 & age <= 72) | (cycles == 1 & age >= 56 & age <= 74) | (cycles == 2 & age >= 58 & age <= 76) ~ 3,
      
      # Silent generation
      (cycles == 0 & age >= 73 & age <= 89) | (cycles == 1 & age >= 75 & age <= 91) | (cycles == 2 & age >= 77 & age <= 93) ~ 4
      
    ),
    
    # Health status
    generalhealth = case_when(
      generalhealth == 1 ~ 5,
      generalhealth == 2 ~ 4,
      generalhealth == 3 ~ 3,
      generalhealth == 4 ~ 2,
      generalhealth == 5 ~ 1
    ),
    
    # Year
    year = case_when(
      cycles == 0 ~ 1,
      cycles == 1 ~ 2,
      cycles == 2 ~ 3
    )
    
  ) %>%
  
  # Select cooked variables
  select(
    
    # Weight
    starts_with("person_finwt"),
    
    # Year
    year,
    
    # eHealth use
    ehealth_use,
    
    # Sex
    selfgender,
    
    # Age
    age,
    
    # Age cohort
    age_cohort,
    
    # Race
    raceethn5,
    
    # Education
    educa,
    
    # Income
    hhinc,
    
    # Health status
    generalhealth,
    
  ) %>%
  
  # Sample Selection (Full sample, N = 9691)
  filter(
    
    # Remove observations with any NA values
    !is.na(ehealth_use)
    & !is.na(selfgender)
    & !is.na(age_cohort)
    & !is.na(raceethn5)
    & !is.na(educa)
    & !is.na(hhinc)
    & !is.na(generalhealth)
    & !is.na(age)
    
  )





#### Factorization --------------------------------------------------

raw <- raw %>%
  
  mutate(
    
    # ehealth use
    ehealth_use_f = relevel(
      factor(
        ehealth_use,
        levels = c(0, 1),
        labels = c("No", "Yes")
      ),
      ref = "No"
    ),
    
    # Sex
    selfgender_f = relevel(
      factor(
        selfgender,
        levels = c(0, 1),
        labels = c("Male", "Female")
      ),
      ref = "Male"
    ),
    
    # Age cohort
    age_cohort_f = relevel(
      factor(
        age_cohort,
        levels = c(1, 2, 3, 4),
        labels = c("Millennials", "Generation X", "Baby boomers", "Silent generation")
      ),
      ref = "Millennials"
    ),
    
    # Race
    raceethn5_f = relevel(
      factor(
        raceethn5,
        levels = c(1, 2, 3, 4, 5),
        labels = c("White", "Black", "Hispanic", "Asian", "Other")
      ),
      ref = "White"
    ),
    
    # Education
    educa_f = relevel(
      factor(
        educa,
        levels = c(1, 2, 3, 4),
        labels = c("Less than high school graduate", "High school graduate", "Some college", "College graduate")
      ),
      ref = "Less than high school graduate"
    ),
    
    # Income
    hhinc_f = relevel(
      factor(
        hhinc,
        levels = c(1, 2, 3, 4, 5),
        labels = c("<20,000", "20,000-34,999", "35,000-49,999", "50,000-74,999", "≥75,000")
      ),
      ref = "<20,000"
    ),
    
    # Year
    year_f = relevel(
      factor(
        year,
        levels = c(1, 2, 3),
        labels = c("2018", "2020", "2022")
      ),
      ref = "2018"
    )
    
  )




#### Generate survey design object --------------------------------------------------


hints_svy = as_survey_rep(
  
  # Assign data frame
  .data = raw, 
  
  # final weight
  weights = person_finwt0, 
  
  # rep weight
  repweights = num_range(prefix = "person_finwt", range = 1:50), 
  
  # type
  type = "JKn",
  
  # scale
  scale = 0.98,  
  
  # rep scale
  rscales = rep(1, times = 50)
  
) 




#### Define Functions --------------------------------------------------


# Weighted percentage calculator
# This function will return you the weighted percentage for survey 'survey' when the column 'col' equals to value 'value'
# Formal parameter 'survey' for survey object (survey), 'col' for column name (char or string) and 'value' for the column value(int or double)
wpc_calculator <- function(survey, col, value) {
  
  # Transform to symbol
  col_sym <- rlang::sym(col)
  
  # Calculate
  result <- survey %>%
    
    # Remove NA
    dplyr::filter(!is.na(!!col_sym)) %>%
    
    # Group by col
    dplyr::group_by(!!col_sym) %>%
    
    # Calculate weighted percent
    dplyr::summarise(pct = srvyr::survey_prop(proportion = TRUE)) %>%
    
    # Filter
    dplyr::filter(!!col_sym == value) %>%
    
    # Extract weighted percent
    dplyr::pull(pct)
  
  # Return
  return(round(result, 4) * 100)
}


# Chi-square test p-value calculator
# This function will return you the p value for chi-square test on 'col1' and 'col2' in survey 'survey'
# Formal parameter 'survey' for survey object (survey), 'col1' and 'col2' for column name (char or string)
chisq_calculator <- function(survey, col1, col2) {
  
  # Set formula
  formula <- stats::reformulate(c(col1, col2))
  
  # Chi-square test
  chisq_result <- survey::svychisq(
    formula = formula,
    design = survey,
    statistic = "F"
  )
  
  # Return p value
  return(round(chisq_result$p.value, 2))
}


# Chi-square test p-value calculator
# This function will return you the p value for chi-square test on 'col1' and 'col2' in dataframe 'df'
# Formal parameter 'df' for dataframe, 'col1' and 'col2' for column name (string)
chisq_calculator_general <- function(df, col1, col2) {
  
  # Extract data
  col1 <- df[[col1]]
  col2 <- df[[col2]]
  
  # Generate crosstab
  ctable <- table(col1, col2)
  
  # Chi-square test
  test_result <- chisq.test(ctable)
  rm(col1, col2, ctable)
  
  # Return p-value
  return(round(test_result$p.value, 2))
  
}


# Adjusted OR extractor with CI
# This function will return the formatted adjusted OR with 95% CI for the variable at position 'pointer' in model 'model'
# Formal parameter 'model' for logistic regression model object (glm), 'pointer' for variable position index (integer)
extract_or_ci <- function(model, pointer) {
  
  # Extract the results
  results <- tidy(
    model,
    conf.int = TRUE,
    conf.level = 0.95,
    exponentiate = TRUE
  ) %>%
    
    # Exclude intercept
    filter(term != "(Intercept)")
  
  # Find the target row
  target_row <- results[pointer, ]
  sprintf(
    "%.2f (%.2f, %.2f)", 
    round(target_row$estimate, 2),
    round(target_row$conf.low, 2),
    round(target_row$conf.high, 2)
  )
  
}



