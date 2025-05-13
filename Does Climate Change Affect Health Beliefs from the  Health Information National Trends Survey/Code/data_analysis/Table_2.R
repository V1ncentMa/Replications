
#### Data frame preparation --------------------------------------------------


# Generate initial data frame
# Substep 1: generate column names
col_names <- c(
  "characteristics",
  "harm_not_at_all",
  "harm_a_little",
  "harm_some",
  "harm_a_lot"
)

# Substep 2: update column names and name the data frame
table_2 <- data.frame(
  
  # 34 rows and 5 columns
  matrix(NA, nrow = 30, ncol = 5)
)

colnames(table_2) <- col_names

# Substep 3: add row names
table_2$characteristics <- c(
  "Gender",
  "Male",
  "Female",
  "Age",
  "18-34",
  "35-49",
  "50-64",
  "65-74",
  "75 or older",
  "Race/Ethnicity",
  "Non-Hispanic White",
  "Non-Hispanic Black",
  "Hispanic",
  "Non-Hispanic Other",
  "Education",
  "High school",
  "Some college",
  "College graduate",
  "Income",
  "Less than $35,000",
  "35,000-$75,000",
  "Greater than $75,000",
  "Cancer History",
  "Cancer survivor",
  "Never had cancer",
  "Census Region",
  "Northeast",
  "Midwest",
  "South",
  "West"
)

# Substep 4: add labels to every column
# Substep 4.1: generate label list
col_label <- c(
  "",
  "Not at all (Marginal effect, 95% CI)",
  "A little (Marginal effect, 95% CI)",
  "Some (Marginal effect, 95% CI)",
  "A lot (Marginal effect, 95% CI)"
)




#### Mutivariable logistic regression --------------------------------------------------

# Set Stata code
stata_code <- 
  "// Set survey
svyset _n [pw=PERSON_FINWT0], ///
    jkrweight(PERSON_FINWT1-PERSON_FINWT50, multiplier(0.98)) ///
    vce(jackknife) mse
    
// Table 2: Generalized ordered logit model 
svy: gologit2 ClimateChgHarmHealth_f i.gender_f i.age_f i.race_group_f i.edu_group_f i.income_group_f i.cancer_history_f i.region_group_f

// Table 2: Calculate marginal effect
margins, dydx(*)"

# Run and store Stata result
#stata(stata_code, data.in = raw_sel)

# Store Stata results
table_2$harm_not_at_all <- c(
  "",
  "Base outcome", "-0.11 (-0.16 to -0.06)",
  "",
  "Base outcome", "0.06 (-0.01 to 0.13)", "0.11 (0.04 to 0.17)", "0.08 (0.03 to 0.14)", "0.10 (0.03 to 0.18)",
  "",
  "Base outcome", "-0.10 (-0.15 to -0.05)", "-0.07 (-0.13 to -0.01)", "-0.04 (-0.13 to 0.04)",
  "",
  "Base outcome", "0.01 (-0.06 to 0.08)", "-0.07 (-0.14 to -0.01)",
  "",
  "Base outcome", "-0.08 (-0.15 to -0.01)", "-0.02 (-0.09 to 0.05)",
  "",
  "Base outcome", "-0.03 (-0.10 to 0.04)",
  "",
  "Base outcome", "0.04 (-0.03 to 0.12)", "0.08 (0.03 to 0.13)", "-0.01 (-0.07 to 0.05)"
)

table_2$harm_a_little <- c(
  "",
  "Base outcome", "0.03 (-0.01 to 0.07)",
  "",
  "Base outcome", "0.02 (-0.05 to 0.09)", "0.01 (-0.05 to 0.07)", "-0.03 (-0.09 to 0.03)", "0.01 (-0.05 to 0.08)",
  "",
  "Base outcome", "0.03 (-0.03 to 0.09)", "-0.01 (-0.06 to 0.04)", "0.05 (-0.03 to 0.14)",
  "",
  "Base outcome", "-0.04 (-0.11 to 0.03)", "-0.08 (-0.15 to -0.02)",
  "",
  "Base outcome", "0.06 (0.00 to 0.11)", "0.06 (0.00 to 0.12)",
  "",
  "Base outcome", "0.02 (-0.03 to 0.07)",
  "",
  "Base outcome", "0.00 (-0.06 to 0.06)", "0.01 (-0.05 to 0.06)", "0.00 (-0.06 to 0.06)"
)

table_2$harm_some <- c(
  "",
  "Base outcome", "0.02 (-0.02 to 0.07)",
  "",
  "Base outcome", "-0.00 (-0.04 to 0.03)", "-0.02 (-0.09 to 0.04)", "0.00 (-0.08 to 0.08)", "-0.01 (-0.10 to 0.08)",
  "",
  "Base outcome", "0.02 (-0.05 to 0.09)", "0.01 (-0.06 to 0.07)", "-0.06 (-0.13 to 0.01)",
  "",
  "Base outcome", "-0.02 (-0.09 to 0.06)", "0.08 (0.01 to 0.15)",
  "",
  "Base outcome", "0.06 (-0.02 to 0.13)", "-0.01 (-0.08 to 0.06)",
  "",
  "Base outcome", "0.03 (-0.03 to 0.09)",
  "",
  "Base outcome", "-0.00 (-0.09 to 0.08)", "-0.06 (-0.13 to 0.01)", "-0.04 (-0.11 to 0.04)"
)

table_2$harm_a_lot <- c(
  "",
  "Base outcome", "0.05 (0.01 to 0.10)",
  "",
  "Base outcome", "-0.07 (-0.15 to 0.00)", "-0.10 (-0.17 to -0.02)", "-0.06 (-0.13 to 0.01)", "-0.10 (-0.19 to -0.01)",
  "",
  "Base outcome", "0.05 (-0.02 to 0.12)", "0.07 (0.01 to 0.13)", "0.05 (-0.05 to 0.16)",
  "",
  "Base outcome", "0.05 (-0.01 to 0.12)", "0.08 (0.01 to 0.14)",
  "",
  "Base outcome", "-0.03 (-0.11 to 0.04)", "-0.03 (-0.10 to 0.04)",
  "",
  "Base outcome", "-0.02 (-0.08 to 0.05)",
  "",
  "Base outcome", "-0.04 (-0.11 to 0.03)", "-0.05 (-0.13 to 0.02)", "0.04 (-0.05 to 0.13)"
)

# Add labels
for(i in 1:5) {
  attr(table_2[, i], "label") <- col_label[i]
}



#### Generate and modify flex table --------------------------------------------------
flextable_2 <- table_2 %>%
  
  # Transform to flextable
  flextable() %>%
  
  # Add title
  set_caption(
    caption = as_paragraph(
      as_chunk("Table 2. ",
               props = fp_text(
                 bold = TRUE,
                 color = "black"
               )
      ),
      as_chunk("Differences in believing climate change will harm health by sociodemographic characteristics")
    ),
    align_with_table = FALSE
  ) %>%
  
  # Set line spacing
  line_spacing(part = "header", space = 1.5) %>%
  
  # Set all fonts to 'Calibri'
  font(fontname = "TimesNewRoman", part = "all") %>%
  
  # Set to bold
  bold(i = c(3, 7, 9, 12), j = c(2, 5), part = "body") %>%
  bold(i = c(8, 11, 29), j = c(2), part = "body") %>%
  bold(i = c(18), j = c(2:5), part = "body") %>%
  bold(i = c(21), j = c(2:3), part = "body") %>%
  bold(i = c(22), j = c(3), part = "body") %>%
  
  # Modify alignment
  align(align = "center", i = 1, j = 2:5, part = "header") %>%
  align(align = "center", i = 1:30, j = 2:5, part = "body") %>%
  
  # Set paddings
  padding(
    i = c(2, 3, 5:9, 11:14, 16:18, 20:22, 24, 25, 27:30),
    j = 1,
    padding.left = 20,
    part = "body"
  ) %>%
  
  # Auto fit column width
  autofit()




#### Display table 2  --------------------------------------------------

flextable_2
