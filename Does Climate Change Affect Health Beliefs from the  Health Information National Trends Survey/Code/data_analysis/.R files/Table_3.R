
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
table_3 <- data.frame(
  
  # 34 rows and 5 columns
  matrix(NA, nrow = 9, ncol = 5)
)

colnames(table_3) <- col_names

# Substep 3: add row names
table_3$characteristics <- c(
  "Trust information about cancer from scientists",
  "Some/A little/Not at all",
  "A lot",
  "Health recommendations from experts conflict",
  "Rarely/Never",
  "Often/Very often",
  "Health recommendations from experts change",
  "Rarely/Never",
  "Often/Very often"
)




#### Mutivariable logistic regression --------------------------------------------------

# Set Stata code
stata_code <- 
  "// Set survey
svyset _n [pw=PERSON_FINWT0], ///
    jkrweight(PERSON_FINWT1-PERSON_FINWT50, multiplier(0.98)) ///
    vce(jackknife) mse
    
// Table 3: Generalized ordered logit model 
svy: gologit2 ClimateChgHarmHealth_f i.CancerTrustScientists_f i.HealthRecsConflict_f i.HealthRecsChange_f i.gender_f i.age_f i.race_group_f i.edu_group_f i.income_group_f i.cancer_history_f i.region_group_f

// Table 3: Calculate marginal effect
margins, dydx(*)"

# Run and store Stata result
#stata(stata_code, data.in = raw_sel)

# Store Stata results
table_3$harm_not_at_all <- c(
  "",
  "Base outcome", "-0.14 (-0.20 to -0.08)",
  "",
  "Base outcome", "0.08 (0.02 to 0.14)",
  "",
  "Base outcome", "0.03 (-0.03 to 0.09)"
)

table_3$harm_a_little <- c(
  "",
  "Base outcome", "-0.06 (-0.10 to -0.02)",
  "",
  "Base outcome", "-0.03 (-0.08 to 0.02)",
  "",
  "Base outcome", "-0.01 (-0.07 to 0.05)"
)

table_3$harm_some <- c(
  "",
  "Base outcome", "0.08 (0.03 to 0.12)",
  "",
  "Base outcome", "-0.05 (-0.12 to 0.02)",
  "",
  "Base outcome", "-0.03 (-0.11 to -0.05)"
)

table_3$harm_a_lot <- c(
  "",
  "Base outcome", "0.12 (0.08 to 0.17)",
  "",
  "Base outcome", "0.00 (-0.05 to 0.05)",
  "",
  "Base outcome", "0.02 (-0.03 to 0.06)"
)

# Add labels
for(i in 1:5) {
  attr(table_2[, i], "label") <- col_label[i]
}



#### Generate and modify flex table --------------------------------------------------
flextable_3 <- table_3 %>%
  
  # Transform to flextable
  flextable() %>%
  
  # Add title
  set_caption(
    caption = as_paragraph(
      as_chunk("Table 3. ",
               props = fp_text(
                 bold = TRUE,
                 color = "black"
               )
      ),
      as_chunk("Odds of believing climate change will harm health by opinions about scientists and expert recommendations")
    ),
    align_with_table = FALSE
  ) %>%
  
  # Set line spacing
  line_spacing(part = "header", space = 1.5) %>%
  
  # Set all fonts to 'Calibri'
  font(fontname = "TimesNewRoman", part = "all") %>%
  
  # Set to bold
  bold(i = c(3), j = c(2:5), part = "body") %>%
  bold(i = c(6), j = c(2), part = "body") %>%
  
  # Modify alignment
  align(align = "center", i = 1, j = 2:5, part = "header") %>%
  align(align = "center", i = 1:9, j = 2:5, part = "body") %>%
  
  # Set paddings
  padding(
    i = c(2, 3, 5, 6, 8, 9),
    j = 1,
    padding.left = 20,
    part = "body"
  ) %>%
  
  # Auto fit column width
  autofit()




#### Display table 3  --------------------------------------------------

flextable_3


rm(col_label, col_names, group_label, i, j, mapped_i, pointer, stata_code,characteristics)
