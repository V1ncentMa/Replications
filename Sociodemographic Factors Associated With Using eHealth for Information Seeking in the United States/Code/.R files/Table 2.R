

#### Data frame preparation --------------------------------------------------


# Generate initial data frame
# Substep 1: generate column names
col_names <- c(
  "characteristics",
  "adj_or_pooled",
  "adj_or_millennials",
  "adj_or_generation_x",
  "adj_or_baby_boomers",
  "adj_or_silent_generation"
)

# Substep 2: update column names and name the data frame
table_2 <- data.frame(
  
  # 60 rows and 2 columns
  matrix(NA, nrow = 16, ncol = 6)
)

colnames(table_2) <- col_names

# Substep 3: add row names
table_2$characteristics <- c(
  "Female",
  "Age",
  "Asian",
  "Black",
  "Hispanic",
  "Other",
  "High school graduate",
  "Some college",
  "College graduate",
  "US $20,000-$34,999",
  "US $35,000-$49,999",
  "US $50,000-$74,999",
  "≥US $75,000",
  "Health status",
  "2020",
  "2022"
)


# Substep 4: add labels to every column
# Substep 4.1: generate label list
label_tb2 <- c(
  "Characteristics",
  "Pooled sample, OR (95% CI)",
  "Millennials, OR (95% CI)",
  "Generation X, OR (95% CI)",
  "Baby boomers, OR (95% CI)",
  "Silent generation, OR (95% CI)"
)

# Substep 4.2: add labels
for(i in 1:6) {
  attr(table_2[, i], "label") <- label_tb2[i]
}




#### Multivariable logistic regression --------------------------------------------------


# Column 'Pooled Sample'

# Multivariable logistic regression
glm_result <- svyglm(
  
  # Design
  design = hints_svy,
  
  # Formula
  reformulate(
    termlabels = c(
      "selfgender_f",
      "age",
      "raceethn5_f",
      "educa_f",
      "hhinc_f",
      "generalhealth",
      "year_f"
    ),
    response = "ehealth_use_f"
  ),
  
  # Family
  family = quasibinomial()
  
)

# Check the result
summary(glm_result)

# Set pointer
pointer <- c(1:2, 4, 5, 3, 6:16)

# Add to table 2
for(i in 1:16) {
  table_2[i, 2] <- extract_or_ci(glm_result, pointer[i])
}


# Column 'Age cohort'
# Set pointer
pointer <- c(1:2, 5, 3, 4, 6:16)

# Add to table 2
for(i in 1:4) {
  
  glm_result <- svyglm(
    
    # Design
    design = hints_svy %>% filter(age_cohort == i),
    
    # Formula
    reformulate(
      termlabels = c(
        "selfgender_f",
        "age",
        "raceethn5_f",
        "educa_f",
        "hhinc_f",
        "generalhealth",
        "year_f"
      ),
      response = "ehealth_use_f"
    ),
    
    # Family
    family = quasibinomial()
    
  )
  
  # Check the result
  summary(glm_result)
  
  # Add to table 2
  for(j in 1:16) {
    
    table_2[j, i + 2] <- extract_or_ci(glm_result, pointer[j])
    
  }
  
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
                 color = "#0A0D79"
               )
      ),
      as_chunk(paste(
        "Logistic regression for using eHealth for information seeking, stratified by age cohort, Health Information National Trends Survey for",
        "2018 (n = ",
        nrow(raw %>% filter(year == 1)),
        "), 2020 (n = ",
        nrow(raw %>% filter(year == 2)),
        "), and 2022 (n = ",
        nrow(raw %>% filter(year == 3)),
        ")"
      ))
    ),
    align_with_table = FALSE
  ) %>%
  
  # Set line spacing
  line_spacing(part = "header", space = 1.5) %>%
  
  # Set all fonts to 'Calibri'
  font(fontname = "Calibri", part = "all") %>%
  
  # Set statistical significance to italic
  italic(i = c(1:2, 8:9, 11:16), j = 2, part = "body") %>%
  italic(i = c(1, 8:9, 12:13), j = 3, part = "body") %>%
  italic(i = c(1, 8:9, 12:14, 16), j = 4, part = "body") %>%
  italic(i = c(1:2, 7:9, 12:13, 15:16), j = 5, part = "body") %>%
  italic(i = c(2, 8:9, 12:13, 16), j = 6, part = "body") %>%
  
  # Modify alignment
  align(align = "center", i = 1, j = 1:6, part = "header") %>%
  align(align = "center", i = 1:16, j = 2, part = "body") %>%
  
  # Auto fit column width
  autofit()


#### Display flextable 2 --------------------------------------------------

flextable_2



# Clean the environment
rm(aov_result, glm_result, i, j, pointer, censor, characteristics, col_names, label_tb1, label_tb2, value)