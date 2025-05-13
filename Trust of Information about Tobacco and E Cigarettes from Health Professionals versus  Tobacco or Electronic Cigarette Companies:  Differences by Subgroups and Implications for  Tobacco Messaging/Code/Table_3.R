
#### Data frame preparation --------------------------------------------------


# Generate initial data frame
# Substep 1: generate column names
col_names <- c(
  "characteristics",
  "adj_or"
)

# Substep 2: update column names and name the data frame
table_3 <- data.frame(
  
  # 19 rows and 2 columns
  matrix(NA, nrow = 19, ncol = 2)
)

colnames(table_3) <- col_names

# Substep 3: add row names
table_3$characteristics <- c(
  "E-cigarette-related relative trust",
  "Trust e-cigarette companies as much or more than health professionals",
  "Trust health professionals more than e-cigarette companies",
  "Race / ethnicity",
  "Non-Hispanic White",
  "Racial / ethnic minority",
  "Education",
  "≤ HighSchool",
  "> HighSchool",
  "Income",
  "< $20000",
  "≥ $20000",
  "Gender",
  "Male",
  "Female",
  "Geographic area",
  "Non-metropolitan",
  "Metropolitan",
  "Age (years)"
)

# Substep 4: add labels to every column
# Substep 4.1: generate label list
label_tb3 <- c(
  "Characteristics",
  "aOR (95% CI)"
)

# Substep 4.2: add labels
for(i in 1:2) {
  attr(table_3[, i], "label") <- label_tb3[i]
}




#### Multinomial logistic regression --------------------------------------------------


# Logistic regression
glm_result <- svyglm(
  
  # Formula
  ecig_use_f ~ ecig_rel_trust_f + race_f + education_f + income_f + gender_f + geographic_area_f + age,
  
  # Assign design
  design = hints_survey,
  
  # Set family
  family = quasibinomial()
)


# Draw coefficients
glm_coef <- coef(glm_result)

# Transform to adjusted-OR
glm_adj_or <- round(exp(glm_coef), digits = 2)

# Calculate Variance-Covariance Matrix for glm result
glm_vcov_matrix <- vcov(glm_result)

# Calculate the standard error of the OR on the log scale
glm_log_or_se <- sqrt(diag(glm_vcov_matrix))

# Calculate the 95% CI on the log scale
glm_ci_lower <- round(exp(glm_coef - 1.96 * glm_log_or_se), digits = 2)
glm_ci_upper <- round(exp(glm_coef + 1.96 * glm_log_or_se), digits = 2)

# Add to table 3
for(i in 1:6){
  
  # Set 'Reference'
  table_3$adj_or[i * 3] = "REF"
  
  # Set adj-OR and CI
  table_3$adj_or[i * 3 - 1] = paste(
    glm_adj_or[i + 1],
    " (",
    glm_ci_lower[i + 1],
    ", ",
    glm_ci_upper[i + 1],
    ")",
    sep = ""
  )
  
  # Set space
  table_3$adj_or[i * 3 - 2] = NA
}

table_3$adj_or[19] = paste(
  
  # adj-OR
  glm_adj_or[8],
  " (",
  
  # Lower CI
  glm_ci_lower[8],
  ", ",
  
  # Higher CI
  glm_ci_upper[8],
  ")",
  sep = ""
)


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
                 color = "#0A0D79"
               )
      ),
      as_chunk("Multivariable binary logistic regression for the association between E-cigarette-related relative trust and E-cigarette use.")
    ),
    align_with_table = FALSE
  ) %>%
  
  # Add footnote
  add_footer_lines("Note: Weighted analyses are presented. Data from the Health Information National Trends Survey-FDA Cycle.") %>%
  
  # Add header rows 'Trust information from:'
  add_header_row(
    values = c(
      "",
      "E-cigarette use"
    ),
    colwidths = c(1, 1)
  ) %>%
  
  # Remove all borders
  border_remove() %>%
  
  # Add horizontal lines inside the headers
  hline(
    i = 1, 
    j = 2, 
    border = fp_border(color = "#0A0D79", width = 2),
    part = "header"
  ) %>%
  
  # Add horizontal line on the top of the header
  hline_top(border = fp_border(color = "#0A0D79", width = 2), part = "header") %>%
  
  # Add horizontal line on the top of the body
  hline_top(border = fp_border(color = "#0A0D79", width = 2), part = "body") %>%
  
  # Add horizontal line on the bottom of the body
  hline_bottom(border = fp_border(color = "#0A0D79", width = 2), part = "body") %>%
  
  # Modify alignment
  align(align = "center", i = 1, j = 2, part = "header") %>%
  align(align = "center",j = 2, part = "body") %>%
  
  # Set paddings
  padding(
    i = c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18),
    j = 1,
    padding.left = 20,
    part = "body"
  ) %>%
  
  # Set line spacing
  line_spacing(part = "header", space = 1.5) %>%
  
  # Set all fonts to 'Calibri'
  font(fontname = "Adora", part = "all") %>%
  
  # Auto fit column width
  autofit()




#### Display table 1 --------------------------------------------------

flextable_3