#### Data frame preparation --------------------------------------------------


# Generate initial data frame
# Substep 1: generate column names
col_names <- c(
  "characteristics",
  "total",
  "health_prof_more_than_tobacco",
  "tobacco_company_more",
  "p_value_tobacco",
  "health_prof_more_than_ecig",
  "ecig_company_more",
  "p_value_ecig"
)

# Substep 2: update column names and name the data frame
table_1 <- data.frame(
  
  # 17 rows and 8 colemns
  matrix(NA, nrow = 17, ncol = 8)
)

colnames(table_1) <- col_names

# Substep 3: add row names
table_1$characteristics <- c(
  "Demographics",
  "Race / ethnicity",
  "Racial / ethnicminority",
  "Non-Hispanic White",
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
label_tb1 <- c(
  "Characteristics",
  paste("Total, N = ", nrow(raw_sel), ", n (%) or M (SE)"),
  "Health professionals more than tobacco companies, n (%) or M (SE)",
  "Tobacco companies as much or more than health professionals, n (%) or M (SE)",
  "p - Value",
  "Health professionals more than e-cigarette companies, n (%) or M (SE)",
  "E-cigarette companies as much or more than health professionals, n (%) or M (SE)",
  "p - Value"
)

# Substep 4.2: add labels
for(i in 1:8) {
  attr(table_1[, i], "label") <- label_tb1[i]
}

# Generate survey design object
hints_survey <- svrepdesign(
  
  # Data frame
  data = raw_sel,
  
  # Assign weights
  weights = ~ weight,
  
  # Assign
  repweights = select(raw_sel, num_range("person_finwt", 1:50)),
  
  # Jackknife type
  type = "JK1",
  
  # Combined weights
  combined.weights = TRUE,
)




#### Chi-squared tests for categorical variables --------------------------------------------------


# Generate demographic list
demographics <- c("race", "education", "income", "gender", "geographic_area")


# Calculation
for(i in 1:5){
  
  # Column ---- Total number of observations and weighted percentages
  # Subgroup 1
  table_1[i * 3, "total"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 1, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel, demographics[i], 1),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_1[i * 3 + 1, "total"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 0, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel, demographics[i], 0),
    ")",
    sep = ""
  )
  
  
  # Column ---- Tobacco relative trust: trust information from health professionals more than tobacco companies
  # Subgroup 1
  table_1[i * 3, "health_prof_more_than_tobacco"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 1 & raw_sel$tobacco_rel_trust == 0, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "tobacco_rel_trust", 0),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_1[i * 3 + 1, "health_prof_more_than_tobacco"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 0 & raw_sel$tobacco_rel_trust == 0, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "tobacco_rel_trust", 0),
    ")",
    sep = ""
  )
  
  
  # Column ---- Tobacco relative trust: trust information from tobacco companies as much or more than health professionals
  # Subgroup: 1
  table_1[i * 3, "tobacco_company_more"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 1 & raw_sel$tobacco_rel_trust == 1, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "tobacco_rel_trust", 1),
    ")",
    sep = ""
  )
  
  # Subgroup: 2
  table_1[i * 3 + 1, "tobacco_company_more"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 0 & raw_sel$tobacco_rel_trust == 1, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "tobacco_rel_trust", 1),
    ")",
    sep = ""
  )
  
  
  # Column ---- p value of tobacco-related relative trust
  table_1[i * 3, "p_value_tobacco"] <- round(
    
    # Chi-squared test
    svychisq(
      
      # Reformulate formula
      reformulate(termlabels = c(demographics[i], "tobacco_rel_trust"), response = NULL),
      
      # Assign survey design
      design = hints_survey,
      
      # F statistic
      statistic = "F"
    )$p.value,
    4
  )
  
  
  # Column ---- E-cigarette relative trust: trust information from health professionals more than e-cigarette companies
  # Subgroup 1
  table_1[i * 3, "health_prof_more_than_ecig"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 1 & raw_sel$ecig_rel_trust == 0, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "ecig_rel_trust", 0),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_1[i * 3 + 1, "health_prof_more_than_ecig"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 0 & raw_sel$ecig_rel_trust == 0, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "ecig_rel_trust", 0),
    ")",
    sep = ""
  )
  
  
  # Column ---- E-cigarette relative trust: trust information from e-cigarette companies as much or more than health professionals
  # Subgroup 1
  table_1[i * 3, "ecig_company_more"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 1 & raw_sel$ecig_rel_trust == 1, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "ecig_rel_trust", 1),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_1[i * 3 + 1, "ecig_company_more"] <- paste(
    
    # Total number of observation
    sum(raw_sel[demographics[i]] == 0 & raw_sel$ecig_rel_trust == 1, na.rm = TRUE),
    " (",
    
    # Weighted percentage
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "ecig_rel_trust", 1),
    ")",
    sep = ""
  )
  
  
  # Column ---- p value of e-cigarette-related relative trust
  table_1[i * 3, "p_value_ecig"] <- round(
    
    # Chi-squared test
    svychisq(
      
      # Reformulate formula
      reformulate(termlabels = c(demographics[i], "ecig_rel_trust"), response = NULL),
      
      # Assign survey
      design = hints_survey,
      
      # F statistics
      statistic = "F",)$p.value,
    4
  )
}




#### T tests for continuous variables --------------------------------------------------


# Column ---- Total number of observations and weighted percentages
# Age
table_1[17, "total"] <- paste(
  
  # Weighted mean
  round(svymean(~ age, design = hints_survey, na.rm = TRUE), 1),
  " (",
  
  # Weighted SE
  round(SE(svymean(~ age, design = hints_survey, na.rm = TRUE)), 2),
  ")",
  sep = ""
)


# Column ---- Tobacco relative trust: trust information from health professionals more than tobacco companies
# Age
design <- svrepdesign(
  
  # Data filtering
  data = raw_sel %>% filter(tobacco_rel_trust == 0),
  
  # Assign weight
  weights = ~ weight,
  
  # Assign repeated weights
  repweights = select(raw_sel %>% filter(tobacco_rel_trust == 0), starts_with("person_")),
  
  # Weights type
  type = "JK1",
  
  # Combine weights
  combined.weights = TRUE,
)

table_1[17, "health_prof_more_than_tobacco"] <- paste(
  
  # Weighted mean
  round(svymean(~ age, design = design, na.rm = TRUE), 1),
  " (",
  
  # Weighted SE
  round(SE(svymean(~ age, design = design, na.rm = TRUE)), 2),
  ")",
  sep = ""
)


# Column ---- Tobacco relative trust: trust information from tobacco companies as much or more than health professionals
# Age
design <- svrepdesign(
  
  # Data filtering
  data = raw_sel %>% filter(tobacco_rel_trust == 1),
  
  # Assign weight
  weights = ~ weight,
  
  # Assign repeated weights
  repweights = select(raw_sel %>% filter(tobacco_rel_trust == 1), starts_with("person_")),
  
  # Weights type
  type = "JK1",
  
  # Combine weights
  combined.weights = TRUE,
)

table_1[17, "tobacco_company_more"] <- paste(
  
  # Weighted mean
  round(svymean(~ age, design = design, na.rm = TRUE), 1),
  " (",
  
  # Weighted SE
  round(SE(svymean(~ age, design = design, na.rm = TRUE)), 2),
  ")",
  sep = ""
)


# Column ---- p value of tobacco-related relative trust
# Age
table_1[17, "p_value_tobacco"] <- round(svyttest(
  
  # Formula
  age ~ tobacco_rel_trust,
  
  # Design
  design = hints_survey
)$p.value,
4
)


# Column ---- E-cigarette relative trust: trust information from health professionals more than e-cigarette companies
# Age
design <- svrepdesign(
  
  # Data filtering
  data = raw_sel %>% filter(ecig_rel_trust == 0),
  
  # Assign weight
  weights = ~ weight,
  
  # Assign repeated weights
  repweights = select(raw_sel %>% filter(ecig_rel_trust == 0), starts_with("person_")),
  
  # Weights type
  type = "JK1",
  
  # Combine weights
  combined.weights = TRUE,
)

table_1[17, "health_prof_more_than_ecig"] <- paste(
  
  # Weighted mean
  round(svymean(~ age, design = design, na.rm = TRUE), 1),
  " (",
  
  # Weighted SE
  round(SE(svymean(~ age, design = design, na.rm = TRUE)), 2),
  ")",
  sep = ""
)


# Column ---- E-cigarette relative trust: trust information from e-cigarette companies as much or more than health professionals
# Age
design <- svrepdesign(
  
  # Data filtering
  data = raw_sel %>% filter(ecig_rel_trust == 1),
  
  # Weights
  weights = ~ weight,
  
  # Repeated weights
  repweights = select(raw_sel %>% filter(ecig_rel_trust == 1), starts_with("person_")),
  
  # Weights type
  type = "JK1",
  
  # Combine weights
  combined.weights = TRUE,
)

table_1[17, "ecig_company_more"] <- paste(
  
  # Weighted mean
  round(svymean(~ age, design = design, na.rm = TRUE), 1),
  " (",
  
  # Weighted SE
  round(SE(svymean(~ age, design = design, na.rm = TRUE)), 2),
  ")",
  sep = ""
)


# Column ---- p value of e-cigarette-related relative trust
# Age
table_1[17, "p_value_ecig"] <- round(svyttest(
  
  # Formula
  age ~ ecig_rel_trust,
  
  # Design
  design = hints_survey
)$p.value, 
4
)




#### Generate and modify flex table --------------------------------------------------


flextable_1 <- table_1 %>%
  
  # Transform to flextable
  flextable() %>%
  
  # Add title
  set_caption(
    caption = as_paragraph(
      as_chunk("Table 1. ",
               props = fp_text(
                 bold = TRUE,
                 color = "#0A0D79"
               )
      ),
      as_chunk("Differences in tobacco-related and E-cigarette-related relative trust by demographics.")
    ),
    align_with_table = FALSE
  ) %>%
  
  # Add footnote
  add_footer_lines("Note: Weighted percentages, means, and standard errors are presented. Data from the Health Information National Trends Survey-FDA Cycle.") %>%
  
  # Add header rows 'Trust information from:'
  add_header_row(
    values = c(
      "",
      "Trust information from:",
      "",
      "Trust information from:",
      ""
    ),
    colwidths = c(2, 2, 1, 2, 1)
  ) %>%
  
  # Add header rows 'Tobacco-related relative trust' and 'E-cigarette-related relative trust'
  add_header_row(
    values = c(
      "",
      paste("Tobacco-related relative trust, n =", sum(!is.na(raw_sel$tobacco_rel_trust))),
      paste("E-cigarette-related relative trust, n =", sum(!is.na(raw_sel$ecig_rel_trust)))),
    colwidths = c(2, 3, 3)) %>%
  
  # Remove all borders
  border_remove() %>%
  
  # Add horizontal lines inside the headers
  hline(
    i = 1, 
    j = 3:5, 
    border = fp_border(color = "#0A0D79", width = 2),
    part = "header"
  ) %>%
  
  hline(
    i = 1, 
    j = 6:8, 
    border = fp_border(color = "#0A0D79", width = 2),
    part = "header"
  ) %>%
  
  hline(
    i = 2, 
    j = 3:4, 
    border = fp_border(color = "#0A0D79", width = 2),
    part = "header"
  ) %>%
  
  hline(
    i = 2, 
    j = 6:7, 
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
  align(align = "center", i = 1:3, j = 2:8, part = "header") %>%
  align(align = "right", i = 1:17, j = 2:8, part = "body") %>%
  
  # Set paddings
  padding(
    i = c(3, 4, 6, 7, 9, 10, 12, 13, 15, 16),
    j = 1,
    padding.left = 20,
    part = "body"
  ) %>%
  
  # Set line spacing
  line_spacing(part = "header", space = 1.5) %>%
  
  # Set all fonts to 'Calibri'
  font(fontname = "Adora", part = "all") %>%
  
  # Set 'Demographic' to italic
  italic(i = 1, part = "body") %>%
  
  # Auto fit column width
  autofit()




#### Display table 1  --------------------------------------------------

flextable_1