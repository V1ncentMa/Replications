
#### Data frame preparation --------------------------------------------------


# Generate initial data frame
# Substep 1: generate column names
col_names <- c(
  "characteristics",
  "tobacco_current",
  "tobacco_former",
  "tobacco_never",
  "p_value_tobacco",
  "ecig_ever",
  "ecig_never",
  "p_value_ecig"
)

# Substep 2: update column names and name the data frame
table_2 <- data.frame(
  
  # 24 rows and 8 columns
  matrix(NA, nrow = 24, ncol = 8)
)

colnames(table_2) <- col_names

# Substep 3: add row names
table_2$characteristics <- c(
  "Relative trust",
  "Tobacco-related relative trust",
  "Trust health professionals more than tobacco companies",
  "Trust tobacco companies as much or more than health professionals",
  "E-cigarette-related relative trust",
  "Trust health professionals more than e-cigarette companies",
  "Trust e-cigarette companies as much or more than health professionals",
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
label_tb2 <- c(
  "Characteristics",
  "Current, n (%) or M (SE)",
  "Former, n (%) or M (SE)",
  "Never, n (%) or M (SE)",
  "p - Value",
  "Ever, n (%) or M (SE)",
  "Never, n (%) or M (SE)",
  "p - Value"
)

# Substep 4.2: add labels
for(i in 1:8) {
  attr(table_2[, i], "label") <- label_tb2[i]
}




#### Chi-squared tests for categorical variables --------------------------------------------------


# Relative trust
# Column ---- Smoking status: current smoker
# Subgroup 1
table_2[3, "tobacco_current"] <- paste(
  sum(raw_sel$smoking_status == 1 & raw_sel$tobacco_rel_trust == 0, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["tobacco_rel_trust"] == 0), "smoking_status", 1),
  ")",
  sep = ""
)

# Subgroup 2
table_2[4, "tobacco_current"] <- paste(
  sum(raw_sel$smoking_status == 1 & raw_sel$tobacco_rel_trust == 1, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["tobacco_rel_trust"] == 1), "smoking_status", 1),
  ")",
  sep = ""
)


# Column ---- Smoking status: former smoker
# Subgroup 1
table_2[3, "tobacco_former"] <- paste(
  sum(raw_sel$smoking_status == 2 & raw_sel$tobacco_rel_trust == 0, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["tobacco_rel_trust"] == 0), "smoking_status", 2),
  ")",
  sep = ""
)

# Subgroup 2
table_2[4, "tobacco_former"] <- paste(
  sum(raw_sel$smoking_status == 2 & raw_sel$tobacco_rel_trust == 1, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["tobacco_rel_trust"] == 1), "smoking_status", 2),
  ")",
  sep = ""
)


# Column ---- Smoking status: never smoker
# Subgroup 1
table_2[3, "tobacco_never"] <- paste(
  sum(raw_sel$smoking_status == 3 & raw_sel$tobacco_rel_trust == 0, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["tobacco_rel_trust"] == 0), "smoking_status", 3),
  ")",
  sep = ""
)

# Subgroup 2
table_2[4, "tobacco_never"] <- paste(
  sum(raw_sel$smoking_status == 3 & raw_sel$tobacco_rel_trust == 1, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["tobacco_rel_trust"] == 1), "smoking_status", 3),
  ")",
  sep = ""
)


# Column ---- p value of smoking status
table_2[3, "p_value_tobacco"] <- round(
  svychisq(
    
    # Formula
    ~ tobacco_rel_trust_f + smoking_status_f,
    
    # Design
    design = hints_survey,
    statistic = "F"
  )$p.value,
  4
)


# Column ---- E-cigarette use: ever smoker
# Subgroup 1
table_2[6, "ecig_ever"] <- paste(
  sum(raw_sel$ecig_use == 1 & raw_sel$ecig_rel_trust == 0, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["ecig_rel_trust"] == 0), "ecig_use", 1),
  ")",
  sep = ""
)

# Subgroup 2
table_2[7, "ecig_ever"] <- paste(
  sum(raw_sel$ecig_use == 1 & raw_sel$ecig_rel_trust == 1, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["ecig_rel_trust"] == 1), "ecig_use", 1),
  ")",
  sep = ""
)


# Column ---- E-cigarette use: never smoker
# Subgroup 1
table_2[6, "ecig_never"] <- paste(
  sum(raw_sel$ecig_use == 0 & raw_sel$ecig_rel_trust == 0, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["ecig_rel_trust"] == 0), "ecig_use", 0),
  ")",
  sep = ""
)

# Subgroup 2
table_2[7, "ecig_never"] <- paste(
  sum(raw_sel$ecig_use == 0 & raw_sel$ecig_rel_trust == 1, na.rm = TRUE),
  " (",
  wpc_calculator(raw_sel %>% filter(raw_sel["ecig_rel_trust"] == 1), "ecig_use", 0),
  ")",
  sep = ""
)


# Column ---- p value of e-cigarette use
table_2[6, "p_value_ecig"] <- round(
  svychisq(
    
    # Formula
    ~ ecig_rel_trust_f + ecig_use_f,
    
    # Design
    design = hints_survey,
    statistic = "F"
  )$p.value,
  4
)


# Demographics
for(i in 1:5){
  
  # Column ---- Smoking status: current smoker
  # Subgroup 1
  table_2[(i + 2) * 3 + 1, "tobacco_current"] <- paste(
    sum(raw_sel[demographics[i]] == 1 & raw_sel$smoking_status == 1, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "smoking_status", 1),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_2[(i + 2) * 3 + 2, "tobacco_current"] <- paste(
    sum(raw_sel[demographics[i]] == 0 & raw_sel$smoking_status == 1, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "smoking_status", 1),
    ")",
    sep = ""
  )
  
  
  # Column ---- Smoking status: former smoker
  # Subgroup 1
  table_2[(i + 2) * 3 + 1, "tobacco_former"] <- paste(
    sum(raw_sel[demographics[i]] == 1 & raw_sel$smoking_status == 2, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "smoking_status", 2),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_2[(i + 2) * 3 + 2, "tobacco_former"] <- paste(
    sum(raw_sel[demographics[i]] == 0 & raw_sel$smoking_status == 2, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "smoking_status", 2),
    ")",
    sep = ""
  )
  
  
  # Column ---- Smoking status: never smoker
  # Subgroup 1
  table_2[(i + 2) * 3 + 1, "tobacco_never"] <- paste(
    sum(raw_sel[demographics[i]] == 1 & raw_sel$smoking_status == 3, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "smoking_status", 3),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_2[(i + 2) * 3 + 2, "tobacco_never"] <- paste(
    sum(raw_sel[demographics[i]] == 0 & raw_sel$smoking_status == 3, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "smoking_status", 3),
    ")",
    sep = ""
  )
  
  
  # Column ---- p value of tobacco-related relative trust
  table_2[(i + 2) * 3 + 1, "p_value_tobacco"] <- round(
    svychisq(
      reformulate(termlabels = c(demographics[i], "smoking_status_f"), response = NULL),
      design = hints_survey,
      statistic = "F"
    )$p.value,
    4
  )
  
  
  
  # Column ---- E-cigarette relative trust: trust information from health professionals more than e-cigarette companies
  # Subgroup 1
  table_2[(i + 2) * 3 + 2, "ecig_ever"] <- paste(
    sum(raw_sel[demographics[i]] == 1 & raw_sel$ecig_use == 1, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "ecig_use", 1),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_2[(i + 2) * 3 + 1, "ecig_ever"] <- paste(
    sum(raw_sel[demographics[i]] == 0 & raw_sel$ecig_use == 1, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "ecig_use", 1),
    ")",
    sep = ""
  )
  
  
  # Column ---- E-cigarette relative trust: trust information from e-cigarette companies as much or more than health professionals
  # Subgroup 1
  table_2[(i + 2) * 3 + 1, "ecig_never"] <- paste(
    sum(raw_sel[demographics[i]] == 1 & raw_sel$ecig_use == 1, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 1), "ecig_use", 0),
    ")",
    sep = ""
  )
  
  # Subgroup 2
  table_2[(i + 2) * 3 + 2, "ecig_never"] <- paste(
    sum(raw_sel[demographics[i]] == 0 & raw_sel$ecig_use == 1, na.rm = TRUE),
    " (",
    wpc_calculator(raw_sel %>% filter(raw_sel[demographics[i]] == 0), "ecig_use", 0),
    ")",
    sep = ""
  )
  
  
  # Column ---- p value of e-cigarette-related relative trust
  
  
  
  
  table_2[(i + 2) * 3 + 1, "p_value_ecig"] <- round(
    svychisq(
      reformulate(termlabels = c(demographics[i], "ecig_use_f"), response = NULL),
      design = hints_survey,
      statistic = "F"
    )$p.value,
    4
  )
}







#### One-way analysis of variance (ANOVA) --------------------------------------------------


# Tobacco smoking status
# Substep 1: Generate column names
col_names <- c("tobacco_current", "tobacco_former", "tobacco_never")

# Substep 2: Calculation
for(i in 1:3){
  
  # Update design
  design <- svrepdesign(
    
    # data filtration
    data = raw_sel %>% filter(smoking_status == i),
    
    # Assign weights
    weights = ~ weight,
    
    # Assign replicated weights
    repweights = select(raw_sel %>% filter(smoking_status == i), starts_with("person_")),
    
    # Weights type
    type = "JK1",
    
    # Combine weights
    combined.weights = TRUE,
  )
  
  # Assign values
  table_2[24, col_names[i]] <- paste(
    
    # Mean
    round(svymean(~ age, design = design, na.rm = TRUE), 1),
    " (",
    
    # Standard error
    round(SE(svymean(~ age, design = design, na.rm = TRUE)), 2),
    ")",
    sep = ""
  )
}

# Substep 3: one-way analysis of variance (ANOVA)
table_2[24, "p_value_tobacco"] <- round(
  
  # Statistical test
  regTermTest(
    
    # GLM regression
    svyglm(
      
      # Formula
      age ~ factor(smoking_status),
      
      # Set design
      design = hints_survey
    ),
    
    # Set factor
    ~ factor(smoking_status),
    
    # Set method
    method = "Wald")$p,
  4
)


# Ecigarette use status
# Substep 1: Generate column names
col_names <- c("ecig_never", "ecig_ever")

# Substep 2: Calculation
for(i in 1:2){
  
  # Update design
  design <- svrepdesign(
    
    # data filtration
    data = raw_sel %>% filter(ecig_use == i - 1),
    
    # Assign weights
    weights = ~ weight,
    
    # Assign replicated weights
    repweights = select(raw_sel %>% filter(ecig_use == i - 1), starts_with("person_")),
    
    # Weights type
    type = "JK1",
    
    # Combine weights
    combined.weights = TRUE,
  )
  
  # Assign values
  table_2[24, col_names[i]] <- paste(
    
    # Mean
    round(svymean(~ age, design = design, na.rm = TRUE), 1),
    " (",
    
    # Weighted SE
    round(SE(svymean(~ age, design = design, na.rm = TRUE)), 2),
    ")",
    sep = ""
  )
}


# Substep 3: one-way analysis of variance (ANOVA)
table_2[24, "p_value_ecig"] <- round(
  
  # Statistical test
  regTermTest(
    
    # GLM regression
    svyglm(
      
      # Formula
      age ~ factor(ecig_use),
      
      # Set design
      design = hints_survey
    ),
    
    # Set factor
    ~ factor(ecig_use),
    
    # Set method
    method = "Wald")$p,
  4
)


# N/A
table_2[6:7, "tobacco_current"] <- "N / A"
table_2[6:7, "tobacco_former"] <- "N / A"
table_2[6:7, "tobacco_never"] <- "N / A"
table_2[3:4, "ecig_ever"] <- "N / A"
table_2[3:4, "ecig_never"] <- "N / A"


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
      as_chunk("Differences in smoking status and E-cigarette use by relative trust and demographics.")
    ),
    align_with_table = FALSE
  ) %>%
  
  # Add footnote
  add_footer_lines("Note: Weighted percentages, means, and standard errors are presented. Data from the Health Information National Trends Survey-FDA Cycle.") %>%
  
  # Add header rows 'Tobacco-related relative trust' and 'E-cigarette-related relative trust'
  add_header_row(
    values = c(
      "",
      paste("Smoking Status, n =", sum(!is.na(raw_sel$smoking_status))),
      paste("Smoking Status, n =", sum(!is.na(raw_sel$ecig_use)))),
    colwidths = c(1, 4, 3)) %>%
  
  # Remove all borders
  border_remove() %>%
  
  # Add horizontal lines inside the headers
  hline(
    i = 1, 
    j = 2:8, 
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
  align(align = "center", i = 1:2, j = 2:8, part = "header") %>%
  align(align = "right", i = 1:24, j = 2:8, part = "body") %>%
  
  # Set paddings
  padding(
    i = c(3:4, 6:7, 10:11, 13:14, 16:17, 19:20, 22:23),
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
  italic(i = 8, part = "body") %>%
  
  # Auto fit column width
  autofit()




#### Display table 2  --------------------------------------------------

flextable_2

