

#### Data frame preparation --------------------------------------------------

# Generate initial data frame
# Substep 1: generate column names
col_names <- c(
  "characteristics",
  "total",
  "year_18",
  "year_20",
  "year_22",
  "p_value"
)

# Substep 2: update column names and name the data frame
table_1 <- data.frame(
  
  # 61 rows and 8 colemns
  matrix(NA, nrow = 29, ncol = 6)
)

colnames(table_1) <- col_names

# Substep 3: add row names
table_1$characteristics <- c(
  "Sex, n(%)",
  "Male",
  "Female",
  "Age (years), n (%); mean (SD)",
  "Millennials",
  "Generation X",
  "Baby boomers",
  "Silent generation",
  "Race and ethnicity, n (%)",
  "Asian",
  "Black",
  "Hispanic",
  "White",
  "Other",
  "Education, n (%)",
  "Less than high school graduate",
  "High school graduate",
  "Some college",
  "College graduate",
  "Income (US $), n (%)",
  "<20,000",
  "20,000-34,999",
  "35,000-49,999",
  "50,000-74,999",
  "≥75,000",
  "Health status, mean (SD)",
  "eHealth use in the past year, n (%)",
  "No",
  "Yes"
)

# Substep 4: add labels to every column
# Substep 4.1: generate label list
label_tb1 <- c(
  "Characteristics",
  paste("Full sample, (N = ", nrow(raw), ")"),
  paste("2018, ( N = ", nrow(raw %>% filter(year == 1)), ")"),
  paste("2020, ( N = ", nrow(raw %>% filter(year == 2)), ")"),
  paste("2022, ( N = ", nrow(raw %>% filter(year == 3)), ")"),
  "P value (chi-square or ANOVA)"
)

# Substep 4.2: add labels
for(i in 1:6) {
  attr(table_1[, i], "label") <- label_tb1[i]
}




#### Descriptive characteristics --------------------------------------------------

# Set pointer
pointer <- c(1:29)

# Set censor
censor <- c(1, 4, 9, 15, 20, 26, 27)

# Reset pointer
pointer <- setdiff(pointer, censor)

# Vector of the variable's value
value <- c(0:1, 1:4, 4, 2, 3, 1, 5, 1:4, 1:5, 0:1)

# Vector of the variable's name
characteristics <- c(
  rep("selfgender", 2),
  rep("age_cohort", 4),
  rep("raceethn5", 5),
  rep("educa", 4), 
  rep("hhinc", 5), 
  rep("ehealth_use", 2)
)

# Column "Total"
for(i in 1:22) {
  
  
  if(i >= 3 & i <= 6) {
    
    # 'Age cohort' rows
    table_1[pointer[i], 2] <- paste(
      
      # Total number of observation
      sum(raw[characteristics[i]] == value[i], na.rm = TRUE),
      " (",
      
      # Weighted percentage
      wpc_calculator(hints_svy, characteristics[i], value[i]),
      "); ",
      
      # Mean
      round(
        raw %>% 
          filter(raw[characteristics[i]] == value[i]) %>% 
          pull(age) %>% 
          mean(na.rm = TRUE), 
        1
      ),
      " (",
      
      # SD
      round(
        raw %>% 
          filter(raw[characteristics[i]] == value[i]) %>% 
          pull(age) %>%
          sd(na.rm = TRUE),
        2
      ),
      ")"
      
    )
    
  } else {
    
    # Other rows
    table_1[pointer[i], 2] <- paste(
      
      # Total number of observation
      sum(raw[characteristics[i]] == value[i], na.rm = TRUE),
      " (",
      
      # Weighted percentage
      wpc_calculator(hints_svy, characteristics[i], value[i]),
      ")"
    )
    
  }
  
}

# Column "Years"
for(i in 1:3) {
  
  for(j in 1:22) {
    
    if(j >= 3 & j <= 6) {
      
      # 'Age cohort' rows
      table_1[pointer[j], i + 2] <- paste(
        
        # Total number of observation
        raw %>% 
          filter(raw[characteristics[j]] == value[j] & raw$year == i) %>% 
          pull(age) %>% 
          nrow(),
        " (",
        
        # Weighted percentage
        wpc_calculator(hints_svy %>% filter(year == i), characteristics[j], value[j]),
        "); ",
        
        # Mean
        round(
          raw %>%
            filter(raw[characteristics[j]] == value[j] & raw$year == i) %>%
            pull(age) %>%
            mean(na.rm = TRUE),
          1
        ),
        " (",
        
        # SD
        round(
          raw %>%
            filter(raw[characteristics[j]] == value[j] & raw$year == i) %>%
            pull(age) %>% 
            sd(na.rm = TRUE),
          2
        ),
        ")"
        
      )
      
    } else {
      
      # Other rows
      table_1[pointer[j], i + 2] <- paste(
        
        # Total number of observation
        sum(raw[characteristics[j]] == value[j] & raw$year == i, na.rm = TRUE),
        " (",
        
        # Weighted percentage
        wpc_calculator(hints_svy %>% filter(year == i), characteristics[j], value[j]),
        ")"
      )
      
    }
    
  }
  
}

# Health status (column 'Total')
table_1[26, 2] <- paste(
  
  # Mean
  round(
    raw %>% 
      pull(generalhealth) %>% 
      mean(na.rm = TRUE),
    2
  ),
  " (",
  
  # SD
  round(
    raw %>% 
      pull(generalhealth) %>% 
      sd(na.rm = TRUE),
    2
  ),
  ")"
)

# Health status (column 'Years')
for(i in 1:3) {
  
  table_1[26, i + 2] <- paste(
    
    # Mean
    round(
      raw %>%
        filter(year == i) %>%
        pull(generalhealth) %>%
        mean(na.rm = TRUE),
      2
    ),
    " (",
    
    # SD
    round(
      raw %>%
        filter(year == i) %>%
        pull(generalhealth) %>%
        sd(na.rm = TRUE),
      2
    ),
    ")"
  )
}




#### Chi-square tests --------------------------------------------------

# Vector of the variable's name
characteristics <- c("selfgender", "age_cohort", "raceethn5", "educa", "hhinc", "ehealth_use")

# Censor
censor <- c(1, 4, 9, 15, 20, 27)

for(i in 1:6) {
  
  table_1[censor[i], 6] <- chisq_calculator_general(raw, characteristics[i], "year")
  
}




#### ANOVA test --------------------------------------------------

# ANOVA test
aov_result <- aov(generalhealth ~ year_f, data = raw)
table_1[26, 6] <- round(
  summary(aov_result)[[1]]$`Pr(>F)`[1],
  2
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
      as_chunk("Descriptive characteristics of the study sample from the Health Information National Trends Survey for 2018, 2020, and 2022.")
    ),
    align_with_table = FALSE
  ) %>%
  
  # Set line spacing
  line_spacing(part = "header", space = 1.5) %>%
  
  # Set all fonts to 'Calibri'
  font(fontname = "Calibri", part = "all") %>%
  
  # Modify alignment
  align(align = "center", i = 1, j = 2:6, part = "header") %>%
  align(align = "center", i = 1:29, j = 2:6, part = "body") %>%
  
  # Set paddings
  padding(
    i = pointer,
    j = 1,
    padding.left = 20,
    part = "body"
  ) %>%
  
  # Auto fit column width
  autofit()


#### Display flextable 1 --------------------------------------------------

flextable_1