#### Data frame preparation --------------------------------------------------


# Generate initial data frame
# Substep 1: generate column names
col_names <- c(
  "characteristics",
  "total_sample",
  "harm_a_lot",
  "harm_some",
  "harm_a_little",
  "harm_not_at_all",
  "p_value"
)

# Substep 2: update column names and name the data frame
table_1 <- data.frame(
  
  # 30 rows and 7 columns
  matrix(NA, nrow = 30, ncol = 7)
)

colnames(table_1) <- col_names

# Substep 3: add row names
table_1$characteristics <- c(
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
  paste("Total Sample, (n = ", nrow(raw_sel), ") n"),
  paste("Climate Change Will Harm Health A Lot (n = ", nrow(raw_sel %>% filter(ClimateChgHarmHealth == 4)), ") n (weighted %)"),
  paste("Climate Change Will Harm Health Some (n = ", nrow(raw_sel %>% filter(ClimateChgHarmHealth == 3)), ") n (weighted %)"),
  paste("Climate Change Will Harm Health A Little (n = ", nrow(raw_sel %>% filter(ClimateChgHarmHealth == 2)), ") n (weighted %)"),
  paste("Climate Change Will Harm Health Not At All (n = ", nrow(raw_sel %>% filter(ClimateChgHarmHealth == 1)), ") n (weighted %)"),
  "P - Value"
)

# Substep 4.2: add labels
for(i in 1:7) {
  attr(table_1[, i], "label") <- col_label[i]
}




#### Descriptive Statistics --------------------------------------------------

# Column ---- Total Sample
# Gender
group_label <- c("Male", "Female")
for(i in 1:2) {
  table_1$total_sample[1 + i] <- n_calculator(raw_sel, group_label[i], 1)
}

# Age
group_label <- c("age_18_34", "age_35_49", "age_50_64", "age_65_74", "age_75_plus")
for(i in 1:5) {
  table_1$total_sample[4 + i] <- n_calculator(raw_sel, group_label[i], 1)
}

# Race
group_label <- c("NH_white", "NH_black", "hispanic", "other_race")
for(i in 1:4) {
  table_1$total_sample[10 + i] <- n_calculator(raw_sel, group_label[i], 1)
}

# Education
group_label <- c("edu_high_school_less", "edu_some_college", "edu_college_grad")
for(i in 1:3) {
  table_1$total_sample[15 + i] <- n_calculator(raw_sel, group_label[i], 1)
}

# Income
group_label <- c("income_35000_less", "income_35000_75000", "income_75000_plus")
for(i in 1:3) {
  table_1$total_sample[19 + i] <- n_calculator(raw_sel, group_label[i], 1)
}

# Cancer History
group_label <- c("Cancer_survivor", "Never_had_cancer")
for(i in 1:2) {
  table_1$total_sample[23 + i] <- n_calculator(raw_sel, group_label[i], 1)
}

# Census Region
group_label <- c("CR_northeast", "CR_midwest", "CR_south", "CR_west")
for(i in 1:4) {
  table_1$total_sample[26 + i] <- n_calculator(raw_sel, group_label[i], 1)
}


# Column ---- Climate Change Will Harm Health
for(i in 1:4) {
  
  # Map i (1-4, 2-3, 3-2, 4-1)
  mapped_i <- ifelse(i == 1, 4,
                     ifelse(i == 2, 3,
                            ifelse(i == 3, 2, 1)))
  
  # Gender
  group_label <- c("Male", "Female")
  for(j in 1:2) {
    table_1[j + 1, i + 2] <- paste(
      n_calculator(raw_sel %>% filter(ClimateChgHarmHealth == mapped_i), group_label[j], 1),
      "(",
      wpc_calculator(raw_sel %>% filter(raw_sel[[group_label[j]]] == 1), "ClimateChgHarmHealth", mapped_i),
      ")"
    )
  }
  
  # Age
  group_label <- c("age_18_34", "age_35_49", "age_50_64", "age_65_74", "age_75_plus")
  for(j in 1:5) {
    table_1[j + 4, i + 2] <- paste(
      n_calculator(raw_sel %>% filter(ClimateChgHarmHealth == mapped_i), group_label[j], 1),
      "(",
      wpc_calculator(raw_sel %>% filter(raw_sel[[group_label[j]]] == 1), "ClimateChgHarmHealth", mapped_i),
      ")"
    )
  }
  
  # Race
  group_label <- c("NH_white", "NH_black", "hispanic", "other_race")
  for(j in 1:4) {
    table_1[j + 10, i + 2] <- paste(
      n_calculator(raw_sel %>% filter(ClimateChgHarmHealth == mapped_i), group_label[j], 1),
      "(",
      wpc_calculator(raw_sel %>% filter(raw_sel[[group_label[j]]] == 1), "ClimateChgHarmHealth", mapped_i),
      ")"
    )
  }
  
  # Education
  group_label <- c("edu_high_school_less", "edu_some_college", "edu_college_grad")
  for(j in 1:3) {
    table_1[j + 15, i + 2] <- paste(
      n_calculator(raw_sel %>% filter(ClimateChgHarmHealth == mapped_i), group_label[j], 1),
      "(",
      wpc_calculator(raw_sel %>% filter(raw_sel[[group_label[j]]] == 1), "ClimateChgHarmHealth", mapped_i),
      ")"
    )
  }
  
  # Income
  group_label <- c("income_35000_less", "income_35000_75000", "income_75000_plus")
  for(j in 1:3) {
    table_1[j + 19, i + 2] <- paste(
      n_calculator(raw_sel %>% filter(ClimateChgHarmHealth == mapped_i), group_label[j], 1),
      "(",
      wpc_calculator(raw_sel %>% filter(raw_sel[[group_label[j]]] == 1), "ClimateChgHarmHealth", mapped_i),
      ")"
    )
  }
  
  # Cancer History
  group_label <- c("Cancer_survivor", "Never_had_cancer")
  for(j in 1:2) {
    table_1[j + 23, i + 2] <- paste(
      n_calculator(raw_sel %>% filter(ClimateChgHarmHealth == mapped_i), group_label[j], 1),
      "(",
      wpc_calculator(raw_sel %>% filter(raw_sel[[group_label[j]]] == 1), "ClimateChgHarmHealth", mapped_i),
      ")"
    )
  }
  
  # Census Region
  group_label <- c("CR_northeast", "CR_midwest", "CR_south", "CR_west")
  for(j in 1:4) {
    table_1[j + 26, i + 2] <- paste(
      n_calculator(raw_sel %>% filter(ClimateChgHarmHealth == mapped_i), group_label[j], 1),
      "(",
      wpc_calculator(raw_sel %>% filter(raw_sel[[group_label[j]]] == 1), "ClimateChgHarmHealth", mapped_i),
      ")"
    )
  }
}




#### Chi-squared tests for categorical variables --------------------------------------------------

pointer <- c(1, 4, 10, 15, 19, 23, 26)
characteristics <- c("BirthGender", "AgeGrpB", "race_group", "edu_group", "income_group", "EverHadCancer", "region_group")

for(i in 1:7) {
  table_1$p_value[pointer[i]] <- round(
    svychisq(
      
      # Formula
      reformulate(termlabels = c(characteristics[i], "ClimateChgHarmHealth"), response = NULL),
      
      # Assign survey design
      design = hints_survey,
      
      # Assign statistics
      statistic = "F"
      
    )$p.value,
    
    # Keep 3 decimal places
    3
  )
}




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
                 color = "black"
               )
      ),
      as_chunk("Demographics of sample who answered climate change question on HINTS 6")
    ),
    align_with_table = FALSE
  ) %>%
  
  # Set line spacing
  line_spacing(part = "header", space = 1.5) %>%
  
  # Set all fonts to 'Calibri'
  font(fontname = "TimesNewRoman", part = "all") %>%
  
  # Set p-value to bold
  bold(i = c(1, 4, 10, 15, 26), j = 7, part = "body") %>%
  
  # Modify alignment
  align(align = "center", i = 1, j = 2:7, part = "header") %>%
  align(align = "center", i = 1:30, j = 2:7, part = "body") %>%
  
  # Set paddings
  padding(
    i = c(2, 3, 5:9, 11:14, 16:18, 20:22, 24, 25, 27:30),
    j = 1,
    padding.left = 20,
    part = "body"
  ) %>%
  
  # Auto fit column width
  autofit()




#### Display table 1  --------------------------------------------------

flextable_1
