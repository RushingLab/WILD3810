## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----lecture_schedule, echo = FALSE--------------------------------------
library(kableExtra)
schedule <- data.frame(Week = c("1 (Jan. 7 & 9)", "2 (Jan. 14 & 16)", "3 (Jan. 21 & 23)", "4 (Jan. 28 & 30)",
                                "5 (Feb. 4 & 6)", "6 (Feb. 11 & 13)", "7 (Feb. 18 & 20)", "8 (Feb. 25 & 27)",
                                "9 (Mar. 4 & 6)", "10 (Mar. 11 & 13)", "11 (Mar. 18 & 20)", "12 (Mar. 25 & 27)",
                                "13 (Apr. 1 & 3)", "14 (Apr. 8 & 10)", "15 (Apr. 15 & 17)", "16 (Apr. 22 & 24)"),
                       M = c("[Intro to WILD3810](lecture1.html)", "Guest lecture - scientific writing", 
                             "MLK Holiday - no class", "Declining populations", 
                              "Dynamics of small populations", "Review Exam 1/Life tables", 
                             "President's Day - no class", "Age-structured dynamics",
                             "", "Spring break - no class", 
                             "Sensitivity and elasticity", "Exam 2",
                             "Metapopulation dynamics", "Occupancy", 
                             "Competition", "Harvest dynamics"),
                       W = c("[Abundance](lecture1.html)", "Density-independent population growth", 
                             "Density-dependent population growth", "Stochastic population dynamics",
                             "Exam 1", "Life-history diversity", 
                             "Survival estimation", "Matrix models",
                             "Buffer", "Spring break - no class", 
                             "Life history strategies", "Review exam 2",
                             "Source-sink dynamics", "Buffer", 
                             "Predator-prey dynamics", "No class"))

schedule %>%
  kable("html", col.names = c("Week (dates)", "M", "W"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE) %>%
  add_header_above(c("", "Lecture" = 2))  %>%
  group_rows("Unit 1: Dynamics of unstructured populations", 1,5, label_row_css = "background-color: #333333; color: #eeeeee;") %>%
  group_rows("Unit 2: Dynamics of structured populations", 6,12, label_row_css = "background-color: #333333; color: #eeeeee;") %>%
  group_rows("Unit 3: Spatial dynamics", 13, 14, label_row_css = "background-color: #333333; color: #eeeeee;") %>%
  group_rows("Unit 4: Species interactions", 15, 16, label_row_css = "background-color: #333333; color: #eeeeee;")


## ----lab_schedule, echo = FALSE------------------------------------------
library(kableExtra)
labs <- data.frame(Week = c("1 (Jan. 7)", "2 (Jan. 14)", "3 (Jan. 21)", "4 (Jan. 28)",
                                "5 (Feb. 4)", "6 (Feb. 11)", "7 (Feb. 18)", "8 (Feb. 25)",
                                "9 (Mar. 4)", "10 (Mar. 11)", "11 (Mar. 18)", "12 (Mar. 25)",
                                "13 (Apr. 1)", "14 (Apr. 8)", "15 (Apr. 15)", "16 (Apr. 22)"),
                   Topic = c("No lab", "Introduction to R/Abundance estimation", "MLK Holiday - no lab", "Population viability analysis",
                             "Density-dependent dynamics", "Harvest management", "President's Day - no lab", "Life tables",
                             "Survival estimation", "Spring break - no lab", "Matrix population projections", "Elasticity analysis and comparison of life history strageties",
                             "Metapopulation models", "Occupancy models", "Compensatory predation", "TBD"))

labs %>%
  kable("html", col.names = c("Week (date)", "Topic"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE) %>%
  group_rows("Unit 1: Dynamics of unstructured populations", 1,6, label_row_css = "background-color: #333333; color: #eeeeee;") %>%
  group_rows("Unit 2: Dynamics of structured populations", 7,12, label_row_css = "background-color: #333333; color: #eeeeee;") %>%
  group_rows("Unit 3: Spatial dynamics", 13, 14, label_row_css = "background-color: #333333; color: #eeeeee;") %>%
  group_rows("Unit 4: Species interactions", 15, 16, label_row_css = "background-color: #333333; color: #eeeeee;")


