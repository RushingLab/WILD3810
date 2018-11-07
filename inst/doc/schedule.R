## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----schedule------------------------------------------------------------
library(kableExtra)
schedule <- data.frame(Week = c("1 (Jan. 7 & 9)", "2 (Jan. 14 & 16)", "3 (Jan. 21 & 23)", "4 (Jan. 28 & 30)",
                                "5 (Feb. 4 & 6)", "6 (Feb. 11 & 13)", "7 (Feb. 18 & 20)", "8 (Feb. 25 & 27)",
                                "9 (Mar. 4 & 6)", "10 (Mar. 11 & 13)", "11 (Mar. 18 & 20)", "12 (Mar. 25 & 27)",
                                "13 (Apr. 1 & 3)", "14 (Apr. 8 & 10)", "15 (Apr. 15 & 17)", "16 (Apr. 22 & 24)"),
                       M = c("[Intro to WILD3810](lecture1.html)", NA, "MLK Holiday - no class", NA,
                             NA, NA, "President's Day - no class", NA,
                             NA, "Spring break - no class", NA, NA,
                             NA, NA, NA, NA),
                       W = c("[Abundance](lecture1.html)", NA, NA, NA,
                             NA, NA, NA, NA,
                             NA, "Spring break - no class", NA, NA,
                             NA, NA, NA, "No class"),
                       Lab = c("Introduction to R", NA, "MLK Holiday - no lab", NA,
                             NA, NA, "President's Day - no lab", NA,
                             NA, "Spring break - no lab", NA, NA,
                             NA, NA, NA, NA))

schedule %>%
  kable("html", col.names = c("Week (dates)", "M", "W", "Lab"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE) %>%
  add_header_above(c("", "Lecture" = 2, ""))


