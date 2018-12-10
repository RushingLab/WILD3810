## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----grading, echo=FALSE-------------------------------------------------
library(kableExtra)
grades <- data.frame(Grade = c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "F"),
                       Percent = c("93-100%", "90-92.9%", 
                                   "87-89.9%", "83-86.9%", "80-82.9%", 
                                   "77-79.9%", "73-76.9%", "70-72.9%",
                                   "67-69.9%", "60-66.9%", "59.9% and below"),
                       Points = c("930-1000", "900-929", 
                                   "870-899", "830-869", "800-829", 
                                   "770-799", "730-769", "700-729",
                                   "670-699", "600-669", "0-599"))

grades %>%
  kable("html", col.names = c("GRADE", "% RANGE", "POINT RANGE"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


