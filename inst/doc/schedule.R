## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----schedule------------------------------------------------------------
library(kableExtra)
schedule <- data.frame(Week = seq(1:15),
                       '1' = c("[Intro to WILD3810](lecture1.html)", rep(NA, 14)),
                       '2' = c("[Abundance](lecture1.html)", rep(NA, 14)),
                       Lab = c("Introduction to R", rep(NA, 14)))

schedule %>%
  kable("html", col.names = c("Week", "1", "2", "Lab"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE) %>%
  add_header_above(c("", "Lecture" = 2, ""))


