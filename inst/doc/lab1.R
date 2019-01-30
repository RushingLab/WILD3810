## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----calc, echo=TRUE, eval = FALSE---------------------------------------
#  2+2

## ----echo = TRUE---------------------------------------------------------
x <- 3

## ----printx, echo=TRUE---------------------------------------------------
new.x <- 25/5
new.x

## ----multx, echo=TRUE----------------------------------------------------
x <- 3
y <- x*4

## ----echo=TRUE, eval = FALSE---------------------------------------------
#  n1 <-
#  
#  n2 <-
#  
#  m2 <-

## ----include = FALSE, echo = TRUE, eval = TRUE---------------------------
N <- n1 * n2 / m2

## ----LP_comment, echo = TRUE---------------------------------------------
n1 <- 44     # Number of individuals captured on first occasion

n2 <- 32     # Number of individuals captured on second occasion
  
m2 <- 15     # Number of previously marked individuals captured on second occasion

## ----product, echo=TRUE--------------------------------------------------
n1 <- 45     # Number of individuals captured on first occasion

n2 <- 32     # Number of individuals captured on second occasion

numerator <- prod(n1, n2)
numerator

## ----help, eval = FALSE, echo = TRUE-------------------------------------
#  ?round

## ----round, eval = TRUE, echo = TRUE-------------------------------------
N <- prod(n1, n2)/m2
N

round(N, 2)

## ----round2, eval = TRUE, echo = TRUE------------------------------------
round(digits = 2, x = N)

## ----echo = TRUE---------------------------------------------------------
x <- c(3,5,2,5)
x

## ----echo = TRUE---------------------------------------------------------
occasions <- c("Occasion1", "Occasion2", "Occasion3")
occasions

## ----echo = TRUE---------------------------------------------------------
length(x)

## ----echo = TRUE---------------------------------------------------------
class(x)
class(occasions)

## ----echo = TRUE---------------------------------------------------------
mixed <- c(1, 2, "3", "4")

## ----echo = TRUE---------------------------------------------------------
y <- c(x, 4,8,3)

## ----iguanas, echo = FALSE-----------------------------------------------
library(kableExtra)
iguanas <- data.frame(Day = c(1,2,3),
                      C = c(155, 175, 131),
                      R = c(0, 109, 116))

iguanas %>%
  kable("html", col.names = c("Day", "Animals Observed", "Animal with marks"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


## ----echo = TRUE---------------------------------------------------------
Ct <- c(155, 175, 131) # Number of captures during each occassion

## ----echo = TRUE---------------------------------------------------------
Rt <- c(0, 109, 116) # Number of recaptures during each occassion

## ----echo = TRUE---------------------------------------------------------
Mt <- c(0, 155, 221) # Number of recaptures during each occassion

## ----eval = FALSE--------------------------------------------------------
#  
#  N <- ((155*0) + (175 * 155) + (131 * 221)) / (0 + 109 + 116)
#  

## ----echo = TRUE---------------------------------------------------------
Ct * Mt

## ----echo = TRUE---------------------------------------------------------
N <- sum(Ct * Mt) / sum(Rt)
round(x = N, digits = 0)

