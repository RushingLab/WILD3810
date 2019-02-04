#' get_N
#'
#'
#' @export

get_N <- function(N0, number.of.years, b0, variance.b, d0, variance.d, K = NULL, theta = 1,
                  fire.prob, fire.b, fire.d, d.slope, seed){

  set.seed(seed)

  N1 <- numeric(length = number.of.years[1])
  N1[1] <- N0

  b1 <- d1 <- d.pred1 <- b.pred1 <- numeric(length = number.of.years[1] - 1)
  K.d <- numeric(length = 2)

  K.d[1] <- plogis(qlogis(d0[1]) + (d.slope * K[1]) ^ (1- theta + 1))
  slope2 <- -(K.d[1] - b0) / (K[1]^theta)

  for(t in 1:(number.of.years[1] - 1)){
    d.pred1[t] <- plogis(qlogis(d0) + (d.slope*N1[t])^(1-theta + 1))
    b.pred1[t] <- max(b0 - (slope2 * N1[t] ^ theta), 0)
    b1[t] <- rlnorm(1, meanlog = log(b.pred1[t]), sdlog = sqrt(variance.b))
    d1[t] <- min(rlnorm(1, meanlog = log(d.pred1[t]), sdlog = sqrt(variance.d)), 1)
    if(runif(1) < fire.prob[1]){
      b1[t] <- fire.b
      d1[t] <- fire.d
    }
    suppressWarnings(N1[t + 1] <- N1[t] + rpois(n = 1, b1[t] * N1[t]) - rbinom(n = 1, size = N1[t], prob = d1[t]))
  }

  N2 <- numeric(length = number.of.years[2])

  b2 <- d2 <- d.pred2 <- b.pred2 <- numeric(length = number.of.years[2] - 1)

  K.d[2] <- plogis(qlogis(d0) + (d.slope * K[2]) ^ (1- theta + 1))
  slope2 <- -(K.d[2] - b0) / (K[2]^theta)

  d.pred2[1] <- plogis(qlogis(d0) + (d.slope*N1[number.of.years[1]])^(1-theta + 1))
  b.pred2[1] <- max(b0 - (slope2 * N1[number.of.years[1]] ^ theta), 0.1)
  b2[1] <- rlnorm(1, meanlog = log(b.pred2[1]), sdlog = sqrt(variance.b))
  d2[1] <- min(rlnorm(1, meanlog = log(d.pred2[1]), sdlog = sqrt(variance.d)), 1)
  suppressWarnings(N2[1] <- N1[number.of.years[1]] +
                     rpois(n = 1, b2[1] * N1[number.of.years[1]]) -
                     rbinom(n = 1, size = N1[number.of.years[1]], prob = d2[1]))

  for(t in 2:number.of.years[2]){
    d.pred2[t] <- plogis(qlogis(d0) + (d.slope*N2[t-1])^(1-theta + 1))
    b.pred2[t] <- max(b0 - (slope2 * N2[t -1] ^ theta), 0)
    b2[t] <- rlnorm(1, meanlog = log(b.pred2[t]), sdlog = sqrt(variance.b))
    d2[t] <- min(rlnorm(1, meanlog = log(d.pred2[t]), sdlog = sqrt(variance.d)), 1)
    if(runif(1) < fire.prob[2]){
      b1[t] <- fire.b
      d1[t] <- fire.d
    }
    suppressWarnings(N2[t] <- N2[t-1] + rpois(n = 1, b2[t] * N2[t-1]) - rbinom(n = 1, size = N2[t-1], prob = d2[t]))
  }


  obs_N <- data.frame(Year = seq(from = 2019 - sum(number.of.years), to = 2018),
                      N = c(N1, N2),
                      Time = c(rep("Before", number.of.years[1]), rep("After", number.of.years[2])))

  return(obs_N)
}
