#' fit_N
#'
#'
#' @export

fit_N <- function(b0, variance.b, d0, variance.d, K = NULL, theta = 1,
                  d.slope){

  data("obs_N")
  N <- matrix(NA, nrow = nrow(obs_N), ncol = 100)
  N[1,] <- obs_N$N[1]

  b <- d <- d.pred <- b.pred <-matrix(NA, nrow = nrow(obs_N) - 1, ncol = 100)


  if(is.null(K)){
    for(t in 1:(nrow(obs_N) - 1)){
      for(i in 1:100){
        b[t, i] <- rlnorm(1, meanlog = log(b0), sdlog = sqrt(variance.b))
        d[t, i] <- min(rlnorm(1, meanlog = log(d0), sdlog = sqrt(variance.d)), 1)
        suppressWarnings(N[t + 1, i] <- N[t, i] + rpois(n = 1, b[t,i] * N[t, i]) - rbinom(n = 1, size = N[t, i], prob = d[t,i]))
      }
    }
  }else{
    K.d <- plogis(qlogis(d0) + (d.slope * K) ^ (1- theta + 1))
    slope <- -(K.d - b0) / (K^theta)

    for(t in 1:(nrow(obs_N) - 1)){
      for(i in 1:100){
        d.pred[t, i] <- plogis(qlogis(d0) + (d.slope*N[t,i])^(1-theta + 1))
        b.pred[t, i] <- max(b0 - (slope * N[t,i] ^ theta), 0)
        b[t, i] <- rlnorm(1, meanlog = log(b.pred[t, i]), sdlog = sqrt(variance.b))
        d[t, i] <- min(rlnorm(1, meanlog = log(d.pred[t, i]), sdlog = sqrt(variance.d)), 1)
        suppressWarnings(N[t + 1, i] <- N[t, i] + rpois(n = 1, b[t, i] * N[t, i]) - rbinom(n = 1, size = N[t, i], prob = d[t, i]))
      }
    }
  }

  N_df <- data.frame(Year = rep(obs_N$Year, 100),
                     N = c(N),
                     Population = as.factor(rep(1:100, each = nrow(obs_N))),
                     Type = c(rep("Simulated", nrow(obs_N)*100)))
  obs_N2 <- obs_N
  obs_N2 <- dplyr::mutate(obs_N, Type = "Observed")

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = N_df, ggplot2::aes(x = Year, y = N, group = Population), alpha = 0.2) +
    ggplot2::geom_path(data = obs_N2, ggplot2::aes(x = Year, y = N),
                       color = WILD3810_colors$value[WILD3810_colors$name=="warning"], size = 2)
  return(p)
}
