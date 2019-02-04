#' baseline_pva
#'
#' @param number.of.years Number of years to simulate dynamics beyond the last observed count
#' @param number.of.populations Number simulated populations
#' @param b0 The baseline birth rate
#' @param d0 The baseline death rate
#' @param fire.prob Annual probability that a severe fire occurs
#' @param fire.b Birth rate in severe fire years
#' @param fire.d Death rate in severe fire years
#' @param K Carrying capacity
#' @param exitinction.threshold The abundance threshold used to classify quasi-extinction probability (default is 0)
#' @export

baseline_pva <- function(number.of.years, number.of.populations, b0, d0, variance.b = 0.4, variance.d = 0.9, fire.prob = 0.25, fire.b, fire.d,
                                 K = NULL, theta = 1,
                                 d.slope = 0.0075, extinction.threshold = 0){

  data("obs_N")

  N2 <- matrix(NA, nrow = number.of.years + 1, ncol = number.of.populations)
  N2[1,] <- obs_N$N[nrow(obs_N)]
  b <- d <- d.pred <- b.pred <-matrix(NA, nrow = number.of.years,
                                      ncol = number.of.populations)

  K.d <- plogis(qlogis(d0) + (d.slope * K) ^ (1- theta + 1))
  slope <- -(K.d - b0) / (K^theta)

  for(i in 1:number.of.populations){
    for(t in 2:(number.of.years+1)){
      d.pred[t-1,i] <- plogis(qlogis(d0) + (d.slope*N2[t-1,i])^(1-theta + 1))
      b.pred[t-1,i] <- max(b0 - (slope * N2[t-1,i] ^ theta), 0)
      b[t-1,i] <- rlnorm(1, meanlog = log(b.pred[t-1,i]), sdlog = sqrt(variance.b))
      d[t-1,i] <- min(rlnorm(1, meanlog = log(d.pred[t-1,i]), sdlog = sqrt(variance.d)), 1)
      if(runif(1) < fire.prob){
        b[t-1,i] <- fire.b
        d[t-1,i] <- fire.d
      }
      suppressWarnings(N2[t,i] <- N2[t-1,i] + rpois(n = 1, b[t-1,i] * N2[t-1,i]) -
                         rbinom(n = 1, size = N2[t-1,i], prob = d[t-1,i]))
    }
  }

  baseline_N <- data.frame(Year = rep(seq(from = 2018, to = 2018 + number.of.years), number.of.populations),
                           N = c(N2),
                           Population = as.factor(rep(1:number.of.populations,
                                                      each = number.of.years + 1)))
  baseline_summary <- data.frame(b0 = b0, variance.b = variance.b, d0 = d0, variance.d = variance.d,
                                 K = K,
                                 theta = theta, d.slope = d.slope,
                                 extinction.threshold = extinction.threshold,
                                 number.of.years = number.of.years, number.of.populations=number.of.populations,
                                 fire.b = fire.b, fire.d = fire.d)
  baseline = list(baseline_N = baseline_N, baseline_summary = baseline_summary)
  if(extinction.threshold==0) saveRDS(baseline, "baseline.rds")


  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = baseline_N, ggplot2::aes(x = Year, y = N, group = Population), alpha = 0.05,
                       color = WILD3810_colors$value[WILD3810_colors$name == "warning"], size = 0.8) +
    ggplot2::geom_path(data = obs_N, ggplot2::aes(x = Year, y = N), size = 1.1) +
    ggplot2::scale_y_continuous("Abundance") +
    ggplot2::scale_x_continuous() +
    ggplot2::theme(axis.title = element_text(color = "grey20"), axis.text = element_text(color = "grey30"))
  print(p)

  suppressWarnings(x <- apply(N2, 2, function(x) min(which(x <= extinction.threshold))))
  x[is.infinite(x)] <- NA

  summary_df <- data.frame(Population = 1:number.of.populations,
                           minimum_N = apply(N2, 2, min),
                           Extinct = ifelse(apply(N2, 2, min) <= extinction.threshold, 1, 0),
                           year0 = x)
  return(summary_df)
}


#' alternative_pva
#'
#'
#' @export

alternative_pva <- function(K = NULL, fire.prob = 0.25, tot_cost = 100000, extinction.threshold = NULL){

  if(fire.prob < 0 | fire.prob > 3/8) stop("Fire frequency must be between 0 and 0.375")
  data("obs_N")
  baseline <- readRDS("baseline.rds")

  if(is.null(K)) K <- baseline$baseline_summary$K

  if(is.null(extinction.threshold)) extinction.threshold <- baseline$baseline_summary$extinction.threshold


  K_cost <- (K/baseline$baseline_summary$K - 1)*50000
  fire_cost <- (1 - fire.prob/0.25)*150000

  if(K_cost + fire_cost > tot_cost) stop("You do not have enough funds to implement your management actions. Reduce one or both actions to keep costs within the allowable funds")

  N2 <- matrix(NA, nrow = baseline$baseline_summary$number.of.years + 1, ncol = baseline$baseline_summary$number.of.populations)
  N2[1,] <- obs_N$N[nrow(obs_N)]
  b <- d <- d.pred <- b.pred <-matrix(NA, nrow = baseline$baseline_summary$number.of.years,
                                      ncol = baseline$baseline_summary$number.of.populations)

  K.d <- plogis(qlogis(baseline$baseline_summary$d0) +
                  (baseline$baseline_summary$d.slope * K) ^ (1- baseline$baseline_summary$theta + 1))
  slope <- -(K.d - baseline$baseline_summary$b0) / (K^baseline$baseline_summary$theta)

  for(i in 1:baseline$baseline_summary$number.of.populations){
    for(t in 2:(baseline$baseline_summary$number.of.years+1)){
      d.pred[t-1,i] <- plogis(qlogis(baseline$baseline_summary$d0) +
                                (baseline$baseline_summary$d.slope*N2[t-1,i])^(1-baseline$baseline_summary$theta + 1))
      b.pred[t-1,i] <- max(baseline$baseline_summary$b0 - (slope * N2[t-1,i] ^ baseline$baseline_summary$theta), 0)
      b[t-1,i] <- rlnorm(1, meanlog = log(b.pred[t-1,i]), sdlog = sqrt(baseline$baseline_summary$variance.b))
      d[t-1,i] <- min(rlnorm(1, meanlog = log(d.pred[t-1,i]), sdlog = sqrt(baseline$baseline_summary$variance.d)), 1)
      if(runif(1) < fire.prob){
        b[t-1,i] <- baseline$baseline_summary$fire.b
        d[t-1,i] <- baseline$baseline_summary$fire.d
      }
      suppressWarnings(N2[t,i] <- N2[t-1,i] + rpois(n = 1, b[t-1,i] * N2[t-1,i]) -
                         rbinom(n = 1, size = N2[t-1,i], prob = d[t-1,i]))
    }
  }

  alt_N <- data.frame(Year = rep(seq(from = 2018, to = 2018 + baseline$baseline_summary$number.of.years), baseline$baseline_summary$number.of.populations),
                       N = c(N2),
                       Population = as.factor(rep(1:baseline$baseline_summary$number.of.populations,
                                                  each = baseline$baseline_summary$number.of.years + 1)))
  saveRDS(alt_N, "alt_N.rds")
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = baseline$baseline_N, ggplot2::aes(x = Year, y = N, group = Population), alpha = 0.05,
                       color = WILD3810_colors$value[WILD3810_colors$name == "warning"], size = 0.8) +
    ggplot2::geom_path(data = alt_N, ggplot2::aes(x = Year, y = N, group = Population), alpha = 0.05,
                       color = WILD3810_colors$value[WILD3810_colors$name == "info"], size = 0.8) +
    ggplot2::geom_path(data = obs_N, ggplot2::aes(x = Year, y = N), size = 1.1) +
    ggplot2::scale_y_continuous("Abundance") +
    ggplot2::theme(axis.title = element_text(color = "grey20"), axis.text = element_text(color = "grey30")) +
    ggplot2::geom_segment(aes(x = 2025, xend = 2040, y = 1000, yend = 1000), color = WILD3810_colors$value[WILD3810_colors$name == "warning"], size = 0.8) +
    ggplot2::annotate("text", x = 2032.5, y = 1050, label = "Baseline", hjust = 0.5) +
    ggplot2::geom_segment(aes(x = 2025, xend = 2040, y = 850, yend = 850), color = WILD3810_colors$value[WILD3810_colors$name == "info"], size = 0.8) +
    ggplot2::annotate("text", x = 2032.5, y = 900, label =
                        "Alternative", hjust = 0.5) +
    labs(title = "Predicted population viability",
         subtitle = paste0("K = ", K, ", Fire freq. = ", fire.prob))
  if(extinction.threshold == 0){
    ggplot2::ggsave(p, filename = "alternative_pva.png", width = 6, height = 5, dpi = 300)
  }else{
    ggplot2::ggsave(p, filename = "alternative_monitoring_pva.png", width = 6, height = 5, dpi = 300)
  }

  print(p)

  suppressWarnings(x <- apply(N2, 2, function(x) min(which(x <= extinction.threshold))))
  x[is.infinite(x)] <- NA

  summary_df <- data.frame(Population = 1:baseline$baseline_summary$number.of.populations,
                           minimum_N = apply(N2, 2, min),
                           Extinct = ifelse(apply(N2, 2, min) <= extinction.threshold, 1, 0),
                           year0 = x)
  return(summary_df)
}


#' alternative_variance_pva
#'
#'
#' @export

alternative_variance_pva <- function(variance.b = NULL, variance.d = NULL){

  data("obs_N")
  baseline <- readRDS("baseline.rds")

  if(is.null(variance.b)) variance.b = baseline$baseline_summary$variance.b
  if(is.null(variance.d)) variance.d = baseline$baseline_summary$variance.d

  N2 <- matrix(NA, nrow = baseline$baseline_summary$number.of.years + 1, ncol = number.of.populations)
  N2[1,] <- obs_N$N[nrow(obs_N)]
  b <- d <- d.pred <- b.pred <-matrix(NA, nrow = baseline$baseline_summary$number.of.years,
                                      ncol = number.of.populations)

  K.d <- plogis(qlogis(baseline$baseline_summary$d0) +
                  (baseline$baseline_summary$d.slope * baseline$baseline_summary$K) ^ (1- baseline$baseline_summary$theta + 1))
  slope <- -(K.d - baseline$baseline_summary$b0) / (K^baseline$baseline_summary$theta)

  for(i in 1:number.of.populations){
    for(t in 1:(baseline$baseline_summary$number.of.years-1)){
      d.pred[t,i] <- plogis(qlogis(baseline$baseline_summary$d0) +
                              (baseline$baseline_summary$d.slope*N2[t,i])^(1-baseline$baseline_summary$theta + 1))
      b.pred[t,i] <- max(baseline$baseline_summary$b0 - (slope * N2[t,i] ^ baseline$baseline_summary$theta), 0)
      b[t,i] <- rlnorm(1, meanlog = log(b.pred[t,i]), sdlog = sqrt(variance.b))
      d[t,i] <- min(rlnorm(1, meanlog = log(d.pred[t,i]), sdlog = sqrt(variance.d)), 1)
      suppressWarnings(N2[t+1,i] <- N2[t,i] + rpois(n = 1, b[t,i] * N2[t,i]) -
                         rbinom(n = 1, size = N2[t,i], prob = d[t,i]))
    }
  }

  alt_N <- data.frame(Year = rep(seq(from = 2018, to = 2018 + baseline$baseline_summary$number.of.years), number.of.populations),
                      N = c(N2),
                      Population = as.factor(rep(1:number.of.populations,
                                                 each = baseline$baseline_summary$number.of.years + 1)))
  saveRDS(alt_N, "alt_N.rds")
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = baseline$baseline_N, ggplot2::aes(x = Year, y = N, group = Population), alpha = 0.1,
                       color = WILD3810_colors$value[WILD3810_colors$name == "warning"], size = 0.8) +
    ggplot2::geom_path(data = alt_N, ggplot2::aes(x = Year, y = N, group = Population), alpha = 0.1,
                       color = WILD3810_colors$value[WILD3810_colors$name == "info"], size = 0.8) +
    ggplot2::geom_path(data = obs_N, ggplot2::aes(x = Year, y = N), size = 1.1) +
    ggplot2::scale_y_continuous("Abundance") +
    ggplot2::theme(axis.title = element_text(color = "grey20"), axis.text = element_text(color = "grey30"))
  ggplot2::ggsave(p, filename = "alternative_variance_pva.png", width = 6, height = 5, dpi = 300)
  print(p)

  suppressWarnings(x <- apply(N2, 2, function(x) min(which(x <= baseline$baseline_summary$extinction.threshold))))
  x[is.infinite(x)] <- NA

  summary_df <- data.frame(Population = 1:number.of.populations,
                           minimum_N = apply(N2, 2, min),
                           Extinct = ifelse(apply(N2, 2, min) <= baseline$baseline_summary$extinction.threshold, 1, 0),
                           year0 = x)
  return(summary_df)
}
