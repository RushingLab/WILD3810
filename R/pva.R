#' pva
#'
#' Basic population viability analysis simulator
#'
#' @param N0 Initial population size
#' @param number.of.years Number of years to simulate population change
#' @param number.of.populations Number of simulated populations
#' @param r0 Either the mean growth rate (density-independent model) or the growth rate at N = 0 (density-dependent model)
#' @param variance.r Magnitude of environmental stochasticity in the growth rate (annual variation in `r`)
#' @param dd.slope Strength of density-dependence on r
#' @param K Optional carrying capacity. If K is given, the model will assume density dependence in both b and d; if no K is given, the model will assume density-independent growth
#' @param theta Theta parameter in the theta-logistic growth model; control shape of density-dependence (default = 1)
#' @param extinction.threshold Threshold abundance used to determine a population "extinct" (default = 0)
#' @export

pva <- function(N0, number.of.years, number.of.populations = 500,
                r0, variance.r = 0, K = NULL, Allee = NULL, theta = 1,
                extinction.threshold = 0){

  if(theta <= 0) stop("theta must be greater than 0")

  N <- matrix(NA, nrow = number.of.years, ncol = number.of.populations)
  N[1,] <- N0

  r.pred <- matrix(NA, nrow = number.of.years - 1, ncol = number.of.populations)

  epsilon <- matrix(rnorm(number.of.populations*number.of.years, 0, sqrt(variance.r)),
                    number.of.years, number.of.populations)

  if(is.null(K)){
      for(t in 2:number.of.years){
        suppressWarnings(N[t, ] <- rpois(number.of.populations, N[t-1, ] * exp(r0 + epsilon[t,])))
      }
    r_df <- data.frame(N = c(N[-1,]),
                       r = c(r0 + epsilon[-1,]))

    r.plot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = r_df, ggplot2::aes(x = N, y = r), alpha = 0.1) +
      ggplot2::geom_hline(yintercept = 0, color = "grey75", linetype = "longdash") +
      ggplot2::geom_segment(aes(x = 0, xend = Inf, y = r0, yend = r0), color = "white", size = 2, alpha = 0.75) +
      ggplot2::geom_segment(aes(x = 0, xend = Inf, y = r0, yend = r0), color = "black") +
      ggplot2::scale_y_continuous("r") +
      ggplot2::scale_x_continuous("N")
  }else{
    for(t in 2:number.of.years){
        r.pred[t-1,] <- r0 * (1 -  (N[t-1,]/K) ^ theta)
        suppressWarnings(N[t,] <- rpois(number.of.populations, N[t-1,] * exp(r.pred[t-1,] + epsilon[t-1,])))
    }
    r_df <- data.frame(N = c(N[-number.of.years,]),
                       r.pred = c(r.pred),
                       r = c(r.pred + epsilon[-number.of.years,]))

    r.plot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = r_df, ggplot2::aes(x = N, y = r), alpha = 0.1) +
      ggplot2::geom_hline(yintercept = 0, color = "grey75", linetype = "longdash") +
      ggplot2::geom_segment(ggplot2::aes(x = K, xend = K, y = -Inf, yend = 0), color = "grey60", linetype = "dashed") +
      ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = 0, yend = 0), color = "grey60", linetype = "dashed") +
      ggplot2::geom_line(data = r_df, ggplot2::aes(x = N, y = r.pred), color = "white", size = 2, alpha = 0.75) +
      ggplot2::geom_line(data = r_df, ggplot2::aes(x = N, y = r.pred)) +
      ggplot2::scale_y_continuous("r") +
      ggplot2::scale_x_continuous("N")
  }

  N_df <- data.frame(Year = rep(1:number.of.years, number.of.populations),
                     N = c(N),
                     Population = as.factor(rep(1:number.of.populations, each = number.of.years)))

  if(is.null(K)){
    q <- ggplot2::ggplot(N_df, ggplot2::aes(x = Year, y = N, group = Population)) +
      ggplot2::geom_path(alpha = 0.1, size = 0.8, color = WILD6900_colors$value[WILD6900_colors$name == "warning"]) +
      ggplot2::geom_hline(yintercept = extinction.threshold, color = "grey60", linetype = "dashed")
  }else{
    q <- ggplot2::ggplot(N_df, ggplot2::aes(x = Year, y = N, group = Population)) +
      ggplot2::geom_path(alpha = 0.1, size = 0.8, color = WILD6900_colors$value[WILD6900_colors$name == "warning"]) +
      ggplot2::geom_hline(yintercept = extinction.threshold, color = "grey60", linetype = "dashed") +
      ggplot2::geom_hline(yintercept = K, color = "grey60")
  }

  ext_df <- data.frame(Prob = apply(N, 1, function(x) mean(x <= extinction.threshold)),
                       Year = seq(1:number.of.years))
  s <- ggplot2::ggplot(ext_df, ggplot2::aes(x = Year, y = Prob)) + geom_path(size = 2) +
    scale_y_continuous("Cumulative probability \nof extinction")

  print(cowplot::plot_grid(r.plot, q, s))
  dd <- ifelse(is.null(K), "No", "Yes")
  K <- ifelse(is.null(K), NA, K)
  suppressWarnings(x <- apply(N, 2, function(x) min(which(x <= extinction.threshold))))
  med_tte <- median(x[!is.infinite(x)])
  summary_df <- data.frame(nYears = number.of.years, N0, r0 = r0, variance.r = variance.r,
                           density.dependent = dd, K = K, theta = theta,
                           prob.extinct = mean(N[number.of.years,]<=extinction.threshold, na.rm = TRUE),
                           median.time.to.ext = med_tte)
  return(summary_df)
}

