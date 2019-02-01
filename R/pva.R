#' pva
#'
#' Basic population viability analysis simulator
#'
#' @param N0 Initial population size
#' @param number.of.years Number of years to simulate population change
#' @param number.of.populations Number of simulated populations
#' @param b0 Either the mean birth rate (density-independent model) or the birth rate at N = 0 (density-dependent model)
#' @param variance.b Magnitude of environmental stochasticity in the birth rate (annual variation in `b`)
#' @param d0 Either the mean death rate (density-independent model) or the death rate at N = 0 (density-dependent model)
#' @param variance.d Magnitude of environmental stochasticity in the death rate (annual variation in `d`)
#' @param d.slope Strength of density-dependence on d
#' @param K Optional carrying capacity. If K is given, the model will assume density dependence in both b and d; if no K is given, the model will assume density-independent growth
#' @param theta Theta parameter in the theta-logistic growth model; control shape of density-dependence (default = 1)
#' @param extinction.threshold Threshold abundance used to determine a population "extinct" (default = 0)
#' @export

pva <- function(N0, number.of.years, number.of.populations = 100,
                b0, variance.b = 0, d0, variance.d = 0, K = NULL, Allee = NULL, theta = 1,
                extinction.threshold = 0, d.slope = 0.0075){

  if(theta < 0) stop("theta must be greater than 0")
  if(b0 < 0 ) stop("b0 must be greater than 0")
  if(d0 < 0 | d0 > 1 ) stop("d0 must be between 0 and 1")
  if(b0 < 1 & !is.null(K)) stop("In a density-dependent model, b0 should be greater than 1")

  N <- matrix(NA, nrow = number.of.years, ncol = number.of.populations)
  N[1,] <- N0

  b <- d <- d.pred <- b.pred <-matrix(NA, nrow = number.of.years - 1, ncol = number.of.populations)


 if(is.null(K)){
   for(t in 1:(number.of.years - 1)){
     for(i in 1:number.of.populations){
       b[t, i] <- rlnorm(1, meanlog = log(b0), sdlog = sqrt(variance.b))
       d[t, i] <- min(rlnorm(1, meanlog = log(d0), sdlog = sqrt(variance.d)), 1)
       suppressWarnings(N[t + 1, i] <- N[t, i] + rpois(n = 1, b[t,i] * N[t, i]) - rbinom(n = 1, size = N[t, i], prob = d[t,i]))
     }
   }
   v_df <- data.frame(N = rep(c(N[-number.of.years,]),3),
                      value = c(c(b), c(d), c(b - d)),
                      Rate = rep(c("Birth rate", "Death rate", "r"), each = length(N[-number.of.years,])))
   v_df <- dplyr::filter(v_df, N > 0)
   p <- ggplot2::ggplot() +
     ggplot2::geom_point(data = v_df[v_df$Rate!="r",], ggplot2::aes(x = N, y = value, color = Rate), alpha = 0.1) +
     ggplot2::geom_segment(aes(x = 0, xend = Inf, y = b0, yend = b0), color = "white", size = 2, alpha = 0.75) +
     ggplot2::geom_segment(aes(x = 0, xend = Inf, y = b0, yend = b0), color = WILD6900_colors$value[WILD6900_colors$name=="primary"]) +
     ggplot2::geom_segment(aes(x = 0, xend = Inf, y = d0, yend = d0), color = "white", size = 2, alpha = 0.75) +
     ggplot2::geom_segment(aes(x = 0, xend = Inf, y = d0, yend = d0), color = WILD6900_colors$value[WILD6900_colors$name=="warning"]) +
     ggplot2::scale_y_continuous("Per capita rate") +
     ggplot2::scale_x_continuous("N")

   r <- ggplot2::ggplot() +
     ggplot2::geom_point(data = v_df[v_df$Rate=="r",], ggplot2::aes(x = N, y = value), alpha = 0.1) +
     ggplot2::geom_hline(yintercept = 0, color = "grey75", linetype = "longdash") +
     ggplot2::geom_segment(aes(x = 0, xend = Inf, y = b0-d0, yend = b0-d0), color = "white", size = 2, alpha = 0.75) +
     ggplot2::geom_segment(aes(x = 0, xend = Inf, y = b0-d0, yend = b0-d0), color = "black") +
     ggplot2::scale_y_continuous("r") +
     ggplot2::scale_x_continuous("N")
 }else{
   K.d <- plogis(qlogis(d0) + (d.slope * K) ^ (1- theta + 1))
   slope <- -(K.d - b0) / (K^theta)

   for(t in 1:(number.of.years - 1)){
     for(i in 1:number.of.populations){
       d.pred[t, i] <- plogis(qlogis(d0) + (d.slope*N[t,i])^(1-theta + 1))
       b.pred[t, i] <- max(b0 - (slope * N[t,i] ^ theta), 0)
       b[t, i] <- rlnorm(1, meanlog = log(b.pred[t, i]), sdlog = sqrt(variance.b))
       d[t, i] <- min(rlnorm(1, meanlog = log(d.pred[t, i]), sdlog = sqrt(variance.d)), 1)
       suppressWarnings(N[t + 1, i] <- N[t, i] + rpois(n = 1, b[t, i] * N[t, i]) - rbinom(n = 1, size = N[t, i], prob = d[t, i]))
     }
   }
   v_df <- data.frame(N = rep(c(N[-number.of.years,]),3),
                      pred.value = c(c(b.pred), c(d.pred), c(b.pred - d.pred)),
                      obs.value = c(c(b), c(d), c(b - d)),
                      Rate = rep(c("Birth rate", "Death rate", "r"), each = length(N[-number.of.years,])))
   v_df <- dplyr::filter(v_df, N > 0)
   p <- ggplot2::ggplot() +
     ggplot2::geom_point(data = v_df[v_df$Rate!="r",], ggplot2::aes(x = N, y = obs.value, color = Rate), alpha = 0.1) +
     ggplot2::geom_segment(ggplot2::aes(x = K, xend = K, y = -Inf, yend = K.d), color = "grey60", linetype = "dashed") +
     ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = K, y = K.d, yend = K.d), color = "grey60", linetype = "dashed") +
     ggplot2::geom_line(data = v_df[v_df$Rate!="r",], ggplot2::aes(x = N, y = pred.value, group = Rate), color = "white", size = 2, alpha = 0.75) +
     ggplot2::geom_line(data = v_df[v_df$Rate!="r",], ggplot2::aes(x = N, y = pred.value, color = Rate)) +
     ggplot2::scale_y_continuous("Per capita rate") +
     ggplot2::scale_x_continuous("N")

   r <- ggplot2::ggplot() +
     ggplot2::geom_point(data = v_df[v_df$Rate=="r",], ggplot2::aes(x = N, y = obs.value), alpha = 0.1) +
     ggplot2::geom_hline(yintercept = 0, color = "grey75", linetype = "longdash") +
     ggplot2::geom_segment(ggplot2::aes(x = K, xend = K, y = -Inf, yend = 0), color = "grey60", linetype = "dashed") +
     ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = 0, yend = 0), color = "grey60", linetype = "dashed") +
     ggplot2::geom_line(data = v_df[v_df$Rate=="r",], ggplot2::aes(x = N, y = pred.value), color = "white", size = 2, alpha = 0.75) +
     ggplot2::geom_line(data = v_df[v_df$Rate=="r",], ggplot2::aes(x = N, y = pred.value)) +
     ggplot2::scale_y_continuous("r") +
     ggplot2::scale_x_continuous("N")
 }

 N_df <- data.frame(Year = rep(1:number.of.years, number.of.populations),
                    N = c(N),
                    Population = as.factor(rep(1:number.of.populations, each = number.of.years)))

 if(is.null(K)){
   q <- ggplot2::ggplot(N_df, ggplot2::aes(x = Year, y = N, group = Population)) +
     ggplot2::geom_path(alpha = 0.25, color = WILD6900_colors$value[WILD6900_colors$name == "warning"]) +
     ggplot2::geom_hline(yintercept = extinction.threshold, color = "grey60", linetype = "dashed")
 }else{
   q <- ggplot2::ggplot(N_df, ggplot2::aes(x = Year, y = N, group = Population)) +
     ggplot2::geom_path(alpha = 0.25, color = WILD6900_colors$value[WILD6900_colors$name == "warning"]) +
     ggplot2::geom_hline(yintercept = extinction.threshold, color = "grey60", linetype = "dashed") +
     ggplot2::geom_hline(yintercept = K, color = "grey60")
 }

 ext_df <- data.frame(Prob = apply(N, 1, function(x) mean(x <= extinction.threshold)),
                      Year = seq(1:number.of.years))
 s <- ggplot2::ggplot(ext_df, ggplot2::aes(x = Year, y = Prob)) + geom_path(size = 2) +
   scale_y_continuous("Cumulative probability \nof extinction")

 print(cowplot::plot_grid(p, r, q, s))
 dd <- ifelse(is.null(K), "No", "Yes")
 K <- ifelse(is.null(K), NA, K)
 suppressWarnings(x <- apply(N, 2, function(x) min(which(x <= extinction.threshold))))
 med_tte <- median(x[!is.infinite(x)])
 summary_df <- data.frame(nYears = number.of.years, N0, b0 = b0, variance.b = variance.b,
                          d0 = d0, variance.d = variance.d, density.dependent = dd, K = K,
                          prob.extinct = mean(N[number.of.years,]<=extinction.threshold, na.rm = TRUE),
                          median.time.to.ext = med_tte)
 return(summary_df)
}

