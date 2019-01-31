#' pva
#'
#' Basic population viability analysis simulator
#' 
#' @param N0 Initial population size
#' @param number.of.years Number of years to simulate population change
#' @param number.of.populations Number of simulated populations
#' @param mean.r Mean intrinsic population growth rate
#' @param variance.r Magnitude of environmental stochasticity (annual variation in `r`)
#' @param K Optional carrying capacity (if K given, population growth modeled using density-dependent model)
#' @param theta Optional parameter for theta-logistic model to allow non-linear density-dependence (must be >=0)
#' @param Allee Optional strenth of Allee effects  
#' @export

pva <- function(N0, number.of.years, number.of.populations, 
                b0, variance.b = 0, d0, variance.d = 0, K = NULL, Allee = NULL, theta = 1,
                extinction.threshold = 0, d.slope = 0.0075){
  if(theta < 0) stop("theta must be greater than 0")
  if(b0 < 0 ) stop("b0 must be greater than 0")
  if(d0 < 0 | d0 > 1 ) stop("d0 must be between 0 and 1")
  if(b0 < 1 & !is.null(K)) stop("In a density-dependent model, b0 should be greater than 1")
  
 N <- b <- d <- d.pred <- b.pred <- matrix(NA, nrow = number.of.years, ncol = number.of.populations)
 N[1,] <- N0
 
 K.d <- plogis(qlogis(d0) + (d.slope * K) ^ (1- theta + 1))
 slope <- -(K.d - b0) / (K^theta)
 
 if(is.null(K)){
   for(t in 1:(number.of.years - 1)){
     for(i in 1:number.of.populations){
       b[t, i] <- rlnorm(1, meanlog = log(b0), sdlog = sqrt(variance.b))
       d[t, i] <- min(rlnorm(1, meanlog = log(d0), sdlog = sqrt(variance.d)), 1)
       N[t + 1, i] <- N[t, i] + rpois(n = 1, b[t,i] * N[t, i]) - rbinom(n = 1, size = N[t, i], prob = d[t,i])
     }
   }
   v_df <- data.frame(N = rep(c(N),2),
                      value = c(c(b), c(d)),
                      Rate = rep(c("Birth rate", "Death rate"), each = length(N)))
   v_df <- dplyr::filter(v_df, N > 0)
   p <- ggplot() + 
     geom_point(data = v_df, aes(x = N, y = value, color = Rate), alpha = 0.1) +
     geom_segment(aes(x = K, xend = K, y = -Inf, yend = K.d), color = "grey60", linetype = "dashed") +
     geom_segment(aes(x = -Inf, xend = K, y = K.d, yend = K.d), color = "grey60", linetype = "dashed") +
     scale_y_continuous("Per capita rate") +
     scale_x_continuous("N")
 }else{
   for(t in 1:(number.of.years - 1)){
     for(i in 1:number.of.populations){
       d.pred[t, i] <- plogis(qlogis(d0) + (d.slope*N[t,i])^(1-theta + 1))
       b.pred[t, i] <- max(b0 - (slope * N[t,i]) ^ theta, 0)
       b[t, i] <- rlnorm(1, meanlog = log(b.pred[t, i]), sdlog = sqrt(variance.b))
       d[t, i] <- min(rlnorm(1, meanlog = log(d.pred[t, i]), sdlog = sqrt(variance.d)), 1)
       N[t + 1, i] <- N[t, i] + rpois(n = 1, b[t, i] * N[t, i]) - rbinom(n = 1, size = N[t, i], prob = d[t, i])
     }
   }
   v_df <- data.frame(N = rep(c(N),2),
                      pred.value = c(c(b.pred), c(d.pred)),
                      obs.value = c(c(b), c(d)),
                      Rate = rep(c("Birth rate", "Death rate"), each = length(N)))
   v_df <- dplyr::filter(v_df, N > 0)
   p <- ggplot() + 
     geom_point(data = v_df, aes(x = N, y = obs.value, color = Rate), alpha = 0.1) +
     geom_segment(aes(x = K, xend = K, y = -Inf, yend = K.d), color = "grey60", linetype = "dashed") +
     geom_segment(aes(x = -Inf, xend = K, y = K.d, yend = K.d), color = "grey60", linetype = "dashed") +
     geom_line(data = v_df, aes(x = N, y = pred.value, group = Rate), color = "white", size = 2, alpha = 0.75) +
     geom_line(data = v_df, aes(x = N, y = pred.value, color = Rate)) +
     scale_y_continuous("Per capita rate") +
     scale_x_continuous("N")
 }
 
 N_df <- data.frame(Year = rep(1:number.of.years, number.of.populations),
                    N = c(N),
                    Population = as.factor(rep(1:number.of.populations, each = number.of.years)))
 

 q <- ggplot(N_df, aes(x = Year, y = N, group = Population)) + 
   geom_path(alpha = 0.25, color = WILD6900_colors$value[WILD6900_colors$name == "warning"]) +
   geom_hline(yintercept = extinction.threshold, color = "grey60", linetype = "dashed")
 
 print(cowplot::plot_grid(p, q))
 dd <- ifelse(is.null(K), "No", "Yes")
 K <- ifelse(is.null(K), NA, K)
 summary_df <- data.frame(nYears = number.of.years, N0, b0 = b0, variance.b = variance.b, 
                          d0 = d0, variance.d = variance.d, density.dependent = dd, K = K,
                          prob.extinct = mean(N[number.of.years,]<=extinction.threshold))
 return(summary_df)
}

sim2 <- pva(N0 = 100, number.of.years = 100, number.of.populations = 100, 
            b0 = 1.9, variance.b = 0.05, d0 = 0.1, variance.d = 0.5, K = 100, theta = 1.1)
sim2


K.d <- plogis(qlogis(d0) + (0.005 * K) ^ (1- theta + 1))
slope <- -(K.d - b0) / (K^theta)

D <- plogis(qlogis(d0) + (0.005*seq(0,2*K))^(1-theta + 1))
B <- b0 - (slope * seq(0,2*K)) ^ theta

plot(B ~ seq(0, 2*K))
plot(D ~ seq(0, 2*K))

plot(B~D)
