set.seed(12345)
n.year <- 10000
n.pop <- 100

N <- matrix(NA, nrow = n.pop, ncol = n.year)
N[,1] <- 100

b <- 0.5
d <- 0.5
for(i in 1:n.pop){
  for(t in 2:n.year){
    N[i, t] <- rbinom(1, size = N[i, t-1], prob = d) + rpois(1, N[i, t-1] * b)
  }
}

Nd <- data.frame(t = seq(1:n.year),
                 N = c(t(N)),
                 Population = as.character(rep(1:n.pop, each = n.year)))

p <- ggplot(Nd[Nd$t < 51, ], aes(x = t, y = N, group = Population)) +
       geom_path(aes(alpha = N), size = 1, color = WILD3810_colors$value[WILD3810_colors$name=="primary"]) +
       transition_reveal(id = Population, along = t) +
       enter_appear() +
       exit_disappear() +
       ease_aes('linear') +
       theme(legend.position = 'none')
p
gganimate::anim_save("docs/articles/Lecture6/extinction_50.gif")


p <- ggplot(Nd[Nd$t < 201, ], aes(x = t, y = N, group = Population)) +
  geom_path(aes(alpha = N), size = 1, color = WILD3810_colors$value[WILD3810_colors$name=="primary"]) +
  transition_reveal(id = Population, along = t) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear') +
  theme(legend.position = 'none')
p
gganimate::anim_save("docs/articles/Lecture6/extinction_200.gif")

p <- ggplot(Nd[Nd$t < 501, ], aes(x = t, y = N, group = Population)) +
  geom_path(aes(alpha = N), size = 1, color = WILD3810_colors$value[WILD3810_colors$name=="primary"]) +
  transition_reveal(id = Population, along = t) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear') +
  theme(legend.position = 'none')
p
gganimate::anim_save("docs/articles/Lecture6/extinction_500.gif")

p <- ggplot(Nd, aes(x = t, y = N, group = Population)) +
  geom_path(aes(alpha = N), size = 1, color = WILD3810_colors$value[WILD3810_colors$name=="primary"]) +
  transition_reveal(id = Population, along = t) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear') +
  theme(legend.position = 'none')
p
gganimate::anim_save("docs/articles/Lecture6/extinction_1000.gif")


p <- ggplot(Nd[Nd$t < 51, ], aes(x = t, y = N, group = Population)) +
  geom_path(color = WILD3810_colors$value[WILD3810_colors$name == "warning"], alpha = 0.25) +
  transition_reveal(id = Population, along = t) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear')
p
gganimate::anim_save("docs/articles/Lecture6/extinction_50.gif")



set.seed(1234)
n.year <- 500
n.pop <- 10

N <- matrix(NA, nrow = n.pop, ncol = n.year)
N[,1] <- round(seq(from = 5, to = 500, length.out = n.pop))

b <- 0.5
d <- 0.5
for(i in 1:n.pop){
  for(t in 2:n.year){
    N[i, t] <- round(rbinom(1, size = N[i, t-1], prob = d) + rpois(1, N[i, t-1] * b), 0)
  }
}

Nd <- data.frame(t = seq(1:n.year),
                 N = c(t(N)),
                 Population = as.character(rep(1:n.pop, each = n.year)),
                 init = rep(N[,1], each = n.year))

p <- ggplot(Nd, aes(x = t, y = N, group = Population)) +
  geom_point(aes(alpha = N), size = 3, color = WILD3810_colors$value[WILD3810_colors$name=="primary"]) +
  geom_path(aes(alpha = N), size = 1, color = WILD3810_colors$value[WILD3810_colors$name=="primary"]) +
  geom_segment(aes(xend = 500, yend = N, alpha = N), color = "black", linetype = 2) +
  geom_text(aes(x = 510, label = N, alpha = N), color = "black", hjust = 0, size = 8) +
  scale_x_continuous(limits = c(0, 600)) +
  transition_reveal(id = Population, along = t) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear') +
  theme(legend.position = 'none')
p

gganimate::anim_save("docs/articles/Lecture6/pop_size_anim.gif")

### Demographic stochasticity ----

set.seed(12345)
n.year <- 30
n.pop <- 100

N <- matrix(NA, nrow = n.pop, ncol = n.year)
N[,1] <- 10

b <- 0.5
d <- 0.5
for(i in 1:n.pop){
  for(t in 2:n.year){
    N[i, t] <- rbinom(1, size = N[i, t-1], prob = d) + rpois(1, N[i, t-1] * b)
  }
}

Nd <- data.frame(t = seq(1:n.year),
                 N = c(t(N)),
                 Population = as.character(rep(1:n.pop, each = n.year)))


dem_plot <- ggplot(Nd, aes(x = t, y = N, group = Population)) +
  geom_path(aes(alpha = N), size = 1, color = WILD3810_colors$value[WILD3810_colors$name=="warning"]) +
  transition_reveal(id = Population, along = t) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear') +
  theme(legend.position = 'none')

dem_plot

gganimate::anim_save("docs/articles/Lecture6/dem_plot10.gif")

set.seed(12345)
n.year <- 30
n.pop <- 100

N <- matrix(NA, nrow = n.pop, ncol = n.year)
N[,1] <- 500

b <- 0.5
d <- 0.5
for(i in 1:n.pop){
  for(t in 2:n.year){
    N[i, t] <- rbinom(1, size = N[i, t-1], prob = d) + rpois(1, N[i, t-1] * b)
  }
}

Nd <- data.frame(t = seq(1:n.year),
                 N = c(t(N)),
                 Population = as.character(rep(1:n.pop, each = n.year)))


dem_plot <- ggplot(Nd, aes(x = t, y = N, group = Population)) +
  geom_path(aes(alpha = N), size = 1, color = WILD3810_colors$value[WILD3810_colors$name=="warning"]) +
  transition_reveal(id = Population, along = t) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear') +
  theme(legend.position = 'none')

dem_plot

gganimate::anim_save("docs/articles/Lecture6/dem_plot500.gif")

#### Environmental stochasticity

lambda <- 1.05

Ns <- data.frame(Time = 1:100,
                 N = 500,
                 Population = "No variance")

for(t in 2:100){
  Ns$N[t] <- Ns$N[t-1]*lambda
}

p <- ggplot(data = Ns, aes(x = Time, y = N)) + geom_path(color = WILD3810_colors$value[WILD3810_colors$name=="secondary"], size = 2) +
  transition_reveal(id = Population, along = Time) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear')
p

gganimate::anim_save("docs/articles/Lecture6/no_var.gif")

set.seed(123456)
lambda <- log(rlnorm(100, meanlog = 1.05, sdlog = 0.2))

Ns2 <- data.frame(Time = 1:100,
                  N = 500,
                  Population = "Medium variance")

for(t in 2:100){
  Ns2$N[t] <- Ns2$N[t-1]*lambda[t]
}

Ns <- dplyr::bind_rows(Ns, Ns2)

lam1 <- paste("lambda[1] == ", 1.05)
lam2 <- paste("lambda[2] == ", round(prod(lambda)^(1/100),2))
p <- ggplot(data = Ns, aes(x = Time, y = N, color = Population)) + geom_path(size = 2) +
  scale_color_manual(values = c(WILD3810_colors$value[WILD3810_colors$name=="warning"],
                                WILD3810_colors$value[WILD3810_colors$name=="secondary"])) +
  annotate("text", x = 20, y = 59000, label = lam1, parse = TRUE, size = 8, color = WILD3810_colors$value[WILD3810_colors$name=="secondary"]) +
  annotate("text", x = 20, y = 51000, label = lam2, parse = TRUE, size = 8, color = WILD3810_colors$value[WILD3810_colors$name=="warning"]) +
  transition_reveal(id = Population, along = Time) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear')  +
  guides(color = "none")
p

gganimate::anim_save("docs/articles/Lecture6/med_var.gif")

set.seed(1234567)
lambda2 <- log(rlnorm(100, meanlog = 1.05, sdlog = 0.4))

Ns3 <- data.frame(Time = 1:100,
                  N = 500,
                  Population = "High variance")

for(t in 2:100){
  Ns3$N[t] <- Ns3$N[t-1]*lambda2[t]
}

Ns <- dplyr::bind_rows(Ns, Ns3)

lam3 <- paste("lambda[3] == ", round(prod(lambda2)^(1/100),2))
p <-ggplot(data = Ns, aes(x = Time, y = N, color = Population)) + geom_path(size = 2) +
  scale_color_manual(values = c(WILD3810_colors$value[WILD3810_colors$name=="success"],
                                WILD3810_colors$value[WILD3810_colors$name=="warning"],
                                WILD3810_colors$value[WILD3810_colors$name=="secondary"])) +
  annotate("text", x = 20, y = 59000, label = lam1, parse = TRUE, size = 8, color = WILD3810_colors$value[WILD3810_colors$name=="secondary"]) +
  annotate("text", x = 20, y = 51000, label = lam2, parse = TRUE, size = 8, color = WILD3810_colors$value[WILD3810_colors$name=="warning"]) +
  annotate("text", x = 20, y = 43000, label = lam3, parse = TRUE, size = 8, color = WILD3810_colors$value[WILD3810_colors$name=="success"]) +
  transition_reveal(id = Population, along = Time) +
  enter_appear() +
  exit_disappear() +
  ease_aes('linear') +
  guides(color = "none")

p
gganimate::anim_save("docs/articles/Lecture6/high_var.gif")
