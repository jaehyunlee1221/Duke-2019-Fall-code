library(tidyverse)
if(!require(faraway)) install.packages("faraway"); library(faraway)
if(!require(doMC)) install.packages("doMc"); library(doMC)
if(!require(foreach)) install.packages("foreach"); library(foreach)
if(!require(profvis)) install.packages("profvis"); library(profvis)
data("orings")
orings <- orings %>% 
  mutate(damage = ifelse(damage >0, 1, 0),
         no_damage = 1 - damage)


foreach(i = 1:nrow(orings), .combine = "+") %dopar% {
  m <- glm(cbind(damage, no_damage) ~ temp, family = "binomial", 
           data = orings[-i, , drop = FALSE]) 
  y_hat <- round(predict(m, newdata = orings[i, , drop = FALSE], type = "response"))
  y <- orings[i, , drop = FALSE]$damage
}

registerDoMC(4)
foreach(i = 1:dim(orings)[1], .combine = "+") %dopar% {
  m <- glm(cbind(damage, no_damage) ~ temp, family = "binomial", 
           data = orings[-i, , drop = FALSE]) 
  y_hat <- round(predict(m, newdata = orings[i, , drop = FALSE], type = "response"))
  y <- orings[i, , drop = FALSE]$damage
  (abs(y - y_hat)) / dim(orings)[1]
}


##Exercise 2
reps <- 10000
n <- 1000
beta_0 <- 2
beta_1 <- .5
beta_2 <- 3
beta_1_hat_all <- c()
for (s in c(1, 3, 7)) {
  beta_1_hat <- c()
  for (i in 1:reps) {
    X <- cbind(rnorm(n), rnorm(n) ^ 2)
    Y <- beta_0 + beta_1 * X[, 1, drop = FALSE]  + 
      beta_2 * X[, 2, drop = FALSE] + rnorm(n, sd = s)
    m <- lm(Y~X) #lm.fit() much faster
    beta_1_hat <- c(beta_1_hat, coefficients(m)[2])
  }
  beta_1_hat_all <- c(beta_1_hat_all, beta_1_hat)
}
beta_df <- tibble(sd = rep(c(1, 3, 7), each = reps),
                  beta_1_hat = beta_1_hat_all)
beta_df %>% 
  ggplot(aes(x = beta_1_hat_all, fill = factor(sd))) +
  geom_density(alpha = .4) +
  labs(x = expression(hat(beta)[1]), fill = expression(sigma)) +
  theme_bw(base_size = 12)
