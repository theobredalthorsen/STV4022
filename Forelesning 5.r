library(rosdata)
library(tidyverse)
library(ggplot2)
library(rstanarm)

fakedat <- rbinom(30, size = 1, prob = .1)
fakedat
sum(fakedat)


data <- hibbs


###---Simulering av mod1 fra forrige forelesning, sjekker modellen--####

mod1 <- stan_glm(vote ~ growth, data = hibbs, refresh = 0)

alpha <- coef(mod1) [1]
beta <- coef(mod1) [2]


x <- seq(-.5, 4.5, .5)
y_predicted <- alpha + beta * x

plot(x, y_predicted)

sigma <- sigma(mod1)

error <- rnorm(10000, mean = 0, sd = sigma)

hist(error, breaks = 30)

quantile(error, probs = c(0.025, 0.975))

y_pred_with_error <- vector()
x_long <- vector()
n_sim <- 500

for (i in length(x)) {
  error <- rnorm(n_sim, 0, sigma)
  y_pred_i <- alpha + beta + x[i] + error
  y_pred_with_error[i] <- c(y_pred_with_error, y_pred_i[i])
  x_long <- c(x_long, rep(x[i], n_sim))
}

plot(x, y_pred_with_error)

####------ Med posterior_predict()

y_rep_1 <- posterior_predict(mod1, newdata = data.frame(growth = x))
str(y_rep_1)

PI2 <- apply(y_rep_1, 2, function(x) quantile(x, probs =
                                               c(.025, .975)))

graphics::plot(x, y_predicted, type = "l", ylim = c(35,70))
lines(x, PI2[1, ], lty = 3)
lines(x, PI2[2, ], lty = 3)

y_rep2 <- posterior_predict(mod1)

par(mfrow = c(4, 4)) 
for(i in 1:nrow(hibbs)) { 
  hist(y_rep2[, i], main = hibbs$year[i], 
       breaks = seq(0, 100, 2), xlim = c(20, 80)) 
  abline(v = hibbs$vote[i], lwd = 3) 
  }

library(bayesplot)
pp_check(mod1)
