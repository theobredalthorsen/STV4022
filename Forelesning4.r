library(rosdata)
library(rstanarm)
library(tidyverse)
library(broom)

data <- hibbs

mod <- lm(vote ~ growth, data = data)
tidy(mod)


data$resid <- residuals(mod)
data$fit <- fitted(mod)


ggplot(data=data, aes(x=growth, y =vote)) +
  geom_point() + 
  geom_abline(intercept = 46.2476, slope = 3.06, color = "blue") + 
  geom_segment(aes(xend = growth, y = fit, yend = vote),
               color = "red", alpha = 0.6) +
  theme_minimal()



# Med RSTANARM

  mod1 <- stan_glm(vote ~ growth, data = hibbs, refresh = 0)

  mod1  #MAD-SD tenkes på som standardfeil, mer robust utregning, men lik i de aller fleste situasjoner
  
  # Sigma er standardavviket til residualene, gjennomsnittlig avvik fra linja
  
  #Konstantleddet er ca. 46.3 
    # Dette er gjennomsnittlig stemmandel når veksten er null
  # Estimatet for koeffisienten/stigningstallet er ca. 3
    # Når veksten øker med 1 prosentpoeng øker stemmandelen med 3
  # Observasjonene er i gjennomsnitt 3.9 pp fra regresjonslinja
    # Sigma er alltid positiv (det gir ikke mening å signifikansteste)
  

# Kolonnen MAD SD beskriver usikkerheten i parameterestimatene, likner på standardfeil i tradisjonell regresjonsanalyse
  # Et 95% konfidensintervall vil være ca Median + - 2 x MAD SD

median(bayes_R2(mod1))


mod_lm <- lm(vote ~ growth, data = hibbs)
summary(mod_lm)

stargazer::stargazer(mod_lm, type = "text")


data <- data |> 
  mutate(
    dem_incumbent = c(1,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1),
    dem_vote = (dem_incumbent*vote) + ((1-dem_incumbent)*(100-vote))
  )

mod2 <- stan_glm(data = data, dem_vote ~ growth + dem_incumbent, refresh = 0)
mod2

mod3 <- stan_glm(data = data, dem_vote ~ growth*dem_incumbent, refresh = 0)

mod3
# i mod3 vil effektene kunne sees som betinget i at samspilleddet er 0, altså at den andre faktoren er 0


library(ggeffects)
pred <- ggpredict(mod3, terms = c("growth", "dem_incumbent"))
ggplot() +
  geom_line(data = pred, aes(x = x, y = predicted, color = group)) +
  geom_ribbon(data = pred, aes(x = x, ymin = conf.low,
                               ymax = conf.high,
                               by = group), alpha = 0.2) +
  geom_point(data = data, aes(x = growth, y = dem_vote,
                               color = as.factor(dem_incumbent))) +
  xlab("Vekst i personlig inntekt (X)") +
  ylab("Prosent av stemmer for \n den demokratiske kandidaten (Y)") +
  labs(color = "dem_incumbent") +
  scale_y_continuous(limits = c(30, 70)) +
  theme_minimal()






