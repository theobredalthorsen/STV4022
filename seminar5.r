library(tidyverse)
library(rosdata)
library(rstanarm)
library(conflicted)
library(ggplot2)
library(tinytable)
conflicts_prefer(dplyr::filter, 
                 dplyr::select,
                 quanteda::stopwords,
                 rosdata::data)

survey_data <- read.csv("phlsurvey_working_data.csv")

survey_data <- survey_data |> 
  filter(treatment_wod == 1 | treatment_iccnocontest == 1)


na_check <- function(x){
 na <-  is.na(x)
 return(summary(na))
}

na_check(survey_data$treatment_iccnocontest)


 sum(survey_data$religion2 == "", na.rm = TRUE)

survey_data <- survey_data |> 
  mutate(religion2 = ifelse(religion2 == "", NA, religion2))


subset1 <- survey_data |> 
  filter(treatment_iccnocontest == 1 )

summary(subset1$age)
table(subset1$gender,subset1$party1)

subset2 <- survey_data |> 
  filter(treatment_iccnocontest == 0 )

summary(subset2$age)
table(subset2$gender,subset2$party1)


mean(subset1$supportwod_bin)

mean(subset2$supportwod_bin)


reg_modell <- stan_glm(supportwod_bin ~ treatment_iccnocontest, data = survey_data, refresh = 0)

print(reg_modell, digits = 3)

reg_modell2 <- stan_glm(supportwod_bin ~ treatment_iccnocontest + age + gender, data = survey_data, refresh = 0)

print(reg_modell2, digits = 3)


set.seed(501)
utfall1 <- rbinom(767, 1, 0.7288136)
utfall2 <- rbinom(756, 1, 0.6878307)

fake <- data.frame(utfall = c(utfall1, utfall2), 
                  treat = c(rep(0, times = 767), c(rep(1, times =756))))


fake_reg <- stan_glm(data = fake, utfall ~ treat, refresh = 0)

print(fake_reg, digits = 3)


utfall1_100 <- list()
utfall2_100 <- list()

for (i in 1:100) {
  utfall1_temp <- rbinom(767, 1, 0.7288136)
  utfall2_temp <- rbinom(756, 1, 0.6878307)
  
  utfall1_100[[i]] <- utfall1_temp
  utfall2_100[[i]] <- utfall2_temp
}

bin <- data.frame(beta = rep(NA, 100),
                  se = rep(NA, 100))

for (i in 1:100) {
  utfall1_temp <- utfall1_100[[i]]
  utfall2_temp <- utfall2_100[[i]]
  
  
  
  fake_100 <- data.frame(
    utfall = c(utfall1_temp, utfall2_temp),
    treat  = c(rep(0, 767), rep(1, 756))
  ) 
  fake_reg_100 <- stan_glm(utfall ~ treat, data = fake_100, refresh = 0)

  bin$beta[i] <- fake_reg_100$coefficients[2]
  bin$se[i] <- fake_reg_100$ses[2]
}

bin$z <- bin$beta/bin$se

sum(abs(bin$z)>1.96)

