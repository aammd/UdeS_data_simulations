
library(tidyverse)

EW <-  round(runif(80, min = 0, max = 250))

EW_moy <- mean(EW)

pH_moy <- 5.5

ph_pour_1ver <- -.01

pH_ver_moy <- pH_moy + ph_pour_1ver*(EW - EW_moy)

plot(x = EW, y = pH_ver_moy)

pH_ver_obs <- rnorm(80, mean = pH_ver_moy, sd = .1)

plot(x = EW, y = pH_ver_obs)

fake_worms <- data.frame(pH_ver_obs, EW)
summary(lm(pH_ver_obs ~ I(EW - mean(EW)), data = fake_worms))

##  fungal biomass

curve(dgamma(x, 4^2/2^2, 4/2^2), xlim = c(0, 10))

curve(dlnorm(x, 2, 1), xlim = c(0, 20))

known_EW_fungi_direct <- .01
known_moy_fungi <- .3

exp(known_moy_fungi + known_EW_fungi_direct * (EW - EW_moy))
