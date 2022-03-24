

### alternatively --- with and without earthworms!
n <- 80
EW <-  round(runif(n, min = 0, max = 250))
# creer des vers de terre absents
EW[sample(1:n, size = floor(n/2))] <- 0
hist(EW)

pH_sans_EW <- 7.5

ph_pour_1ver <- -.01

pH_ver_moy <- pH_sans_EW + ph_pour_1ver*(EW)

plot(x = EW, y = pH_ver_moy)

pH_ver_obs <- rnorm(80, mean = pH_ver_moy, sd = .4)

plot(x = EW, y = pH_ver_obs)

fake_worms <- data.frame(pH_ver_obs, EW)
summary(lm(pH_ver_obs ~ 1 + EW, data = fake_worms))




##  fungal biomass

curve(dgamma(x, 4^2/2^2, 4/2^2), xlim = c(0, 10))

example_gamma <- rgamma(2000, 13^2/2^2, 13/2^2)
mean(example_gamma)
sd(example_gamma)

## effet pH sur fungi

# center pH on the mean
pH_ver_obs - mean(pH_ver_obs)

# what is the fungal biomass at the average pH? mg fungi / g sol
avg_fungi_at_avg_pH <- .1

# higher pH means less fungi
fungi_change_pour_pH <- -.03

## calculate fungi -- direct and indirect effects of worms
effect_of_one_EW_on_fungi <- 0.001 # could be 0

avg_fungi_by_pH <- avg_fungi_at_avg_pH +
  fungi_change_pour_pH*(pH_ver_obs - mean(pH_ver_obs)) +
  effect_of_one_EW_on_fungi * EW


sd_fungi <- .02
fungi_obs <- rgamma(n, avg_fungi_by_pH^2/sd_fungi^2, avg_fungi_by_pH/sd_fungi^2)

plot(pH_ver_obs, fungi_obs)


# summary(glm(fungi_obs ~ I(pH_ver_obs - mean(pH_ver_obs)), family = Gamma))

summary(lm(fungi_obs ~ I(pH_ver_obs - mean(pH_ver_obs))))

## earthworm effects on the fungi

par(mfrow = c(3,1))
plot(EW, fungi_obs)
plot(pH_ver_obs, fungi_obs)
plot(EW, pH_ver_obs)
par(mfrow = c(1,1))


## model with both!

summary(lm(fungi_obs ~ EW + I(pH_ver_obs - mean(pH_ver_obs))))
summary(lm(fungi_obs ~ EW))


