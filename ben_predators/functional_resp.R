### fitting different versions of the functional response


library(cmdstanr)
simple_fr_advice <- cmdstan_model(here::here("ben_predators", "simple_type2.stan"),
                                  stanc_options = list("warn-pedantic" = TRUE))



holling_dens <- rep((2:20)^2, each = 8)

binom_attacks <- rbinom(length(holling_dens),
                        prob = .7/(1 + 0.029*.7*holling_dens),
                        size = holling_dens)

datlist <- list(attacks = binom_attacks,
                densities = holling_dens,
                N = length(holling_dens))



simple_samp_advice <- simple_fr_advice$sample(data = datlist, parallel_chains = 4)

simple_samp_advice$summary()

library(tidyverse)
## plot it


# multiple species --------------------------------------------------------



# biomasses of three prey
B1 <- round(runif(30, 0, 50))
B2 <- round(runif(5, 0, 50))
B3 <- round(runif(5, 0, 50))

# consumpation rates for three prey
# bvec <- runif(3, min = 0, max = 1)

bvec <- c(.1, .8, .3)

handle_pred <- .002

library(tidyverse)

communities <- expand_grid(B1, B2, B3)



fake_rates <- communities |>
  rowwise() |>
  mutate(
    # two ways to calculate
    sum_of_all = bvec[1]*B1 + bvec[2]*B2 + bvec[3]*B3,
    # sum_of_all = (bvec %*% c(B1, B2, B3))[,],
    denom = 1 + handle_pred*sum_of_all,
    B1_attack_prob = bvec[1]/denom,
    B2_attack_prob = bvec[2]/denom,
    B3_attack_prob = bvec[3]/denom,
    B1_attacks = rbinom(1, size = B1, prob = B1_attack_prob),
    B2_attacks = rbinom(1, size = B2, prob = B2_attack_prob),
    B3_attacks = rbinom(1, size = B3, prob = B3_attack_prob),
    )



multiple_fr <- cmdstan_model(here::here("ben_predators", "multiple_type2.stan"),
                      stanc_options = list("warn-pedantic" = TRUE))

data_list <- list(N = nrow(fake_rates),
                  R = 3,
                  attacks = as.matrix(fake_rates[,c("B1_attacks", "B2_attacks", "B3_attacks")]),
                  densities = as.matrix(fake_rates[,c("B1", "B2", "B3")]))

multiple_demo <- multiple_fr$sample(data= data_list, parallel_chains = 4)

multiple_demo

bvec

library(tidybayes)

tidy_draws(multiple_demo)

spread_rvars(multiple_demo, a[spp]) |>
  ggplot(aes(y = spp, dist = a)) +
  stat_dist_halfeye()

# draw a curve


# single species -- biomass -----------------------------------------------


