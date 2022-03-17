
library(tidyverse)


M_fid <- 500

prop_habituate <- .7

obs_demi <- 2

curve(M_fid * (1 - prop_habituate*x/(obs_demi + x)),
      xlim = c(0, 35),
      ylim = c(0, 1000))

hab_function <- fun(M, p, b) M(1 - p*x)

disp <- 50

nobs <- 15
fake_tamia <- tibble(num_obs = 1:nobs,
       reponse_moy = M_fid * (1 - prop_habituate*num_obs/(obs_demi + num_obs)),
       FID = rgamma(nobs, reponse_moy^2/disp^2, reponse_moy/disp^2))

fake_tamia |>
  ggplot(aes(x = num_obs, y = FID)) + geom_point()



library(brms)

nonlin_formula <- bf(FID ~ M * (1 - p*num_obs/(d + num_obs)),
   M ~ 1,
   p ~ 1,
   d ~ 1,
   nl = TRUE,
   family = "normal"
)

get_prior(nonlin_formula, data = fake_tamia)

nonlin_prior <- c(prior(normal(400, 100), class = "b", nlpar = "M"),
  prior(beta(4,4), class = "b", nlpar = "p", lb = 0, ub = 1),
  prior(normal(5,1), class = "b", nlpar = "d"),
  # prior(gamma(6.25, .25), class = "shape"))
  prior(exponential(.1), class = "sigma"))

nonlin_model <- brm(nonlin_formula,
                    data = fake_tamia,
                    prior = nonlin_prior,
                    backend = "cmdstanr",
                    file = "nonlin_model")

summary(nonlin_model)


# gamma family ------------------------------------------------------------


nonlin_formula <- bf(FID ~ M * (1 - p*num_obs/(d + num_obs)),
   M ~ 1,
   p ~ 1,
   d ~ 1,
   nl = TRUE,
   family = Gamma(link = "identity")
)

get_prior(nonlin_formula, data = fake_tamia)

nonlin_prior <- c(prior(normal(400, 200), class = "b", nlpar = "M"),
  prior(beta(4,4), class = "b", nlpar = "p", lb = 0, ub = 1),
  prior(normal(5,3), class = "b", nlpar = "d"),
  prior(gamma(6.25, .25), class = "shape"))


nonlin_gamma_model <- brm(nonlin_formula,
                    data = fake_tamia,
                    prior = nonlin_prior,
                    backend = "cmdstanr",
                    file = "nonlin_model",
                    file_refit = "on_change")

summary(nonlin_gamma_model)

library(tidybayes)

get_variables(nonlin_gamma_model)

nonlin_gamma_model |>
  spread_draws(b_M_Intercept, b_p_Intercept, b_d_Intercept, regex = TRUE) |>
  pivot_longer(starts_with("b")) |>
  ggplot(aes(x = value))+
  stat_halfeye()+
  facet_wrap(~name, scales = "free") +
  geom_vline(aes(xintercept = value),
             data = tribble(~name, ~value,
                            'b_M_Intercept', M_fid,
                            "b_p_Intercept", prop_habituate,
                            "b_d_Intercept", obs_demi))



# log links ---------------------------------------------------------------



nonlin_formula_lnks <- bf(FID ~ exp(logM) * (1 - p*num_obs/(exp(logd) + num_obs)),
   logM ~ 1,
   p ~ 1,
   logd ~ 1,
   nl = TRUE,
   family = Gamma(link = "identity")
)

get_prior(nonlin_formula_lnks, data = fake_tamia)

nonlin_prior_lnks <- c(prior(normal(6,.5), class = "b", nlpar = "logM"),
  prior(beta(4,4), class = "b", nlpar = "p", lb = 0, ub = 1),
  prior(normal(1.5, .5), class = "b", nlpar = "logd"),
  prior(gamma(6.25, .25), class = "shape"))


nonlin_gamma_model_lnks <- brm(nonlin_formula_lnks,
                    data = fake_tamia,
                    prior = nonlin_prior_lnks,
                    backend = "cmdstanr",
                    file = "nonlin_model",
                    file_refit = "on_change")

summary(nonlin_gamma_model_lnks)

library(tidybayes)

get_variables(nonlin_gamma_model_lnks)

nonlin_gamma_model_lnks |>
  spread_draws(b_logM_Intercept, b_p_Intercept, b_logd_Intercept) |>
  pivot_longer(starts_with("b")) |>
  ggplot(aes(x = value))+
  stat_halfeye()+
  facet_wrap(~name, scales = "free") +
  geom_vline(aes(xintercept = value),
             data = tribble(~name, ~value,
                            'b_logM_Intercept', log(M_fid),
                            "b_p_Intercept", prop_habituate,
                            "b_logd_Intercept", log(obs_demi)))

fake_tamia |>
  add_epred_draws(nonlin_gamma_model_lnks) |>
  ggplot(aes(x = num_obs, y = FID))+
  stat_lineribbon(aes(y = .epred)) +
  geom_point(data = fake_tamia, col = "darkgreen") +
  scale_fill_brewer(palette = "Oranges")


















