# faking data for comparing means

means <- c("control" = 4.5,"treatment" = 9)

gl(2, k = 15, labels = names(means))

library(tidyverse)
means["control"]

fake_data <- tibble(trt = rep(names(means), each = 1000),
       expectation = means[trt],
       observations = rnorm(length(trt), mean = expectation, sd = 3))
fake_data |>
  ggplot(aes(x = trt, y = observations)) +
  geom_point()


model_demo <- lm(observations ~ 0 + trt, data = fake_data)
model.matrix(model_demo)


model_demo_inter <- lm(observations ~ 1 + trt, data = fake_data)
model.matrix(model_demo_inter) %*% c(40, 2)
summary(model_demo_inter)

?contrasts

every_biologist <- c("low", "med", "high")
fake_three_cats <- tibble(trt = rep(every_biologist, each = 15)) |>
  mutate(trt = ordered(trt, levels = every_biologist),
         resp = rnorm(length(trt)))

fake_three_cats

three_mod <- lm(resp ~ trt, data= fake_three_cats)
summary(three_mod)

as.data.frame(model.matrix(three_mod)) |>
  distinct() |>
  mutate(l = 1:3) |>
  pivot_longer(-l, names_to = "nm", values_to = "val") |>
  ggplot(aes(x = l, y = val, col = nm)) +
  geom_line()



