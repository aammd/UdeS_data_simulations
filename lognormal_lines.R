xs <- seq(.5, 24, length.out = 15)
xs

library(tidyverse)
library(tidybayes)

data_frame(xs) |>
  rowwise() |>
  mutate(ys = list(rlnorm(200, meanlog = xs, sdlog = 2))) |>
  unnest(ys) |>
  ggplot(aes(x = xs, y = ys)) +
  stat_lineribbon() +
  coord_cartesian(ylim = c(0, 1e9))


data_frame(xs) |>
  rowwise() |>
  mutate(ys = list(rlnorm(200, meanlog = log(xs) - .5*log((2^2 + xs^2)/xs^2), sdlog = sqrt(log((2^2 + xs^2)/xs^2))))) |>
  unnest(ys) |>
  ggplot(aes(x = xs, y = ys)) +
  stat_lineribbon()

library(rethinking)

ln_model <- ulam(
  alist(
    y   ~ lognormal( meanlog = log(mu) - .5*log((sigma^2 + mu^2)/mu^2), sdlog = sqrt(log((sigma^2 + mu^2)/mu^2)) ),
    mu <- a * x1 * x2 * x3,
    sigma ~ exponential(1),
    a ~ lognormal(2, .5)
  ), data = list(y = c(1,10),
                 x1 = c(1, 10),
                 x2 = c(1, 10),
                 x3 = c(1, 10)
                 ),
  sample = FALSE
)

stancode(ln_model)


dd <- data.frame(x = c(3,5,7),
                 y = c(45,17,66))


