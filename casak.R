n <- 100
g <- runif(n, min = -3, max = 3)
# u <- runif(n, min = -3, max = 3)

u <- rbinom(n, 1, prob = .5)

g_on_p <- 3
u_on_p <- 4
u_on_c <- 1.4
g_on_c <- 1
p_on_c <- 0

p <- rnorm(n, mean = g_on_p*g + u_on_p*u, sd = 1)

plot(g, p)
lm(p~g)

plot(u, p)
lm(p~u+g)

c_var <- rnorm(n, mean = g_on_c*g + u_on_c*u + p_on_c*p, sd = 1)
c_var

plot(p, c_var)

summary(lm(c_var ~ g + p))


library(tidyverse)
tibble(g, u, p, c_var) |>
  ggplot(aes(x = g, y = c_var, fill = as.factor(u))) + geom_point(pch = 21)

