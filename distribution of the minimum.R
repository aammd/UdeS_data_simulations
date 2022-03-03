# simulate n random observations from a normal distribution and take the minimum

#'
#' https://stats.stackexchange.com/questions/220/how-is-the-minimum-of-a-set-of-iid-random-variables-distributed
#'
#' tried to calculate the derivative of the minium
#'
rand_normal <- replicate(500, rnorm(5, mean = 15, sd = 3))

min_normal <- apply(rand_normal, 2, min)

plot(ecdf(min_normal))

curve(1 - (1 - pnorm(x, mean = 15, sd = 3))^5, xlim = c(6, 18), add = TRUE, col = "red")


pdf_min <- function(x, n, mean, sd){
  n * (1 - pnorm(x, mean = mean, sd = sd))*(n-1)*dnorm(x, mean = mean, sd = sd)
}

curve(pdf_min(x, 5, 15, 3)*500, xlim = c(0, 40), add = TRUE)

?pnorm

#' https://en.wikipedia.org/wiki/Generalized_extreme_value_distribution

