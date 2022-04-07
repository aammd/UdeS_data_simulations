
# fake dataset
xs <- runif(200, min = -1, max = 1)
true_inter <- -1.3
true_slope <- 3.1
ybar <- true_inter + true_slope*xs
ys <- rnorm(200, ybar, sd = 1)


model <- lm(ys~xs)

# predictions

coef_sterr <- summary(model)$coefficients
inters <- rnorm(500, mean = coef_sterr[1,1], sd = coef_sterr[1,2])
slopes <- rnorm(500, mean = coef_sterr[2,1], sd = coef_sterr[2,2])

newx <- seq(from = -1, to= 1, length.out = 20)
avg_predictions <- cbind(1, newx) %*% rbind(inters, slopes)

conf_predictions <- apply(avg_predictions, 1, quantile, probs = c(.25, .975), simplify = TRUE)

# from confint
conf_interval <- predict(model, newdata=data.frame(xs = newx),
                         interval="confidence",
                         level = 0.95)

# plot to visualize
plot(ys~xs)


# averages are exactly the same
abline(model)
abline(a = coef(model)[1], b = coef(model)[2], col = "red")

# from predict, using parameter covariance
matlines(newx, conf_interval[,2:3], col = "blue", lty=1, lwd = 3)
# from simulated lines, ignoring parameter covariance
matlines(newx, t(conf_predictions), col = "orange", lty = 1, lwd = 2)
