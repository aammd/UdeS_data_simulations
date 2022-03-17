# mass loss of soil samples

# its a ratio of two distributions which are normal.

x1 <- rnorm(300, mean = 47, sd = 4)
x2 <- rnorm(300, mean = 36, sd = 5)
hist(x1)
hist(x2)
hist(x1/x2)
hist(log(x1/x2))

# see this wikipedia page https://en.wikipedia.org/wiki/Ratio_distribution#cite_note-D%C3%ADaz-Franc%C3%A9s_Rubio_pp._309%E2%80%93323-11

# could treat it like a ratio distribution
# rather than the log ratio, use the normal with the log link




# and calculate the

