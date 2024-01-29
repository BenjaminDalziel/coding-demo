# Generate a time series of population abundances
# by sampling from a normal distribution

# Clear the workspace and close all graphics
rm(list = ls())
graphics.off()


# Parameters
mean_abundance <- 1000
noise_level <- 3
ntimepoints <- 150


# Sample population sizes
pop_size <- rnorm(n = ntimepoints,
                  mean = mean_abundance,
                  sd = noise_level)


# Diagnostic plot
par(cex = 3)
plot(pop_size,
     xlab = "Time",
     ylab = "Population size")


# Save data
filename <- "data/popdata.Rdata"
save(pop_size, file = filename)