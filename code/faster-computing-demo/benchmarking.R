rm(list = ls())
graphics.off()

set.seed(1) # for reproducibility

geom_growth_base <- function(N0 = 2,
                             T = 999,
                             lambda = 1.01,
                             sigma = 0.2) {
    Nvals <- vector("numeric") # initiate a place to put the values

    Nvals[1] <- N0
    for (t in 1:T) {
        Nvals[t + 1] <- Nvals[t] * (lambda * exp(rnorm(1, 0, sigma)))
    }

    return(Nvals)
}

# Run the simulation
out <- geom_growth_base() # Plot the results
plot(0:999,
    out,
    xlab = "Time",
    ylab = "Population size",
    type = "o"
)



# Simplest benchmark
start_time <- Sys.time()
out <- geom_growth_base()
end_time <- Sys.time()
time_elapsed <- end_time - start_time
print(time_elapsed)


# Repeat simplest benchmark with longer time horizon
start_time <- Sys.time()
out <- geom_growth_base(T = 9E5)
end_time <- Sys.time()
end_time - start_time
time_elapsed <- end_time - start_time
print(time_elapsed)


# Benchmark with system.time
system.time(geom_growth_base(T = 9E5))


# Benchmark with microbenchmark
library(microbenchmark)