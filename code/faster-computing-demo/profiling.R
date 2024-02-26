rm(list = ls())
graphics.off()

# Alternate version with pre-allocated vector
geom_growth_preallocated <- function(N0 = 2,
                                     T = 999,
                                     lambda = 1.01,
                                     sigma = 0.2){

Nvals <- vector('numeric', length = T + 1) # here's the only change Nvals[1] <- N0
Nvals <- rep(NA, length = T + 1)

for (i in 1:T){
    Nvals[i+1] <- Nvals[i]*(lambda*exp(rnorm(1,0,sigma)))
  }
  return(Nvals)
}


# Function to profile
SimCalc_growth_rates <- function(n) {
    
    N0s <- rlnorm(n)
    Ts <- round(rnorm(n, 50, 10), 0)
    
    data <- mapply(geom_growth_preallocated,
        N0 = N0s,
        T = Ts
    )
    
    growth_rates <- lapply(data, calc_growth_rates)
    mean_growth_rates <- sapply(growth_rates, mean, na.rm = TRUE)
    
    return(mean_growth_rates)
}


# Do profiling
Rprof(line.profiling=TRUE)
out <- SimCalc_growth_rates(1000)
Rprof(NULL)

# View the results
print(summaryRprof())
