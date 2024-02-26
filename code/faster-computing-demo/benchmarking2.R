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





# Benchmark the base version with microbenchmark
library(microbenchmark)
comp <- microbenchmark(
    TS_009 = {
        geom_growth_base(T = 9)
    },
    TS_099 = {
        geom_growth_base(T = 99)
    },
    TS_999 = {
        geom_growth_base(T = 999)
    }
)

#Plot the results
library(ggplot2)
autoplot(comp)


# Compare the base and pre-allocated versions
comp <- microbenchmark(
    base = {
        geom_growth_base(T = 999)
    },
    preallocated = {
        geom_growth_preallocated(T = 999)
    }
)

par(cex = 2)
boxplot(log(time) ~ expr, data = comp,
        ylab = "Log time (seconds)",
        xlab = "Expression", 
        col = c(2,4))