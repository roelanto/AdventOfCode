library(profvis)
source("day22.R")
inf <- bit(length=(2^31)-1)
samples <- runif(n=5000, min=1, max=((2^30)-1))
profvis({
    res <- simulate(infected, weakened, flagged, dims, 1e4, status, debugoutput=FALSE)
#    g <- simulate(infected, weakened, flagged, dims, 100)
#        f<-dornd(inf, samples)
    })
