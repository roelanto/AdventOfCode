setwd("~/AdventOfCode/day17")
buffersize <- function(x) {
   max(1, x-1)
}


dataframe <<- data.frame(pos=rep(0, 10), value=rep(0, 10))

newpos <- function(x, initial=1, stepsize=3) {
    message(x)
    pos <- 1
    if (x > 1) {
        result <- newpos(x-1, initial, stepsize)
        pos <- result[1]
        dataframe[x,1] <<- pos
        dataframe[x,2] <<- x-1
    } else {
        dataframe[x,1] <<- pos
        dataframe[x,2] <<- x-1
##        message("Returning (x==", x,"): at position ", pos, " the value should be ", x-1)
        return(c(pos,(x-1)))
    }
##    message("computed starting position for x==", x-2, " to be ", pos)
    for (i in (1:stepsize)) {
        pos <- 1 + (pos %% buffersize(x))
##        message("step ", i, ": ", pos)
    }
##    message("Returning (x==", x,"): at position ", pos+1, " the value should be ", x-1)
    append(c((1+pos), (x-1)), result)
}
newpos(1) ## expect: 1

newpos(2) ## 2

newpos(3) ## 2

newpos(4) ## 3

newpos(5) ## 3

newpos(6) ## 2
newpos(7) ## 6
newpos(8) ## 3
newpos(9) ## 7



ovalueAtPos <- function(x, res) {
    positionUpdates <- which(res[1,] == x)
    res[,positionUpdates[length(positionUpdates)]][2]
}

valueAfterValue <- function(x, res) {
    valueAtPos(res[,x][1]+1, res)
}

valueBeforeValue <- function(x, res) {
    valueAtPos(res[,x][1]-1, res)
}

printBuffer <- function(x, res) {
    v <- vector(length=dim(res)[2])
    for (i in c(1:dim(res)[2])) {
        pos <- res[1,i]
        ##        browser()
        chunkpos <- c((pos):i)
        chunk <- v[chunkpos]
##        message("pos = ", pos, "shifting range ", chunkpos, " to range ", chunkpos+1)
        
        v[chunkpos+1]<- v[chunkpos]
        v[pos] <- res[2,i]
    }
    v
}

printBuffer(x, res2)
printBuffer(x, res)

res <- sapply(c(1:2018), function(x) {
    c(newpos(x, stepsize=355), x-1)
})

res2 <- sapply(c(1:10), function(x) {
    c(newpos(x), x-1)
})


## the example
m <- matrix(newpos(2018, stepsize=3), nrow=2)
m[,c(ncol(m):1)] <- m[,c(1:ncol(m))]

buffer <- printBuffer(x, m)
buffer[(m[,2018][1]+1)]


## part 1
m <- matrix(newpos(2018, stepsize=355), nrow=2)
m[,c(ncol(m):1)] <- m[,c(1:ncol(m))]

buffer <- printBuffer(x, m)
buffer[(m[,2018][1]+1)]

## part 2
m <- matrix(newpos(710, stepsize=355), nrow=2)
m[,c(ncol(m):1)] <- m[,c(1:ncol(m))]

buffer <- printBuffer(x, m)
buffer[(m[,2018][1]+1)]
