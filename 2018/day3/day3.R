

target <- 26

createspiral <- function(target) {
    seqs <- list()
    seqs[[1]] <- c(1)
    i<-1
    while (!(target %in% unlist(seqs))) {
        seqs[[i+1]] <- seq(from=1+seqs[[i]][length(seqs[[i]])], by=1, length.out=8*i)
        i <- i+1
    }
    return(seqs)
}

midpoints <- function(lst, lstnum) {
    message("length lst: ", length(lst))
    midpoint.r <- left(spiral,lstnum)
    print(midpoint.r)
    message("second midoint: ", which(midpoint.r == lst))
    midpoints <- c( which(midpoint.r == lst),
                   which(lst == midpoint.r) + (length(lst)/4),
                   which(lst == midpoint.r) + 2*(length(lst)/4),
                   which(lst == midpoint.r) + 3*(length(lst)/4))
    midpoints
}


manhattandistance <- function(spiral, val) {
    seq <- spiral[sapply(spiral,function(x) {val %in% x})][[1]]

    midpoints <- seq(from=1, to=length(seq), length.out=4)
    range <- which(unlist(sapply(spiral,function(x) {val %in% x})) == TRUE)[1]

    midpoints <- midpoints(seq, range)
    distances <- sapply(midpoints , function(x, y) {
        which(seq == y) - x
    }, y=val)
    
    shortestdistance <- min(abs(distances))
    pathlength <- shortestdistance  + (range - 1)
    message("midpoints: ", paste(seq[midpoints], " "),  " distances to midpoint: ", abs(distances))
    message("distance to midpoint: ", shortestdistance, " range: ", range)
    pathlength
}

left <- function(spiral, steps, last=TRUE) {
    message("left with steps: ", steps)
    result <- unlist(sapply(c(1:steps), function(x) {
        spiral[[x]][x-1]
    }))
    if (last) {
        return(result[length(result)])
    }
    result
   
}

spiral <- createspiral(1024)

manhattandistance(spiral, 1)
manhattandistance(spiral, 12)
manhattandistance(spiral, 23)

manhattandistance(spiral, 1024)

spiral <- createspiral(368078)
manhattandistance(spiral, 368078)
left(spiral, 3)

## part 2
m <- matrix(c(0,0,0,0,1,0,0,0,0), nrow=3)
m

nb <- function(x, y) {
    list(c(x+1, y), c(x-1, y),
         c(x, y+1), c(x, y-1),
         c(x+1, y+1), c(x-1, y-1),
         c(x+1, y-1), c(x-1, y+1))
}
createpath <- function(begin, steps) {
    result <- vector("list", sum(steps))
    result[[1]] <- begin
    nextstep <- begin
    nextnum <- 2
    for (i in c(1:steps[1])) {
        nextstep <- c(nextstep[1]-1, nextstep[2])
        result[[nextnum]] <- nextstep
        nextnum <- nextnum + 1
    }
    for (i in c(1:steps[2])) {
        nextstep <- c(nextstep[1], nextstep[2]-1)
        result[[nextnum]] <- nextstep
        nextnum <- nextnum + 1
    }
    for (i in c(1:steps[3])) {
        nextstep <- c(nextstep[1]+1, nextstep[2])
        result[[nextnum]] <- nextstep
        nextnum <- nextnum + 1
    }
    for (i in c(1:steps[4])) {
        nextstep <- c(nextstep[1], nextstep[2]+1)
        result[[nextnum]] <- nextstep
        nextnum <- nextnum + 1
    }
    result
}

createsteps <- function(matrixsize) {
    c(1,2,2,3) + (matrixsize-3)
}


matrixsize <- 3
m <- matrix(0, nrow=matrixsize, ncol=matrixsize)
found <- FALSE
while (!found) {
    matrixsize <- matrixsize + 2
    m <- matrix(0, nrow=matrixsize + 2, ncol=matrixsize + 2)
    midpoint <- c(1+((dim(m)[1])-1)/2,1+((dim(m)[1])-1)/2)
    m[midpoint[1], midpoint[2]] <- 1
    for (i in seq(from=3, to=matrixsize, by=2)) {
        steps <- createsteps(i)
        message("steps ", paste(steps, " "))
        midpoint <- c(midpoint[1], midpoint[2]+1)
        message("midpoint ", paste(midpoint, " "))
        positions <- createpath(begin=midpoint, steps=steps)
        message("positions ", paste(positions, " "))
        for (pos in positions) {
            val <- sum(sapply(nb(pos[1],pos[2]), function(x) {
                if (x[1] > 0 && x[1] <= dim(m)[1]) {
                    if (x[2] > 0 && x[2] <= dim(m)[2]) {
                        return(m[x[1],x[2]])
                    }
                }
#                message("discard ", x[1], ", ", x[2])
                return (0)}))
            if (val > 368078) {
                found <- TRUE
                message("FOUND VAL: ", val)
                break
            }
#            message("val ", val, " pos 1 ", pos[1], " ", pos[2])
            m[pos[1],pos[2]] <- val
#            message("na m")
        }
        midpoint <- c(midpoint[1]+1, midpoint[2])
    }
    m
}


p <- createpath(begin=c(2,3), steps=c(1,2,2,3))

1 2 2 3
3 4 4 5
5 6 6 7
7 8 8 9 
