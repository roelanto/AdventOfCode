#library(compiler)
#enableJIT(3)

directions <- c("N", "E", "S", "W")
directionsmp <- list(c(-1,0), c(0,-1), c(1,0), c(0,1))
currentdir <- directions[1]

addcol <- function(grid, num) {
    col <- matrix(rep(0, times=num*dim(grid)[1]), ncol=num)
    col
}

addrow <- function(grid, num) {
    row <- matrix(rep(0, times=num*dim(grid)[2]), nrow=num)
    row
}

expandgrid <- function(current) {
    num <- 8
    if (current[1] == 0) {
        grid <<- rbind(addrow(grid, num), grid)
        current <- current + c(num,0)
    } else if (current[1] > dim(grid)[1]) {
        grid <<- rbind(grid, addrow(grid, num))
    }
    if (current[2] == 0) {
        grid <<- cbind(addcol(grid, num), grid)
        current <- current + c(0,num)
    } else if (current[2] > dim(grid)[2]) {
        grid <<- cbind(grid, addcol(grid, num))
    }
    list(current=current)
}

        
updateposition <- function(grid, current, currentdiridx, numinfections) {
    if (grid[current[1], current[2]] == "#") {
        mod <- 1
        newval <- "."
    } else {
        mod <- -1
        newval <- "#"
        numinfections <- numinfections + 1
    }
                                        #    browser()
    grid[current[1], current[2]] <- newval
#    message("Change ", current, " to ", newval)
    currentdiridx <- currentdiridx + mod
#    message("currentdiridx ", currentdiridx)
    currentdir <- directions[1 + (currentdiridx %% 4 )]
#    message("Step to ", directions[1 + (currentdiridx %% 4 )])
    return(list(current=current+directionsmp[[1 + (currentdiridx %% 4 )]], currentdir=currentdir, currentdiridx=currentdiridx, grid=grid, numinfections=numinfections))
}

setcell <- function(grid, x, y, val) {
#    browser()
    grid[x,y] <<- val
    grid
}

updateposition2 <- function(current, currentdiridx, numinfections) {
    if (grid[current[1], current[2]] == 0) {
        newval <- 2
        mod <- -1
    } else {
        if (grid[current[1], current[2]] == 2) {
            numinfections <- numinfections+1
            newval <- 1
            
            mod <- 0
        } else {
            if (grid[current[1], current[2]] == 1) {
                newval <- 3
                mod <- 1
            } else {
                if (grid[current[1], current[2]] == 3) {
                    newval <- 0
                    mod <- 2
                } else {
                    message("NOT FOUND ", grid[current[1], current[2]])
                }
                
            }
        }
    }
    setcell(grid, current[1], current[2], newval)
##    browser()
#    message("Change ", current, " to ", newval)
    currentdiridx <- currentdiridx + mod
#    message("currentdiridx ", currentdiridx)
    currentdir <- directions[1 + (currentdiridx %% 4 )]
#    message("Step to ", directions[1 + (currentdiridx %% 4 )])
    return(list(current=current+directionsmp[[1 + (currentdiridx %% 4 )]], currentdir=currentdir, currentdiridx=currentdiridx, grid=grid, numinfections=numinfections))
}


computePart1 <- function() {

    lines <- read.delim(file="~/AdventOfCode/day22/input.txt", stringsAsFactors=FALSE, sep="Q", header=FALSE)
    grid <- matrix(as.character(unlist(apply(lines, 1, strsplit, ""))), nrow=nrow(lines), byrow=TRUE)
    
                                        # grid <- matrix(strsplit("..##.....", "")[[1]], nrow=3, byrow=TRUE)
    current <- c(1+((dim(grid)[1]-1)/2),1+((dim(grid)[2]-1)/2))
    
    currentdiridx <- 0
    numinfections <- 0
    for (i in c(1:10000)) {
        res <- updateposition(grid, current, currentdiridx, numinfections)
        current <- res[["current"]]
        currentdiridx <- res[["currentdiridx"]]
        numinfections <- res[["numinfections"]]
        grid <- res[["grid"]]
        res <- expandgrid(grid, current)
        grid <- res[["grid"]]
        current <- res[["current"]]
        message(i)
                                        #    print(grid)
                                        #    message(current)
    }
    message("# infections: ", numinfections)
}

## part 2

grid <- matrix(c(0,0,1,1,0,0,0,0,0), nrow=3, byrow=TRUE)

computePart2 <- function() {
    current <- c(1+((dim(grid)[1]-1)/2),1+((dim(grid)[2]-1)/2))
    currentdiridx <- 0
    numinfections <- 0
    numiters <- 1000000
    for (i in c(1:numiters)) {
        res <- updateposition2(current, currentdiridx, numinfections)
#        print(grid)
        current <- res[["current"]]
        currentdiridx <- res[["currentdiridx"]]
        numinfections <- res[["numinfections"]]
        res <- expandgrid(current)
#        grid <- res[["grid"]]
                                        #    print(grid)
        current <- res[["current"]]
        if ( i %% 10000 == 0) message(i, " ", i/numiters)
                                        #    print(grid)
                                        #    message(current)
    }
    message("# infections: ", numinfections)
}


library(digest)
library(BMS)
library(hashFunction)
library(bit)

computeHash <- function(val, algo=c("murmur3.32", "spooky.32")) {
    serialized <- as.character(paste(val, collapse=","))
#    message("serialized ", val, " to serialized ", serialized)
    hashes <- sapply(algo, function(x) {
        if (x == "murmur3.32") 
        {
            return(murmur3.32(serialized))
        } else {
            if (x == "spooky.32") {
                return(spooky.32(serialized))
            } else {
                stop("THIS CANNOT HAPPEN, HASH WAS CALLED WITH nonexisting HASH FUNCTION")
            }
        }
    })
#    message("Hashes pre: ", hashes, " post: ", bitwAnd(hashes,  0xffff))
    return(bitwAnd(hashes,  0xffff))
}


setinfected <- function(x, infected) {
    infected[computeHash(x)] <- TRUE
    infected
}

clearinfected <- function(x, infected) {
    infected[computeHash(x)] <- FALSE
    infected
}

setflagged <- function(x, flagged) {
    flagged[computeHash(x)] <- TRUE
    flagged
}
clearflagged <- function(x, flagged) {
    flagged[computeHash(x)] <- FALSE
    flagged
}

setweakened <- function(x, weakened) {
#    message("Weakened ", x)
    h <- computeHash(x)
#    message("Hashes: ", paste(h, " "))
#    message("Currently: ", weakened[h])
    weakened[h] <- TRUE
#    message("After set: ", weakened[h])
    weakened
}
clearweakened <- function(x, weakened) {
    weakened[computeHash(x)] <- FALSE
    weakened
}



isinfected <- function(x, infected) {
    sum(infected[computeHash(x)])==2
}
isweakened <- function(x, weakened) {
#    message("Isweakened: sum: ", weakened[computeHash(x)])
    sum(weakened[computeHash(x)]) == 2
}
isflagged <- function(x, flagged) {
    sum(flagged[computeHash(x)]) == 2
}

plotgrid <- function(dims, infected, weakened, flagged, algos=c("murmur3.32", "spooky.32"), coords=NA) {
    m <- matrix(rep(c("."), times=dims*dims), ncol=dims)
    for (x in c(1:dims)) {
        for (y in c(1:dims)) {
            if (isinfected(c(x, y), infected)) {
                m[x,y] <- "#"
            } else {
                if (isweakened(c(x, y), weakened)) {
                    m[x,y] <- "W"
                } else {
                    if (isflagged(c(x, y), flagged)) {
                        m[x,y] <- "F"
                    }
                }
            }
        }
    }
    if (!is.na(coords)) {
        m[x,y] <- "X"
    }
    m
}


updateposition3 <- function(current, currentdiridx, numinfections, weakened, infected, flagged) {
    mod <- 0
#    message("current: ", current)
    if (isweakened(current, weakened)) {
#        message("current is weakened")
        infected <- setinfected(current, infected)
        weakened <- clearweakened(current, weakened)
        numinfections <- numinfections + 1
        mod <- 0
    } else {
        if (isinfected(current, weakened)) {
#            message("current is infected")
            mod <- -1
            infected <- clearinfected(current, infected)
            flagged <- setflagged(current, flagged)
        } else {
            if (isflagged(current, weakened)) {
#                message("current is flagged")

                mod <- 2
                flagged <- clearflagged(current, flagged)
            } else {
                mod <- 1
                weakened <- setweakened(current, infected)
            }
            
        }
    }
    currentdiridx <- currentdiridx + mod
#    message("Mod is : ", mod, ifelse(mod == -1, "right", ifelse(mod==1, "left", ifelse(mod==0, "same", "reverse"))))
#    message("New direction: ", currentdiridx)
                
    ##    browser()
#    message("Step from ... ", current)
    newval <- current+directionsmp[[1 + ((currentdiridx-1) %% 4 )]]
#    message("     to   ... ", newval)
#    message("Change ", current, " to ", newval)
    currentdir <- directionsmp[[1 + ((currentdiridx-1) %% 4 )]]
#    message("currentdiridx ", currentdiridx, "(", directions[1 + ((currentdiridx-1) %% 4 )], ")")
    return(list(current=newval, currentdir=currentdir, currentdiridx=currentdiridx, numinfections=numinfections, weakened=weakened, infected=infected, flagged=flagged))
}


algos <- c("murmur3.32", "spooky.32")


simulate <- function(infected, weakened, flagged, dims, numiter) {
    current <- c(1+((dims-1)/2),1+((dims-1)/2))
    currentdiridx <- 1
    numinfections <- 0
    
    for (i in c(1:numiter)) {
        
        res <- updateposition3(current, currentdiridx, numinfections, weakened=weakened, infected=infected, flagged=flagged)
        
        current <- res[["current"]]
        currentdiridx <- res[["currentdiridx"]]
        numinfections <- res[["numinfections"]]
        weakened <- res[["weakened"]]
        flagged <- res[["flagged"]]
        infected <- res[["infected"]]
        if ((i %% 10000) == 0)  message(i / numiter)
    }
    numinfections
}

dims <- 9
bitstringlength <- (2^16)
infected <- bit(length=bitstringlength)
weakened <- bit(length=bitstringlength)
flagged <- bit(length=bitstringlength)
infected[abs(computeHash(c(4,6)))]  <- TRUE
infected[abs(computeHash(c(5,4)))]  <- TRUE

simulate(infected, weakened, flagged, dims, 10000000)

dornd <- function(infected, samples) {
    infected[samples] <- TRUE
}

#for (i in c(1:40)) {
#    weakened[i] <- TRUE
#}
    

##computePart2()

#at iter 0: dir = N, pos = 5,5
#after iter: dir = E, pos = 5,4
#after iter 2: dir = N, pos = 4,4
#after iter 3: dir = E, pos = 4,3
#after iter 4: dir = S, pos = 3,3

#bs1 <- bit(length=(2^16))
#pos <- runif(n=50, min=1, max=length(bs1))
#for (i in pos) {
#    bs1[i] <- TRUE
#}
