#library(compiler)
                                        #enableJIT(3)
library(memoise)
library(data.table)

directions <- c("N", "E", "S", "W")
directionsmp <- list(c(-1,0), c(0,1), c(1,0), c(0,-1))
currentdir <- directions[1]
numrows <- 50

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
    return(list(current=current+directionsmp[[1 + (currentdiridx %% 4 )]], currentdir=currentdir, currentdiridx=currentdiridx, status=status, numinfections=numinfections))
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
##    message("# infections: ", numinfections)
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


slowComputeHash <- function(val, algo=c("murmur3.32", "spooky.32")) {
    serialized <- as.character(paste(val, collapse=","))
#    message("serialized ", val, " to serialized ", serialized)
    hashes <- c(murmur3.32(serialized), spooky.32(serialized))
#    message("Hashes pre: ", hashes, " post: ", bitwAnd(hashes,  0xffff))
    return(bitwAnd(hashes,  0xffff))
}


computeHash <- slowComputeHash

## na iter 1: 5,4: 1, 5,5: 3, 4,6: 1
## na iter 2: 5,4: 2, 5,5: 3, 4,6: 1
## ma iter 3: 5,4: 2, 5,5: 3, 4,6: 1, 4,4: 3
## ma iter 4: 5,4: 2, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3
## ma iter 5: 5,4: 2, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3, 5,3: 3
## ma iter 6: 5,4: 4, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3, 5,3: 3
## ma iter 7: 5,4: 4, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3, 5,3: 1

## 1 = infected, 2 = flagged, 3 = weakened, 4 = clean

## 1 = weakened, 2 == flagged, 3 == infected 4 == clean
## na iter 1: 5,4: 3, 5,5: 1, 4,6: 3
## na iter 2: 5,4: 2, 5,5: 1, 4,6: 3
## ma iter 3: 5,4: 2, 5,5: 1, 4,6: 3, 4,4: 1
## ma iter 4: 5,4: 2, 5,5: 1, 4,6: 3, 4,4: 1, 4,3: 1
## ma iter 5: 5,4: 2, 5,5: 1, 4,6: 3, 4,4: 1, 4,3: 1, 5,3: 1
## ma iter 6: 5,4: 4, 5,5: 1, 4,6: 3, 4,4: 1, 4,3: 1, 5,3: 1
## ma iter 7: 5,4: 4, 5,5: 1, 4,6: 3, 4,4: 1, 4,3: 1, 5,3: 3



setinfected <- function(x, status) {
    setstatus(x, status, 2)
}

setflagged <- function(x, status) {
    setstatus(x, status, 3)
}

setweakened <- function(x, status) {
    setstatus(x, status, 1)
}

setclean <- function(x, status) {
    setstatus(x, status, 4)
}

createid <- function(pos, status, seqno) {
##    return(serialize(list(x=pos[1], y=pos[2], status=status, seqno=seqno), connection=NULL, ascii=TRUE))
    
    return(sprintf("%i,%i,%s,%i", pos[1], pos[2], status, seqno))
    return(paste(paste(pos, collapse=";"), status, seqno, collapse=","))
}

#createid <- memoise(memcreateid)

lookupstatus <- function(pos, statusdf) {
    thestatus <- statusdf[.(pos[1], pos[2]), 3L]
    if (is.na(thestatus)) {
        return (list(status=4))
    } else {
        return (list(status=thestatus))
    }

}

lookupstatusbitstring <- function(x, status, numrows=50) {
    debugoutput <- FALSE
    id <- createid(pos=x, "W", 1)
    if (debugoutput) message("id of cell ", x, ": ", id)
    hashes <-computeHash(id)
    if (debugoutput) message("hashes: ", hashes)
    if (debugoutput) message("Sum status hashes: ", sum(status[hashes]))
    if (sum(status[hashes]) != 2) {
        if (debugoutput) message("lookupstatus: returning NA")
        return(NA)
    }
    ids <- sapply(c("W", "I", "F", "C"), function(status, pos) {sapply(c(1:numrows), function(seqno, status, pos) {createid(pos=pos, status, seqno)}, status=status, pos=pos)}, pos=x)
    ##browser()
    row <- 1
    col <- 1
    lastrow <- row
    lastcol <- col
    while (row < numrows && sum(status[hashes]) == 2) {
        lastrow <- row
        lastcol <- col
        col <- col + 1
        if (col > 4) {
            col <- 1
            row <- row + 1
        }
        hashes <- computeHash(ids[row,col])
        if (debugoutput) message("for seqno ", row, " and status ", col, " found ", sum(status[hashes]), " hashes")
    }
    if (row >= numrows) {
        stop("MORE THAN ",numrows," ROWS IN THE CHAIN, INCREASE CHAIN")
    }
    if (debugoutput) message("Found status ", lastcol, " at seqno ", lastrow)
    return(list(seqno = lastrow, status=lastcol))
}

setstatus <- function(pos, statusdf, thestatus) {
    if (thestatus == 9) {
        statusdf <- statusdf[! .(pos[1], pos[2])]
        if (is.null(key(statusdf))) setkey(statusdf,x ,y)
    } else {
        numrows <- statusdf[.(pos[1], pos[2]), .N, nomatch=0L]
        if (numrows == 0) {
            rownr <-  which(is.na(statusdf[,x]))[1]
            if (rownr == nrow(statusdf)) {
                stop("Length of status data frame exhausted, increase limit")
            }
            set(statusdf, rownr, 1L, pos[1])
            set(statusdf, rownr, 2L, pos[2])
            set(statusdf, rownr, 3L, thestatus)
            setkey(statusdf, x, y)
        } else {
            statusdf[.(pos[1], pos[2]), status := thestatus]
        }
    }
    statusdf
}

setstatusbitstring <- function(x, status, thestatus) {
    mystatus <- lookupstatus(x, status)
    ps <- c(1, 2, 3, 4)
    if (length(mystatus) < 2 && is.na(mystatus)) {
##        message("setstatus ",x,": unseen yet")
        for (i in c(1:thestatus)) {
            id <- createid(pos=x, c("W","I", "F", "C")[i], 1)
 ##           message(" id ", id)
            hashes <-computeHash(id)
            status[hashes] <- TRUE
        }
    } else {
  ##      message("setstatus ",x," to ", thestatus, ": last known status: seqno = ", mystatus[["seqno"]], ", status: ", mystatus[["status"]])
        id <- createid(pos=x, c("W", "I", "F", "C")[thestatus], ifelse(thestatus != 1, mystatus[["seqno"]], mystatus[["seqno"]]+1))
        hashes <-computeHash(id)
        status[hashes] <- TRUE
    }   
    status
}

##status <- bit(length=bitstringlength)

##status <- setstatus(c(1,2), status, 1)
##status <- setstatus(c(1,2), status, 2)


hasstatus <- function(x, status, whichstatus) {
    thestatus <- lookupstatus(x, status, numrows=numrows)
    if (length(thestatus) > 1 && !is.na(thestatus)) {
        return(thestatus[["status"]] == whichstatus)
    } else {
        ## if na, assume it is clean
        return (whichstatus == 4)
    }
}

isclean <- function(x, status) {
    hasstatus(x, status, 4)
}
isinfected <- function(x, status) {
    hasstatus(x, status, 2)

}
isweakened <- function(x, status) {
    hasstatus(x, status, 1)
}
isflagged <- function(x, status) {
    hasstatus(x, status, 3)

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


updateposition3 <- function(current, currentdiridx, numinfections, status, debugoutput=TRUE) {
    mod <- 0
##    print(paste("Input status: ", status))
    if (debugoutput) message("current: ", current)
                                        #    browser()
    thestatus <- lookupstatus(current, status)
    laststatus <- thestatus[["status"]]
    if (debugoutput) message("Status of cell ", current, ": ", laststatus)
    
    
    if (laststatus == 4) {
        if (debugoutput) message("current is clean")
        mod <- -1
        status <- setweakened(current, status)
    } else {
        if (laststatus == 1) {
            if (debugoutput) message("current is weakened: ", current, " adding an infection")
            status <- setinfected(current, status)
            numinfections <- numinfections + 1
            if (debugoutput) message("numinfections: ", numinfections)
            mod <- 0
        } else {
            if (laststatus == 2) {
                if (debugoutput) message("current is infected")
                mod <- 1
                if (debugoutput) message("flagging current: ", current)
                status <- setflagged(current, status)
            } else {
                if (laststatus == 3) {
                    if (debugoutput) message("current is flagged")
                    status <- setclean(current, status)
                    mod <- 2
                } 
            }
        }
    }
    ##message("After changes to status: ")
    ##print(head(status))
    currentdiridx <- currentdiridx + mod
    if (debugoutput)  {
        message("Mod is : ", mod, ifelse(mod == -1, "right", ifelse(mod==1, "left", ifelse(mod==0, "same", "reverse"))))
        message("New direction: ", currentdiridx)
    }
    
    ##    browser()
    if (debugoutput) message("Step from ... ", current)
    newval <- current+directionsmp[[1 + ((currentdiridx-1) %% 4 )]]
    if (debugoutput) message("     to   ... ", newval)
    if (debugoutput)    message("Change ", current, " to ", newval)
    currentdir <- directionsmp[[1 + ((currentdiridx-1) %% 4 )]]
    if (debugoutput) message("currentdiridx ", currentdiridx, "(", directions[1 + ((currentdiridx-1) %% 4 )], ")")
    return(list(current=newval, currentdir=currentdir, currentdiridx=currentdiridx, numinfections=numinfections, status=status))
}


algos <- c("murmur3.32", "spooky.32")


simulate <- function(infected, weakened, flagged, dims, numiter, status, debugoutput=TRUE) {
    current <- c(1+((dims-1)/2),1+((dims-1)/2))
    currentdiridx <- 1
    numinfections <- 0
    start <- Sys.time()
    totelapsed <- 0
    print("Start simulation")
    stepsize <- 10000
    shouldDelete <- FALSE
    
    for (i in c(1:numiter)) {
        res <- updateposition3(current, currentdiridx, numinfections, status=status, debugoutput=debugoutput)
        if (debugoutput)  {
            message("NA ITER ", i)
            message("current pos: ", res$current)
##            message("current status: ", res$status, " sum: ", sum(res$status))
        }
#        browser()
        
        current <- res[["current"]]
        currentdiridx <- res[["currentdiridx"]]
        if (debugoutput) {
            message("Current dir idx: ", res[["currentdiridx"]], " = ", directions[[1 + ((currentdiridx-1) %% 4 )]], " mod ", directionsmp[[1 + ((currentdiridx-1) %% 4 )]])
            message("Change to dir idx: ", res[["currentdiridx"]])
        }

        numinfections <- res[["numinfections"]]
        status <- res[["status"]]
        if ((i %% (numiter / stepsize)) == 0) {
            if (shouldDelete) {
                orignum <- sum(!is.na(status[,x]))
                setkey(status, "status")
                status <- status[!.(4)]
                setkey(status, x, y)
                nownum <- sum(!is.na(status[,x]))
                message("Removed ", orignum - nownum, " items")
            }
            elapsed <- Sys.time() - start
            totelapsed <- totelapsed + elapsed
            numblockremaining <- (numiter-i)/stepsize
            est <- (totelapsed / (i/10000)) * (1000-(i/1000))
##            estsec <- est %% 60
            ##            estmin <- est / 60
            ##            message("totelpased: ", totelapsed, "i: ", i)
            remest <-  numblockremaining * (totelapsed / (i/stepsize))
            eta <- Sys.time() + remest
            memused <- sum(!is.na(status[,x]))
            
            message(i / numiter, " in ", round(elapsed, 3), "secs (rem est: ", round(remest,2), " seconds, " ,eta,"), mem: ", memused, " (", round(100*(memused/(6*20000)), 2), "%)" )
            start <- Sys.time()
        }
    }
##    print(current)
##    print(status)
    numinfections
}

numrows <- 20000*6
status <- data.table(x=as.numeric(rep(NA, numrows)), y=as.numeric(rep(NA, numrows)), status=as.numeric(rep(NA, numrows)))
setkey(status, x, y)
dims <- 25
##dims <- 9
##status <- setinfected(c(5,4), status)
##status <- setinfected(c(4,6), status)
##status <- setclean(c(5,5), status)
##current <- c(5,5)
currentdiridx <- 0
current <- c(1+((dims-1)/2),1+((dims-1)/2))
currentdiridx <- 1
numinfections <- 0

## na iter 1: pos: 5,4; 5,4: I, 5,5: 3, 4,6: 1, direction: W
## na iter 2: pso: 4,4: 5,4: F, 5,5: 3, 4,6: 1, direciton: N
## ma iter 3: pos: 4,3: 5,4: F, 5,5: 3, 4,6: 1, 4,4: 3, direciton: W
## ma iter 4: pos: 5,3: 5,4: F, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3: direction: S
## ma iter 5: pos: 5,4: 5,4: F, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3, 5,3: 3, direction: E
## ma iter 6: pos: 5,3: 5,4: C, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3, 5,3: 3, direction: W
## ma iter 7: pos: 5,2: 5,4: C, 5,5: 3, 4,6: 1, 4,4: 3, 4,3: 3, 5,3: 1, direction: W

# 2 = infected, 3 = flagged, 1 = weakened, 4 = clean


##res <- simulate(infected, weakened, flagged, dims, 7, status, debugoutput=TRUE)


lines <- read.delim(file="~/AdventOfCode/day22/input.txt", stringsAsFactors=FALSE, sep="Q", header=FALSE)
grid <- matrix(as.character(unlist(apply(lines, 1, strsplit, ""))), nrow=nrow(lines), byrow=TRUE)
for (row in c(1:nrow(grid))) {
    for (col in c(1:ncol(grid))) {
        if (grid[row,col] == "#") {
            status <- setinfected(c(row,col), status)
        }
    }
}

##res <- simulate(infected, weakened, flagged, dims, 100, status, debugoutput=FALSE)

res <- simulate(infected, weakened, flagged, dims, 1e7, status, debugoutput=FALSE)

##res <- simulate(infected, weakened, flagged, dims, 7, status, debugoutput=TRUE)
res

