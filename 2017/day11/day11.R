input <- read.table("~/AdventOfCode/day11/input.txt", sep="Q", stringsAsFactors=FALSE)[,1]

reverse1 <- function(str) {
    if (str == "n") return("s")
    if (str == "ne") return("sw")
    if (str == "nw") return("se")
    if (str == "s") return("n")
    if (str == "se") return("nw")
    if (str == "sw") return("ne")
}

reverse2 <- function(str) {
    if (str == "n") return(c("se", "sw"))
    if (str == "ne") return(c("s", "nw"))
    if (str == "nw") return(c("s", "ne"))
    if (str == "s") return(c("nw", "ne"))
    if (str == "se") return(c("n", "sw"))
    if (str == "sw") return(c("n", "se"))
}

incomponents <- function(str) {
    allchars <- strsplit(str, ",")[[1]]
#    commas <- which(allchars == ",")
#    allchars <- allchars[-commas]
    allchars
}

## not working
testold<- function(components, expected) {
    used<-rep(FALSE, times=length(components))
    score <- 0
    for (i in c(1:length(components))) {
        if (!used[i]) {
            used[i] <- TRUE
            score <- score + 1
            message("increasing score for ", i)
            for (j in c(1:length(components))) {
                if (!used[j]) {
                    if (components[j] %in% reverse1(components[i])) {
                        message("in reverse1, matching component ", j, " to component ", i)
                        score <- score - 1
                        used[j] <- TRUE
                        break;
                    } else if (components[j] %in% reverse2(components[i])) {
                        message("in reverse2")
                        used[j] <- TRUE
                        break;
                    }
                        
                }
            }
        }
    }
    score
}

test <- function(components, expected) {
    g <- sapply(components, function(x) {
        if (x == "n") return(c(0,1,-1))
        if (x == "s") return(-1*(c(0,1,-1)))
        if (x == "ne") return(c(-1,1,0))
        if (x == "sw") return(-1*c(-1,1,0))
        if (x == "nw") return(c(1,0,-1))
        if (x == "se") return(-1*c(1,0,-1))
    })
    sum(abs(apply(g, 1, sum)))/2
    g
}

test2 <- function(components, expected) {
    g <- sapply(components, function(x) {
        if (x == "n") return(c(0,1,-1))
        if (x == "s") return(-1*(c(0,1,-1)))
        if (x == "ne") return(c(-1,1,0))
        if (x == "sw") return(-1*c(-1,1,0))
        if (x == "nw") return(c(1,0,-1))
        if (x == "se") return(-1*c(1,0,-1))
    })
    sums <- vector(length=ncol(g))
    for (i in c(2:ncol(g))) {
        sums[i] <- sum(abs(apply(g[,c(1:i)], 1, sum)))/2
    }
    sums
}


test2(incomponents("ne,ne,ne"), 3)

test(incomponents("ne,ne,sw,sw"), 0)


test(incomponents("ne,ne,s,s"), 2)
test(incomponents("se,sw,se,sw,sw"), 3)

test(incomponents("ne,se,se,s,nw,s,ne,sw,s"), 4)
test_old(incomponents("se,se,nw,se,se,se,se,se"), 5)
test(incomponents("sw,s,s,s,sw,s,s,nw"), 7)

test(incomponents(input), 1)

testold(incomponents("se,se,nw,se,se,se,se,se"), 6)

input <- "s,nw,s,ne"

testold(incomponents(input), 10)
test(incomponents(input), 10)

