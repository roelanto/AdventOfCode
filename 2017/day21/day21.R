library(parallel)
library(memoise)

#no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores)
expand <- function(x) {
    res <- vector("list", length=8)
    b<-x
    res[[1]] <- b
    for (i in c(1:3)) {
        b <- rotate(b)
        res[[i+1]] <- b
    }
    b <- x[,c(ncol(x):1)]
    res[[5]] <- b
    i <- i+1
    for (j in c(1:3)) {
        i <- i+1
        b <- rotate(b)
        res[[i+1]] <- b
    }
    return(res)
}



ruletable <- read.delim("~/AdventOfCode/day21/inputarno.txt", sep=' ', header=FALSE, stringsAsFactors=FALSE)

rules <- vector("list", length=nrow(ruletable))
for (i in c(1:nrow(ruletable))) {
    rule <- list()
    pattern <- strsplit(ruletable[i,1], "")[[1]]
    pattern<-pattern[-which(pattern=="/")]
    expansion <- strsplit(ruletable[i,3], "")[[1]]
    expansion<-expansion[-which(expansion=="/")]
    message("pattern: ", pattern)
    message("expansion: ", pattern)
    rule$pattern <- matrix(pattern, nrow=sqrt(length(pattern)), byrow=TRUE)
    rule$expansion <- matrix(expansion, nrow=sqrt(length(expansion)) ,byrow=TRUE)
    rules[[i]] <- rule
}

block <- matrix(strsplit(".#...####", "")[[1]], byrow=TRUE, nrow=3)


blockasstring <- function(m) {
    return(as.character(t(m)))
}

memexpand <- memoise(expand)
memblockasstring <- memoise(blockasstring)
memnrow <- memoise(nrow)

rotate <- function(x) t(apply(x, 2, rev))


applyrules <- function(block, rules) {
    for (rule in rules) {
#        message("Rule: ", rule$pattern)
        expandedpatterns <- memexpand(rule$pattern)
                                        #        message("Expanded patterns ", expandedpatterns)
        numblockrows <- memnrow(block)
        blockstring <- memblockasstring(block)
#        message(expandedpatterns)
        l <- lapply(expandedpatterns, function(pattern, numblockrows) {
#            message("Considering ", pattern, " bs ", blockstring, " m ", memnrow(pattern))
            if (numblockrows == memnrow(pattern)) {
                if (identical(blockstring,memblockasstring(pattern))) {
#                message("Found, expansion: ", rule$expansion)
                return(rule$expansion)
                }
            }
        }, numblockrows=numblockrows)
#        print(l)
        if (length(which(sapply(l, is.null) == FALSE)) < length(l)) {
            l <- l[-which(sapply(l, is.null) == TRUE)]
        }
#        print(l)
        if (length(l) > 0) {
#            message("returning")
            return(l[[1]])
        }
    }
    stop("COULDN'T FIND ANY RULE (this should not happen)")
}


glue <- function(l) {
    if (length(l) == 1) return(l[[1]])
    size <- sqrt(length(l))
    rows <- vector("list", length=size)
    for (row in c(1:size)) {
        for (col in c(1:size)) {
            rows[[row]] <- cbind(rows[[row]], l[[((row-1)*size)+col]])
        }
    }
    res <- NULL
    for (row in rows) {
        res <- rbind(res, row)
    }
    res
}
#one <- list(m1)
#two <- list(m1,m2,m3,m4)
#three <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
#glue(one)
#glue(two)
#glue(three)




process <- function(block) {
    bs <- 3
    if (nrow(block) %% 2 == 0) {
        bs <- 2
    }
    res <- vector("list", length=(nrow(block)/bs)^2)
    i<-1
    for (x in seq(from=1, to=nrow(block), by=bs)) {
        for (y in seq(from=1, to=nrow(block), by=bs)) {
            res[[i]] <- block[c(x:(x+(bs-1))),c(y:(y+(bs-1)))]
            i<-i+1
        }
    }
    return(res)
}


iterations <- function(block, numiters) {
    pblocks <- block
    for (i in c(1:numiters)) {
        message("iteration ", i)
        pblocks <- process(pblocks)
        message("processing ", length(pblocks), " blocks")
        res <- lapply( pblocks, applyrules, rules)
        message("res size: ", length(res))
        pblocks <- glue(res)
#        print(pblocks)
    }
    return(pblocks)
}

#clusterExport(cl, "expand")
#clusterExport(cl, "blockasstring")
#clusterExport(cl, "rotate")
#clusterExport(cl, "memexpand")
#clusterExport(cl, "memblockasstring")
#clusterExport(cl, "memnrow")

## pt 1
fff <- function() {
    pblocks <- iterations(block, 5)
    sum(as.character(pblocks)=="#")
}
# fff()

##pt 2
g <- function() {
    pblocks <- iterations(block, 18)
    sum(as.character(pblocks)=="#")
}
# g()





## res <- lapply(rules, function(rule) {
##     expandedpatterns <- expand(rule$pattern)
##     l <- lapply(expandedpatterns, function(pattern) {
##         if (all(blockasstring(block) == blockasstring(pattern))) {
##             return(rule$expansion)
##         }
##     })
##     l <- l[-which(sapply(l, is.null) == TRUE)]
##     return(l)
## })[[1]]



## pblock <- process(pblock)

## process(block)            
                           
