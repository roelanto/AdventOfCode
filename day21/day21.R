ruletable <- read.delim("~/AdventOfCode/day21/input.txt", sep=' ', header=FALSE, stringsAsFactors=FALSE)

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
#applypattern <- function(rules, block) {


applyrules <- function(block, rules) {
    for (rule in rules) {
#        message("Rule: ", rule$pattern)
        expandedpatterns <- expand(rule$pattern)
#        message("Expanded patterns ", expandedpatterns)
        l <- lapply(expandedpatterns, function(pattern) {
#            message("COnsidering ", pattern)
            if (nrow(block) == nrow(pattern) && all(blockasstring(block) == blockasstring(pattern))) {
#                message("Found, expansion: ", rule$expansion)
                return(rule$expansion)
            }
        })
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
    message("COULDN'T FIND ANY RULE (this should not happen)")
}


#ctr <- 1
#m1 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m2 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m3 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m4 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m5 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m6 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m7 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m8 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1
## m9 <- matrix(ctr*c(1,2,3,4), nrow=2, byrow=TRUE)
## ctr <- ctr+1



glue <- function(l) {
    if (length(l) == 1) return(l[[1]])
    size <- sqrt(length(l))
    rows <- vector("list", length=size)
    for (row in c(1:size)) {
        for (col in c(1:size)) {
            rows[[row]] <- cbind(rows[[row]], l[[((row-1)*size)+col]])
        }
        print(rows[[row]])
    }
    res <- NULL
    for (row in rows) {
        res <- rbind(res, row)
    }
    res
}
#one <- list(m1)
#two <- list(m1,m2,m3,m4)
#3three <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
#glue(one)
#glue(two)
#glue(three)


iterations <- function(block, numiters) {
    pblocks <- block
    for (i in c(1:numiters)) {
        pblocks <- process(pblocks)
        res <- lapply(pblocks, applyrules, rules)
        pblocks <- glue(res)
    }
    return(pblocks)
}

expand <- function(x) {
    res <- vector("list", length=8)
    b<-x
    res[[1]] <- b
#    message("expand ", x)
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
#    message("expand done")
    return(res)
}

blockasstring <- function(m) {
    return(as.character(t(m)))
}

rotate <- function(x) t(apply(x, 2, rev))

process <- function(block) {
    bs <- 3
    if (nrow(block) %% 2 == 0) {
        bs <- 2
    }
#    message("bs : ", bs)
    res <- vector("list", length=(nrow(block)/bs)^2)
    i<-1
    for (x in seq(from=1, to=nrow(block), by=bs)) {
        for (y in seq(from=1, to=nrow(block), by=bs)) {
            res[[i]] <- block[c(x:(x+(bs-1))),c(y:(y+(bs-1)))]
#            print(res[[i]])
            i<-i+1
        }
    }
    return(res)
}


iterations(block, 2)

pblocks <- block
pblocks <- process(pblocks)
res <- lapply(pblocks, applyrules, rules)
pblocks <- glue(res)
pblocks

pblocks <- process(pblocks)
res <- lapply(pblocks, applyrules, rules)
pblocks <- glue(res)
pblocks

pblocks <- process(pblocks)
res <- lapply(pblocks, applyrules, rules)
pblocks <- glue(res)
pblocks

pblocks <- process(pblocks)
res <- lapply(pblocks, applyrules, rules)
pblocks <- glue(res)
pblocks

pblocks <- process(pblocks)
res <- lapply(pblocks, applyrules, rules)
pblocks <- glue(res)
pblocks

sum(as.character(pblocks)=="#")

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
                           
