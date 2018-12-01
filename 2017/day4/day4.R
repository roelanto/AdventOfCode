setwd("~/AdventOfCode/day4/")

lines <- read.table("input.txt", sep="\t")

sum(unlist(apply(lines, 1, function(line) {
    words <- strsplit(as.character(line), " ")[[1]]
    if (sum(duplicated(words)) > 0) {
        return(FALSE)
    }
    return(TRUE)
})))

## part 2
library(digest)
sum(apply(lines, 1, function(line) {
    words <- strsplit(as.character(line), " ")[[1]]
    digests <- unlist(sapply(words, function(word) {
        digest(strsplit(word, "")[[1]][order(strsplit(word, "")[[1]])])
    }))
    if (sum(duplicated(digests)) > 0) {
        return(FALSE)
    }
    return(TRUE)
}))    
        
    
