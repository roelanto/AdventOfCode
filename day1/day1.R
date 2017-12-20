docompute <- function(seq, step=1) {
    result <- vector()
    for (i in c(1:(length(seq)))) {
        nextidx <- i + step
        if (nextidx > (length(seq))) {
            nextidx <- nextidx - length(seq)
        } 
        message("comparing pos ", i, " to pos ", nextidx, " values]: ", seq[i], " ", seq[nextidx])
        if (seq[i] == seq[nextidx]) result <- append(result, seq[i])
    }
    result
}

sum(docompute(c(1,1,2,2)))
sum(docompute(c(1,1,1,1)))
sum(docompute(c(1,2,3,4)))
sum(docompute(c(9,1,2,1,2,1,2,9)))

readdata <- function(filename) {
    con=file(filename, open="r")
    str <- readChar(con, nchars=1024*1024)
    close(con)
    return(strsplit(str, c())[[1]])
}    

data <- as.numeric(readdata("~/AdventOfCode/day1/input.txt"))
data <- data[!is.na(data)]
sum(docompute(data))


data <- c(1,2,1,2)
sum(docompute(data, step=length(data)/2))

sum(docompute(c(1,2,2,1), step=2))
sum(docompute(c(1,2,3,4,2,5), step=3))
sum(docompute(c(1,2,3,1,2,3), step=3))
sum(docompute(c(1,2,1,3,1,4,1,5), step=4))
    
sum(docompute(data, step=(length(data)/2)))
