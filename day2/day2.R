m <- read.table("inputsample.txt")

m <- read.table("input.txt")

# part 1
sum(apply(m, 1, max, na.rm=TRUE) - apply(m, 1, min, na.rm=TRUE))

# part 2
sum(unlist(apply(m, 1, function(row) {
    sapply(row, function(val) {
        for (val2 in row) {
            if (val != val2) {
                if (val %% val2 == 0) return(val/val2)
#                if (val2 %% val == 0) return(val2/val)
            }
        }
    })
})))
