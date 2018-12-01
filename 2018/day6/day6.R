    

redistribute<-function(mem) {
    chosen <- which(mem == max(mem))[1]
    blocks <- mem[chosen]
    mem[chosen] <- 0
    for (i in (1+(seq(from=chosen, length.out=blocks) %% length(mem)))) {
        mem[i] <- mem[i] + 1
    }
    mem
}

input <- "0	5	10	0	11	14	13	4	11	8	8	7	1	4	12	11"
mem <- as.numeric(strsplit(input, "\t")[[1]])


res <- vector("list", length=1e4)
for (i in c(1:length(res))) res[[i]] <- i
i<-1
while (sum(duplicated(res)) == 0) {
    mem <- redistribute(mem)
    res[[i]] <- mem
    i<-i+1
}

i-1

## part 2
which(duplicated(res)==TRUE) - which(duplicated(res, fromLast=TRUE)==TRUE)


#}

