setwd("~/AdventOfCode/day8/")
script <- read.table("inputsample.txt", sep=" ", stringsAsFactors=FALSE)
script <- read.table("input.txt", sep=" ", stringsAsFactors=FALSE)

evaluate <- function(reg, oper, val, regs) {
    message("eval")
    if (!(reg %in% names(regs))) regs[[reg]] <- 0
    instr <- paste0(regs[[reg]], oper, "as.numeric(",val,")")
    message(instr)
    eval(parse(text=instr))
}

maxvals <- vector()
regs <- list()
for (i in c(1:nrow(script))) {
    line <- as.character(script[i,])
    message(line)
    if (!(line[1] %in% names(regs))) regs[[line[1]]] <- 0
    if (evaluate(line[5], line[6], line[7], regs)) {
        message("TRUE")
        ifelse(line[2]=="inc",
               regs[[line[1]]] <- regs[[line[1]]] + as.numeric(line[3]),
               regs[[line[1]]] <- regs[[line[1]]] - as.numeric(line[3]))
    }
    maxvals <- append(maxvals, max(unlist(regs)))
}

max(unlist(regs))
## part 2:
max(maxvals)
    





        
